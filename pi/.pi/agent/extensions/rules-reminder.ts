/**
 * Rules Reminder Extension
 *
 * Models tend to "forget" AGENTS.md rules as the context window fills. The
 * full files are already in the system prompt, so this extension only
 * re-injects a header-only reference (an index) as the LAST message of an
 * LLM call — appended at the end so the prompt-cache prefix stays intact
 * and recency attention is strongest.
 *
 * Triggers (a single pending flag dedupes back-to-back firings):
 *   - Percent-of-context-window: every time `ctx.getContextUsage().percent`
 *     crosses a new `PI_RULES_PERCENT_STEP`-sized bucket. This is the
 *     primary trigger because turn counters do not match the actual
 *     degradation curve (a few large tool results can fill 50% of the
 *     window in one turn).
 *   - Compaction: any compaction (`session_compact`: manual /compact,
 *     threshold-driven auto-compaction such as pi-blackhole, or overflow
 *     recovery). Re-anchors the model on every fresh context.
 *   - Turn interval (optional, off by default): legacy turn-based trigger,
 *     retained for users who prefer it.
 *
 * Configuration (environment variables):
 *   PI_RULES_PERCENT_STEP   percent step size 1..100 (default 10, 0 disables)
 *   PI_RULES_TURN_INTERVAL  turns between reminders (default 0 = off)
 *   PI_RULES_ON_COMPACT     "0" disables compact trigger (default on)
 *   PI_RULES_DISABLED       "1" disables the extension entirely
 *   PI_RULES_INTERVAL       legacy alias for PI_RULES_TURN_INTERVAL
 *
 * Command:
 *   /rules-reminder  -> toggle on/off and show status
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import {
	getAgentDir,
	type ExtensionAPI,
	type ExtensionContext,
	type SessionCompactEvent,
} from "@earendil-works/pi-coding-agent";

const EXTENSION_ID = "rules-reminder";
const DEFAULT_PERCENT_STEP = 10;
const DEFAULT_TURN_INTERVAL = 0;
const DEFAULT_ON_COMPACT = true;

type PendingReason = "interval" | `compact:${SessionCompactEvent["reason"]}` | `percent:${number}`;

interface RuleSource {
	file: string;
	headers: string[];
}

interface RulesReminderConfig {
	percentStep: number;
	turnInterval: number;
	onCompact: boolean;
	disabled: boolean;
}

/**
 * Header-only reference: markdown `#`..`####` at column 0. Requiring
 * column-0 (no leading whitespace) excludes indented `# comment` lines that
 * appear inside fenced code samples.
 */
function extractRefs(content: string): string[] {
	return content.split("\n").filter((line) => /^#{1,4}\s\S/.test(line));
}

// Walk from cwd up to HOME collecting AGENTS.md (closest = most specific),
// then append the global agent-dir file (least specific). Existence is
// checked lazily by `loadRuleSources` via readFileSync, not here.
function findAgentsFiles(startDir: string): string[] {
	const home = path.resolve(os.homedir());
	const stop = path.dirname(home);

	const chain: string[] = [];
	for (let dir = path.resolve(startDir); ; dir = path.dirname(dir)) {
		chain.push(dir);
		if (dir === home || dir === stop || dir === path.parse(dir).root) break;
	}

	const files = new Set<string>();
	for (const dir of chain) files.add(path.join(dir, "AGENTS.md"));
	files.add(path.join(getAgentDir(), "AGENTS.md"));
	return [...files];
}

function loadRuleSources(startDir: string): RuleSource[] {
	const sources: RuleSource[] = [];
	for (const file of findAgentsFiles(startDir)) {
		try {
			const headers = extractRefs(fs.readFileSync(file, "utf8"));
			if (headers.length > 0) sources.push({ file, headers });
		} catch {
			// Unreadable or vanished — skip.
		}
	}
	return sources;
}

function buildReminder(sources: RuleSource[], n: number, reason: PendingReason): string {
	const sections = sources
		.map((s) => {
			const rel = s.file.replace(os.homedir(), "~");
			return `<rules_ref source="${rel}">\n${s.headers.join("\n")}\n</rules_ref>`;
		})
		.join("\n\n");

	return `<rules_reminder n="${n}" reason="${reason}">\n${sections}\n</rules_reminder>`;
}

// NaN-safe env parsing. Bad, missing, or empty values fall back to the
// default instead of silently disabling triggers via NaN comparisons or
// coercing empty strings to 0. Range is inclusive on both ends.
function parseIntegerInRange(value: string | undefined, fallback: number, min: number, max: number): number {
	if (value === undefined || value.trim() === "") return fallback;
	const parsed = Number(value);
	if (!Number.isFinite(parsed)) return fallback;
	return Math.min(Math.max(Math.floor(parsed), min), max);
}

// Truthy when value is undefined (default-on) or any string other than
// "0"/"false"/"no"/"off" (case-insensitive).
function parseBooleanEnv(value: string | undefined, fallback: boolean): boolean {
	if (value === undefined) return fallback;
	const v = value.trim().toLowerCase();
	if (v === "0" || v === "false" || v === "no" || v === "off") return false;
	return true;
}

function loadConfig(): RulesReminderConfig {
	// Legacy: PI_RULES_INTERVAL used to be the only knob. Keep it working as
	// an alias for PI_RULES_TURN_INTERVAL; explicit PI_RULES_TURN_INTERVAL
	// wins when both are set.
	const turnIntervalRaw =
		process.env.PI_RULES_TURN_INTERVAL !== undefined
			? process.env.PI_RULES_TURN_INTERVAL
			: process.env.PI_RULES_INTERVAL;

	return {
		percentStep: parseIntegerInRange(
			process.env.PI_RULES_PERCENT_STEP,
			DEFAULT_PERCENT_STEP,
			0,
			100,
		),
		turnInterval: parseIntegerInRange(turnIntervalRaw, DEFAULT_TURN_INTERVAL, 0, Number.MAX_SAFE_INTEGER),
		onCompact: parseBooleanEnv(process.env.PI_RULES_ON_COMPACT, DEFAULT_ON_COMPACT),
		disabled: process.env.PI_RULES_DISABLED === "1",
	};
}

function describeTriggers(config: RulesReminderConfig): string {
	const parts: string[] = [];
	if (config.percentStep > 0) parts.push(`every ${config.percentStep}%`);
	if (config.turnInterval > 0) parts.push(`every ${config.turnInterval} turns`);
	if (config.onCompact) parts.push("on compact");
	return parts.length > 0 ? parts.join(", ") : "disabled";
}

export default function rulesReminderExtension(pi: ExtensionAPI) {
	const config = loadConfig();

	let sources: RuleSource[] = [];
	let enabled = !config.disabled;
	let reminderCount = 0;
	let turnsSinceLastInjection = 0;
	// Last percent-bucket index that fired a reminder. Reset against
	// fresh usage after a compaction (`bucketResetPending` defers the
	// reseat until we can see the post-compaction numbers).
	let lastPercentBucket = -1;
	let bucketResetPending = false;
	// Drain-once trigger: percent / compact / interval arms it, the next
	// `context` event consumes and clears it (dedupes back-to-back firings).
	let pending: PendingReason | null = null;

	const statusLine = () =>
		sources.length === 0
			? undefined
			: `rules: ${sources.length} file(s), ${enabled ? `on (${describeTriggers(config)})` : "off"}`;
	const showStatus = (ctx: ExtensionContext) => ctx.ui.setStatus(EXTENSION_ID, statusLine());

	const resetState = () => {
		turnsSinceLastInjection = 0;
		reminderCount = 0;
		pending = null;
		lastPercentBucket = -1;
		bucketResetPending = false;
	};

	pi.on("session_start", async (_event, ctx) => {
		resetState();
		sources = loadRuleSources(ctx.cwd);
		showStatus(ctx);
	});

	// No resources to release; reset in-memory state so a replacement session
	// (new / resume / fork / reload) starts from a clean slate.
	pi.on("session_shutdown", () => resetState());

	pi.registerCommand("rules-reminder", {
		description: "Toggle AGENTS.md rules reminder",
		handler: async (_args, ctx) => {
			enabled = !enabled;
			const detail = sources.map((s) => s.file.replace(os.homedir(), "~")).join(", ");
			ctx.ui.notify(
				`Rules reminder ${enabled ? "ENABLED" : "DISABLED"}${detail ? ` (${detail})` : sources.length === 0 ? " — no AGENTS.md found" : ""}`,
				"info",
			);
			showStatus(ctx);
		},
	});

	// Optional turn-based trigger. Off by default (PI_RULES_TURN_INTERVAL=0);
	// when on, arms a reminder every N turns but does NOT preempt an
	// already-armed percent or compact trigger.
	pi.on("turn_end", () => {
		if (!enabled || sources.length === 0 || config.turnInterval <= 0) return;
		turnsSinceLastInjection += 1;
		if (turnsSinceLastInjection >= config.turnInterval && !pending) {
			pending = "interval";
		}
	});

	// Compaction trigger. Re-anchors the model on any fresh context
	// (manual /compact, threshold auto-compaction, overflow recovery).
	// The bucket reset is DEFERRED to the next `context` event because
	// the post-compaction usage isn't known until the next LLM call —
	// reseating here would risk a spurious post-compaction firing.
	pi.on("session_compact", (event) => {
		if (!enabled || sources.length === 0) return;
		if (config.onCompact) {
			pending = `compact:${event.reason}`;
		}
		turnsSinceLastInjection = 0;
		bucketResetPending = true;
	});

	// Inject as the LAST message: preserves the prompt-cache prefix and lands
	// where recency attention is strongest. `setStatus` is a fire-and-forget
	// method that works in every mode, so no mode/hasUI guard is needed.
	pi.on("context", async (_event, ctx) => {
		if (!enabled || sources.length === 0) return undefined;

		// Reseat the percent-bucket tracker against the post-compaction
		// (or first-of-session) usage. Reseat BEFORE threshold check so
		// we don't fire spuriously for the bucket we're currently inside.
		if (bucketResetPending) {
			const resetUsage = ctx.getContextUsage();
			if (resetUsage?.percent != null && config.percentStep > 0) {
				lastPercentBucket = Math.floor(resetUsage.percent / config.percentStep);
			} else {
				lastPercentBucket = -1;
			}
			bucketResetPending = false;
		}

		// Percent threshold: arm if we've entered a new bucket. Does NOT
		// preempt an already-armed pending — e.g. a compact reminder
		// already armed will fire first.
		if (!pending && config.percentStep > 0) {
			const usage = ctx.getContextUsage();
			if (usage?.percent != null) {
				const bucket = Math.floor(usage.percent / config.percentStep);
				if (bucket > lastPercentBucket) {
					pending = `percent:${bucket}`;
					lastPercentBucket = bucket;
					turnsSinceLastInjection = 0;
				}
			}
		}

		if (!pending) return undefined;

		reminderCount += 1;
		const reason = pending;
		pending = null;
		turnsSinceLastInjection = 0;

		ctx.ui.setStatus(EXTENSION_ID, `rules reminder #${reminderCount} (${reason})`);

		return {
			messages: [
				..._event.messages,
				{
					role: "user" as const,
					content: [
						{ type: "text" as const, text: buildReminder(sources, reminderCount, reason) },
					],
					timestamp: Date.now(),
				},
			],
		};
	});
}