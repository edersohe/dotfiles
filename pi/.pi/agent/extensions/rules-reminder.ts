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
 *   - Every N turns (`turn_end` counter).
 *   - Any compaction (`session_compact`: manual /compact, threshold-driven
 *     auto-compaction, or overflow recovery).
 *
 * Configuration (environment variables):
 *   PI_RULES_INTERVAL  turns between reminders (default 6)
 *   PI_RULES_DISABLED  set to "1" to disable
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
const DEFAULT_INTERVAL = 15;

type PendingReason = "interval" | `compact:${SessionCompactEvent["reason"]}`;

interface RuleSource {
	file: string;
	headers: string[];
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

// NaN-safe env parsing: bad or missing values fall back to the default
// instead of silently disabling interval reminders via NaN comparisons.
function parseInterval(value: string | undefined): number {
	const parsed = Number(value);
	return Number.isFinite(parsed) && parsed >= 1 ? Math.floor(parsed) : DEFAULT_INTERVAL;
}

export default function rulesReminderExtension(pi: ExtensionAPI) {
	const interval = parseInterval(process.env.PI_RULES_INTERVAL);

	let sources: RuleSource[] = [];
	let enabled = process.env.PI_RULES_DISABLED !== "1";
	let reminderCount = 0;
	let turnsSinceLastInjection = 0;
	// Drain-once trigger: interval or compaction arms it, the next `context`
	// event consumes and clears it (dedupes back-to-back firings).
	let pending: PendingReason | null = null;

	const statusLine = () =>
		sources.length === 0
			? undefined
			: `rules: ${sources.length} file(s), ${enabled ? "on" : "off"}`;
	const showStatus = (ctx: ExtensionContext) => ctx.ui.setStatus(EXTENSION_ID, statusLine());

	const resetState = () => {
		turnsSinceLastInjection = 0;
		reminderCount = 0;
		pending = null;
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

	// Interval trigger: arm a reminder every N turns.
	pi.on("turn_end", () => {
		if (!enabled || sources.length === 0) return;
		turnsSinceLastInjection += 1;
		if (turnsSinceLastInjection >= interval && !pending) {
			pending = "interval";
		}
	});

	// Compaction trigger: any reason (manual /compact, threshold auto-compaction
	// such as pi-blackhole, or overflow recovery). Resets the turn counter so a
	// fresh post-compaction context doesn't immediately stack an interval
	// reminder on top, but deliberately does NOT clear an already-armed interval
	// trigger — the next context call fires whichever armed first.
	pi.on("session_compact", (event) => {
		if (!enabled || sources.length === 0) return;
		pending = `compact:${event.reason}`;
		turnsSinceLastInjection = 0;
	});

	// Inject as the LAST message: preserves the prompt-cache prefix and lands
	// where recency attention is strongest. `setStatus` is a fire-and-forget
	// method that works in every mode, so no mode/hasUI guard is needed.
	pi.on("context", async (_event, ctx) => {
		if (!enabled || sources.length === 0 || !pending) return undefined;

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
