/**
 * Rules Reminder Extension
 *
 * Periodically re-injects a one-line "remember the rules" note into the
 * session so the model does not drift from AGENTS.md as the context window
 * fills. The note is delivered as a persisted custom message queued for the
 * user's next turn (`deliverAs: "nextTurn"`), so it appears in the session
 * log and in the LLM context without triggering an immediate response.
 *
 * Triggers (a single pending flag dedupes back-to-back firings):
 *   - Percent: every time `ctx.getContextUsage().percent` enters a new
 *     `PI_RULES_PERCENT_STEP`-sized bucket >= 1 (>= 10% by default).
 *   - Compaction: any session_compact event (manual /compact, threshold
 *     auto-compaction, or overflow recovery).
 *
 * The pending flag clears when the queued message is detected in the next
 * context event's messages array (the only delivery signal available).
 *
 * Configuration:
 *   PI_RULES_DISABLED      "1" disables the extension entirely
 *   PI_RULES_PERCENT_STEP  bucket size 1..100 (default 10; 0 disables the
 *                          percent trigger; compaction trigger still fires)
 *
 * Command:
 *   /rules-reminder  -> toggle on/off and show status
 */

import {
	type ExtensionAPI,
	type ExtensionContext,
	type SessionCompactEvent,
} from "@earendil-works/pi-coding-agent";

const STATUS_KEY = "rules-reminder";
const CUSTOM_TYPE = "rules-reminder";
const DEFAULT_PERCENT_STEP = 10;

const REMINDER_TEXT =
	"[system-reminder] MUST Remember the AGENTS.md rules.\n" +
	"Do not respond to this message directly; continue with the user's next request.";

type PendingReason = "percent" | `compact:${SessionCompactEvent["reason"]}`;

interface Config {
	disabled: boolean;
	percentStep: number;
}

interface State {
	enabled: boolean;
	reminderCount: number;
	lastBucket: number;
	pending: boolean;
}

function parseIntInRange(value: string | undefined, fallback: number, min: number, max: number): number {
	if (value === undefined || value.trim() === "") return fallback;
	const n = Number(value);
	if (!Number.isFinite(n)) return fallback;
	return Math.min(Math.max(Math.floor(n), min), max);
}

function loadConfig(): Config {
	return {
		disabled: process.env.PI_RULES_DISABLED === "1",
		percentStep: parseIntInRange(process.env.PI_RULES_PERCENT_STEP, DEFAULT_PERCENT_STEP, 0, 100),
	};
}

function bucketFor(percent: number, step: number): number {
	return Math.floor(percent / step);
}

function statusText(state: State, percentStep: number): string | undefined {
	if (!state.enabled || percentStep <= 0) return undefined;
	return `reminder ${state.reminderCount}`;
}

function refreshStatus(ctx: ExtensionContext, state: State, percentStep: number): void {
	ctx.ui.setStatus(STATUS_KEY, statusText(state, percentStep));
}

function isReminderMessage(message: unknown): boolean {
	return (
		typeof message === "object" &&
		message !== null &&
		"customType" in message &&
		(message as { customType: unknown }).customType === CUSTOM_TYPE
	);
}

export default function rulesReminderExtension(pi: ExtensionAPI) {
	const config = loadConfig();
	const state: State = {
		enabled: !config.disabled,
		reminderCount: 0,
		lastBucket: -1,
		pending: false,
	};

	const resetTransient = () => {
		state.reminderCount = 0;
		state.lastBucket = -1;
		state.pending = false;
	};

	const queueReminder = (reason: PendingReason, ctx: ExtensionContext) => {
		if (state.pending) return;
		state.reminderCount += 1;
		state.pending = true;
		pi.sendMessage(
			{
				customType: CUSTOM_TYPE,
				content: REMINDER_TEXT,
				display: true,
				details: { reason, count: state.reminderCount },
			},
			{ deliverAs: "nextTurn" },
		);
		refreshStatus(ctx, state, config.percentStep);
	};

	pi.on("session_start", async (_event, ctx) => {
		resetTransient();
		refreshStatus(ctx, state, config.percentStep);
	});

	// New / resume / fork / reload tears down and re-creates the runtime,
	// so this also implicitly clears state via the fresh `state` literal.
	pi.on("session_shutdown", () => resetTransient());

	pi.registerCommand("rules-reminder", {
		description: "Toggle the AGENTS.md rules reminder",
		handler: async (_args, ctx) => {
			state.enabled = !state.enabled;
			ctx.ui.notify(`Rules reminder ${state.enabled ? "ENABLED" : "DISABLED"}`, "info");
			refreshStatus(ctx, state, config.percentStep);
		},
	});

	pi.on("session_compact", (event, ctx) => {
		// Reseed lastBucket against post-compact usage so we do not fire a
		// duplicate reminder for the bucket we are currently inside.
		if (config.percentStep > 0) {
			const usage = ctx.getContextUsage();
			state.lastBucket = usage?.percent != null ? bucketFor(usage.percent, config.percentStep) : -1;
		} else {
			state.lastBucket = -1;
		}
		queueReminder(`compact:${event.reason}`, ctx);
	});

	pi.on("context", async (event, ctx) => {
		// Detect delivery of a previously-queued reminder.
		if (state.pending && event.messages.some(isReminderMessage)) {
			state.pending = false;
		}

		if (!state.enabled || state.pending || config.percentStep <= 0) return;

		const usage = ctx.getContextUsage();
		if (usage?.percent == null) return;

		const bucket = bucketFor(usage.percent, config.percentStep);
		if (bucket >= 1 && bucket > state.lastBucket) {
			state.lastBucket = bucket;
			queueReminder("percent", ctx);
		}
	});
}
