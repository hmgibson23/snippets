/**
 * Team Permission Gate
 *
 * Forces Pi to ask explicit permission before using the `teams` tool
 * (spawn, delegate, message, etc.). Once approved, the choice is
 * remembered for the rest of the session.
 *
 * - "Allow once"     → permits this single call only, ask again next time
 * - "Allow session"  → permits all `teams` calls until pi exits / session reload
 * - "Block"          → blocks the call with a reason the agent can read
 *
 * In non-interactive mode (no UI), all `teams` calls are blocked by default.
 * Calls to `team_done` are always allowed and reset the session approval
 * so a new team would have to be re-approved.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	let sessionApproved = false;

	pi.on("session_start", () => {
		sessionApproved = false;
	});

	pi.on("tool_call", async (event, ctx) => {
		if (event.toolName !== "teams") return undefined;

		const action = (event.input as { action?: string })?.action ?? "delegate";

		// Always allow tearing down a team and reset approval.
		if (action === "team_done") {
			sessionApproved = false;
			return undefined;
		}

		if (sessionApproved) return undefined;

		if (!ctx.hasUI) {
			return {
				block: true,
				reason:
					"Team usage requires explicit user permission. No UI is available to confirm. " +
					"Ask the user in your next message whether to use the teams tool, then proceed only after they say yes.",
			};
		}

		// Build a short preview of what the agent wants to do.
		const input = event.input as Record<string, unknown>;
		const previewParts: string[] = [`action: ${action}`];
		if (Array.isArray(input.tasks)) {
			previewParts.push(`tasks: ${input.tasks.length}`);
		}
		if (typeof input.name === "string") previewParts.push(`name: ${input.name}`);
		if (typeof input.message === "string") {
			const msg = input.message.slice(0, 80);
			previewParts.push(`message: ${msg}${input.message.length > 80 ? "…" : ""}`);
		}
		if (typeof input.model === "string") previewParts.push(`model: ${input.model}`);
		const preview = previewParts.join("\n  ");

		const choice = await ctx.ui.select(
			`🤝 Allow Pi to use the teams tool?\n\n  ${preview}`,
			["Allow once", "Allow for this session", "Block"],
		);

		if (choice === "Allow for this session") {
			sessionApproved = true;
			ctx.ui.notify("Teams approved for this session", "info");
			return undefined;
		}

		if (choice === "Allow once") {
			return undefined;
		}

		return {
			block: true,
			reason:
				"User declined team usage. Do not call the teams tool again unless the user explicitly asks. " +
				"Continue solo, or ask the user how they would like to proceed.",
		};
	});

	// Slash command so the user can revoke approval mid-session if they want.
	pi.registerCommand("team-gate-reset", {
		description: "Revoke the current session's approval to use the teams tool",
		handler: async (_args, ctx) => {
			sessionApproved = false;
			ctx.ui.notify("Team permission revoked. Pi must ask again before using teams.", "info");
		},
	});
}
