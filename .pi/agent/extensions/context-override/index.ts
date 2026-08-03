import { basename, resolve } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@mariozechner/pi-coding-agent";
import { getAgentDir, loadProjectContextFiles } from "@mariozechner/pi-coding-agent";
import {
	buildOverrideInstructions,
	CONTEXT_OVERRIDE_CUSTOM_TYPE,
	createStateEntry,
	extractStateFromEntries,
	removeContextFileSections,
	type ContextFileLike,
} from "./logic.ts";

export default function contextOverrideExtension(pi: ExtensionAPI) {
	let overrideEnabled = false;
	let activeOverride: string | undefined;
	let cwdContextFiles: ContextFileLike[] = [];

	function updateStatus(ctx: ExtensionContext) {
		ctx.ui.setStatus(
			"context-override",
			overrideEnabled ? ctx.ui.theme.fg("warning", "cwd context ignored") : undefined,
		);
	}

	function persistState() {
		pi.appendEntry(CONTEXT_OVERRIDE_CUSTOM_TYPE, createStateEntry(overrideEnabled, activeOverride));
	}

	pi.on("session_start", async (_event, ctx) => {
		const state = extractStateFromEntries(ctx.sessionManager.getBranch());
		overrideEnabled = state.enabled;
		activeOverride = state.override;

		const resolvedCwd = resolve(ctx.cwd);
		cwdContextFiles = loadProjectContextFiles({ cwd: ctx.cwd, agentDir: getAgentDir() }).filter((file) => {
			const fileName = basename(file.path);
			return resolve(file.path, "..") === resolvedCwd && (fileName === "AGENTS.md" || fileName === "CLAUDE.md");
		});

		updateStatus(ctx);
	});

	pi.registerCommand("override-context", {
		description: "Ignore AGENTS.md / CLAUDE.md files in the current working directory for this session",
		handler: async (args, ctx) => {
			overrideEnabled = true;
			activeOverride = args.trim().length > 0 ? args.trim() : undefined;
			persistState();
			updateStatus(ctx);
			ctx.ui.notify(
				activeOverride
					? "Current-directory context files are ignored for this session, with an extra override instruction."
					: "Current-directory AGENTS.md / CLAUDE.md files will be ignored for this session.",
				"info",
			);
		},
	});

	pi.registerCommand("show-context-override", {
		description: "Show whether current-directory context files are being ignored",
		handler: async (_args, ctx) => {
			if (!overrideEnabled) {
				ctx.ui.notify("No context override is active.", "info");
				return;
			}

			const ignoredFiles = cwdContextFiles.length > 0 ? cwdContextFiles.map((file) => `- ${file.path}`).join("\n") : "- none found in cwd";
			const details = activeOverride ? `\n\nExtra override:\n${activeOverride}` : "";
			ctx.ui.notify(`Ignoring current-directory context files:\n${ignoredFiles}${details}`, "info");
		},
	});

	pi.registerCommand("clear-context-override", {
		description: "Stop ignoring current-directory context files",
		handler: async (_args, ctx) => {
			if (!overrideEnabled) {
				ctx.ui.notify("No context override is active.", "info");
				return;
			}

			overrideEnabled = false;
			activeOverride = undefined;
			persistState();
			updateStatus(ctx);
			ctx.ui.notify("Context override cleared.", "info");
		},
	});

	pi.on("before_agent_start", async (event) => {
		if (!overrideEnabled) {
			return;
		}

		const strippedSystemPrompt = removeContextFileSections(event.systemPrompt, cwdContextFiles);
		return {
			systemPrompt: `${strippedSystemPrompt}\n\n${buildOverrideInstructions(activeOverride)}`,
		};
	});
}
