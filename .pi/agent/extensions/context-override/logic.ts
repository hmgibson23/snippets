export const CONTEXT_OVERRIDE_CUSTOM_TYPE = "context-override-state";

export interface ContextOverrideState {
	enabled: boolean;
	override?: string;
}

export interface ContextOverrideStateEntry {
	enabled: boolean;
	override: string | null;
}

export interface CustomStateLike {
	type?: unknown;
	customType?: unknown;
	data?: unknown;
}

export interface ContextFileLike {
	path: string;
	content: string;
}

export function createStateEntry(enabled: boolean, override?: string): ContextOverrideStateEntry {
	return {
		enabled,
		override: override ?? null,
	};
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null;
}

function asTrimmedString(value: unknown): string | undefined {
	return typeof value === "string" && value.trim().length > 0 ? value.trim() : undefined;
}

export function extractStateFromEntries(entries: CustomStateLike[]): ContextOverrideState {
	let current: ContextOverrideState = { enabled: false };

	for (const entry of entries) {
		if (entry.type !== "custom" || entry.customType !== CONTEXT_OVERRIDE_CUSTOM_TYPE || !isRecord(entry.data)) {
			continue;
		}

		const enabled = entry.data.enabled;
		if (typeof enabled !== "boolean") {
			continue;
		}

		current = {
			enabled,
			override: enabled ? asTrimmedString(entry.data.override) : undefined,
		};
	}

	return current;
}

export function removeContextFileSections(systemPrompt: string, files: ContextFileLike[]): string {
	let nextPrompt = systemPrompt;

	for (const file of files) {
		const block = `## ${file.path}\n\n${file.content}\n\n`;
		nextPrompt = nextPrompt.replace(block, "");
	}

	return nextPrompt;
}

export function buildOverrideInstructions(override?: string): string {
	const activeOverride = asTrimmedString(override);
	return `## Runtime Context Override

An explicit runtime override is active for this session.

Current working directory context rule:
- Ignore AGENTS.md and CLAUDE.md files discovered in the current working directory.
- Inherited context files from parent directories remain available unless separately superseded.
- Treat this runtime override as higher priority than the ignored cwd context files.
${activeOverride ? `
Active override:
${activeOverride}` : ""}`;
}
