import test from "node:test";
import assert from "node:assert/strict";
import {
	buildOverrideInstructions,
	createStateEntry,
	extractStateFromEntries,
	removeContextFileSections,
} from "./logic.ts";

test("extractStateFromEntries returns the latest active override", () => {
	const state = extractStateFromEntries([
		{ type: "custom", customType: "context-override-state", data: createStateEntry(true, "first override") },
		{ type: "custom", customType: "context-override-state", data: createStateEntry(true, "latest override") },
	]);

	assert.equal(state.enabled, true);
	assert.equal(state.override, "latest override");
});

test("extractStateFromEntries respects a clear entry", () => {
	const state = extractStateFromEntries([
		{ type: "custom", customType: "context-override-state", data: createStateEntry(true, "first override") },
		{ type: "custom", customType: "context-override-state", data: createStateEntry(false) },
	]);

	assert.equal(state.enabled, false);
	assert.equal(state.override, undefined);
});

test("removeContextFileSections removes AGENTS.md and CLAUDE.md from the cwd but keeps inherited files", () => {
	const systemPrompt = `Base system prompt\n\n# Project Context\n\nProject-specific instructions and guidelines:\n\n## /repo/parent/CLAUDE.md\n\nparent rules\n\n## /repo/current/AGENTS.md\n\nlocal agents\n\n## /repo/current/CLAUDE.md\n\nlocal claude\n\n# Skills\n\n...`;

	const stripped = removeContextFileSections(systemPrompt, [
		{ path: "/repo/current/AGENTS.md", content: "local agents" },
		{ path: "/repo/current/CLAUDE.md", content: "local claude" },
	]);

	assert.match(stripped, /parent rules/);
	assert.doesNotMatch(stripped, /local agents/);
	assert.doesNotMatch(stripped, /local claude/);
});

test("buildOverrideInstructions explains that cwd context files are ignored", () => {
	const text = buildOverrideInstructions("Ignore local testing instructions for this session.");

	assert.match(text, /current working directory/i);
	assert.match(text, /AGENTS\.md/i);
	assert.match(text, /CLAUDE\.md/i);
	assert.match(text, /Ignore local testing instructions for this session\./i);
});

test("buildOverrideInstructions works without extra override text", () => {
	const text = buildOverrideInstructions();

	assert.match(text, /ignored/i);
	assert.doesNotMatch(text, /Active override:/i);
});
