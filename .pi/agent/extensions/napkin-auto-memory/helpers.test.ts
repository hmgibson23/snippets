import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  appendDiscoveryEntry,
  buildDecisionMemory,
  findProjectNapkinVault,
  slugify,
} from "./helpers";

test("findProjectNapkinVault returns nearest ancestor containing .napkin", () => {
  const root = mkdtempSync(join(tmpdir(), "napkin-auto-memory-"));
  const project = join(root, "project");
  const nested = join(project, "src", "feature");
  mkdirSync(join(project, ".napkin"), { recursive: true });
  mkdirSync(nested, { recursive: true });

  assert.equal(findProjectNapkinVault(nested), project);
});

test("slugify normalizes titles for date-prefixed decision notes", () => {
  assert.equal(slugify("Use Vitest for Pi Extension Tests"), "use-vitest-for-pi-extension-tests");
  assert.equal(slugify("Debug repo quirks & API behavior"), "debug-repo-quirks-api-behavior");
});

test("buildDecisionMemory creates a dated path and structured markdown", () => {
  const memory = buildDecisionMemory({
    date: "2026-04-17",
    title: "Use napkin capture tool",
    summary: "We'll use a dedicated tool so pi can save durable memories directly.",
    rationale: "This keeps capture explicit and lets the model save milestones immediately.",
    evidence: "Designed after inspecting pi extension events and napkin's file-based vault structure.",
    files: ["agent/extensions/napkin-auto-memory/index.ts"],
  });

  assert.equal(memory.relativePath, "decisions/2026-04-17-use-napkin-capture-tool.md");
  assert.match(memory.content, /^---\ntype: decision\ndate: 2026-04-17\nstatus: accepted\nsource: pi-auto\n---/);
  assert.match(memory.content, /## Decision\n\nWe'll use a dedicated tool so pi can save durable memories directly\./);
  assert.match(memory.content, /## Why\n\nThis keeps capture explicit/);
  assert.match(memory.content, /## Evidence\n\nDesigned after inspecting pi extension events/);
  assert.match(memory.content, /- `agent\/extensions\/napkin-auto-memory\/index.ts`/);
});

test("appendDiscoveryEntry creates dated sections and avoids duplicate entries", () => {
  const first = appendDiscoveryEntry("", {
    date: "2026-04-17",
    title: "Project vaults resolve locally",
    summary: "The extension should only write to project-local Napkin vaults.",
    evidence: "Nearest .napkin directory wins.",
    files: [".napkin/config.json"],
  });

  assert.equal(first.didAppend, true);
  assert.match(first.nextContent, /^# Discoveries\n\n## 2026-04-17\n\n- \*\*Project vaults resolve locally\*\*: The extension should only write to project-local Napkin vaults\. Evidence: Nearest \.napkin directory wins\. Files: `\.napkin\/config\.json`\n$/);

  const second = appendDiscoveryEntry(first.nextContent, {
    date: "2026-04-17",
    title: "Project vaults resolve locally",
    summary: "The extension should only write to project-local Napkin vaults.",
    evidence: "Nearest .napkin directory wins.",
    files: [".napkin/config.json"],
  });

  assert.equal(second.didAppend, false);
  assert.equal(second.nextContent, first.nextContent);
});
