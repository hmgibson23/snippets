import test from "node:test";
import assert from "node:assert/strict";
import { buildHandoffPrompt, extractFileReferences, summarizeBranch } from "./handoff";

test("extractFileReferences finds likely project file paths", () => {
  assert.deepEqual(
    extractFileReferences("Edited src/app.ts and agent/extensions/foo/index.ts; ignore https://example.com/a.ts"),
    ["src/app.ts", "agent/extensions/foo/index.ts"],
  );
});

test("summarizeBranch keeps recent user and assistant messages", () => {
  const summary = summarizeBranch([
    { type: "message", message: { role: "user", content: [{ type: "text", text: "Build thing" }] } },
    { type: "message", message: { role: "assistant", content: [{ type: "text", text: "I changed src/app.ts" }] } },
    { type: "message", message: { role: "toolResult", content: [{ type: "text", text: "noisy" }] } },
  ]);

  assert.deepEqual(summary.userMessages, ["Build thing"]);
  assert.deepEqual(summary.assistantMessages, ["I changed src/app.ts"]);
  assert.deepEqual(summary.files, ["src/app.ts"]);
});

test("buildHandoffPrompt includes goal, cwd, recent messages, and files", () => {
  const prompt = buildHandoffPrompt({
    goal: "Continue implementation",
    cwd: "/repo/project",
    sessionFile: "/tmp/session.jsonl",
    summary: {
      userMessages: ["Build thing"],
      assistantMessages: ["I changed src/app.ts"],
      files: ["src/app.ts"],
    },
  });

  assert.match(prompt, /## Goal\nContinue implementation/);
  assert.match(prompt, /- cwd: `\/repo\/project`/);
  assert.match(prompt, /- `src\/app.ts`/);
  assert.match(prompt, /Build thing/);
});
