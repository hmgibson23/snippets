import test from "node:test";
import assert from "node:assert/strict";
import { buildPiTitle, titleFromPrompt } from "./title";

test("titleFromPrompt creates concise title from user prompt", () => {
  assert.equal(titleFromPrompt("Please implement recent files widget for pi"), "implement recent files widget for pi");
});

test("titleFromPrompt strips slash command prefix", () => {
  assert.equal(titleFromPrompt("/handoff continue the AWS footer work", 40), "continue the AWS footer work");
});

test("titleFromPrompt falls back for blank prompts", () => {
  assert.equal(titleFromPrompt("   ", 40), "session");
});

test("buildPiTitle includes task and cwd basename", () => {
  assert.equal(buildPiTitle({ task: "AWS footer", cwd: "/Users/hugo/project" }), "π - AWS footer - project");
});

test("buildPiTitle omits task when unavailable", () => {
  assert.equal(buildPiTitle({ cwd: "/Users/hugo/project" }), "π - project");
});
