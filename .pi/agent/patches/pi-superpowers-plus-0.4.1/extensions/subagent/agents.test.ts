import { afterEach, describe, expect, it } from "vitest";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as agentModule from "./agents.js";
import { loadAgentsFromDir } from "./agents.js";

const temporaryDirectories: string[] = [];

afterEach(() => {
  for (const dir of temporaryDirectories.splice(0)) fs.rmSync(dir, { recursive: true, force: true });
});

describe("loadAgentsFromDir", () => {
  it("parses a supported thinking level from agent frontmatter", () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "pi-subagent-agent-test-"));
    temporaryDirectories.push(dir);
    fs.writeFileSync(
      path.join(dir, "agent.md"),
      [
        "---",
        "name: test-agent",
        "description: Test agent",
        "tools: read",
        "model: openai-codex/gpt-5.6-terra",
        "thinking: high",
        "---",
        "Test prompt.",
      ].join("\n"),
    );

    const [agent] = loadAgentsFromDir(dir, "user");

    expect(agent.thinking).toBe("high");
  });

  it("applies invocation model and thinking overrides without mutating the profile", () => {
    const withInvocationOverrides = (agentModule as typeof agentModule & {
      withInvocationOverrides?: (agent: (typeof agentModule)["loadAgentsFromDir"] extends (...args: never[]) => infer R ? R extends Array<infer A> ? A : never : never, overrides: { model: string; thinking: "max" }) => unknown;
    }).withInvocationOverrides;
    expect(withInvocationOverrides).toBeTypeOf("function");

    const agent = {
      name: "test-agent",
      description: "Test agent",
      model: "openai-codex/gpt-5.6-terra",
      thinking: "high" as const,
      systemPrompt: "Test prompt.",
      source: "user" as const,
      filePath: "/tmp/test-agent.md",
    };
    const effective = withInvocationOverrides!(agent, {
      model: "openai-codex/gpt-5.6-luna",
      thinking: "max",
    }) as typeof agent;

    expect(effective).toMatchObject({ model: "openai-codex/gpt-5.6-luna", thinking: "max" });
    expect(agent).toMatchObject({ model: "openai-codex/gpt-5.6-terra", thinking: "high" });
  });
});
