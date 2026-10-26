import test from "node:test";
import assert from "node:assert/strict";
import { existsSync, readFileSync } from "node:fs";
import { hostAllowed, minimalEnv, sandboxProfile, scratchAwsDir } from "./harden";
import { redact } from "./vault";
import { runWithCredentials } from "./exec";

const creds = {
  AWS_ACCESS_KEY_ID: "ASIAFAKEKEY123456789",
  AWS_SECRET_ACCESS_KEY: "fakeSecret/with+chars1234567890abcdefGHIJ",
  AWS_SESSION_TOKEN: "fakeTokenAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  AWS_PROFILE: "test-profile",
  AWS_REGION: "eu-west-1",
};
const mac = process.platform === "darwin" && existsSync("/usr/bin/sandbox-exec");

test("minimalEnv keeps safe vars and drops agent sockets, tokens and inherited creds", () => {
  const env = minimalEnv({
    PATH: "/bin", HOME: "/h", SSH_AUTH_SOCK: "/tmp/agent", GITHUB_TOKEN: "x", AWS_ACCESS_KEY_ID: "old", OPENAI_API_KEY: "k",
  });
  assert.deepEqual(env, { PATH: "/bin", HOME: "/h" });
  assert.deepEqual(minimalEnv({ PATH: "/bin", MY_TOOL_TOKEN: "t" }, ["MY_TOOL_TOKEN"]), { PATH: "/bin", MY_TOOL_TOKEN: "t" });
});

test("scratch AWS dir has the profile stanza and is removed", () => {
  const s = scratchAwsDir("p1", "eu-west-1");
  assert.equal(readFileSync(s.env.AWS_CONFIG_FILE, "utf8"), "[profile p1]\nregion = eu-west-1\n");
  assert.equal(readFileSync(s.env.AWS_SHARED_CREDENTIALS_FILE, "utf8"), "");
  s.cleanup();
  assert.equal(existsSync(s.env.AWS_CONFIG_FILE), false);
});

test("hostAllowed matches exact names and *.suffix only", () => {
  const allowed = ["*.amazonaws.com", "pypi.org"];
  assert.ok(hostAllowed("s3.eu-west-1.amazonaws.com", allowed));
  assert.ok(hostAllowed("PyPI.org.", allowed));
  assert.ok(!hostAllowed("amazonaws.com.evil.io", allowed));
  assert.ok(!hostAllowed("evilamazonaws.com", allowed));
  assert.ok(!hostAllowed("amazonaws.com", allowed));
  assert.match(sandboxProfile(1234), /localhost:1234/);
});

test("redact catches hex, reversed, base64 at any offset, and credential shapes", () => {
  const secret = creds.AWS_SECRET_ACCESS_KEY;
  const leaks = [
    Buffer.from(secret).toString("hex"),
    secret.split("").reverse().join(""),
    Buffer.from("x" + secret).toString("base64"),
    Buffer.from("xy" + secret).toString("base64"),
    Buffer.from("xyz" + secret).toString("base64"),
  ];
  for (const leak of leaks) {
    const out = redact(`leak:${leak}:end`, creds);
    assert.ok(out.includes("[REDACTED]"), `masked ${leak}`);
  }
  // keys that are not the vault's own (e.g. printed by sts assume-role)
  const other = redact('AccessKeyId ASIAOTHERKEY00000000 "SecretAccessKey": "abc", "SessionToken": "def"', creds);
  assert.doesNotMatch(other, /ASIAOTHERKEY|"abc"|"def"/);
  // ordinary 40-char strings (git SHAs, paths) survive
  const sha = "0123456789abcdef0123456789abcdef01234567";
  assert.equal(redact(`commit ${sha}`, creds), `commit ${sha}`);
});

test("commands get no SSH agent, a scratch ~/.aws, and the profile works", async () => {
  const r = await runWithCredentials(
    'echo "sock=${SSH_AUTH_SOCK:-none} tok=${GITHUB_TOKEN:-none}"; cat "$AWS_CONFIG_FILE"; echo "ua=$AWS_SDK_UA_APP_ID"',
    creds,
    { timeoutMs: 5000, baseEnv: { PATH: process.env.PATH, HOME: "/tmp", SSH_AUTH_SOCK: "/tmp/agent", GITHUB_TOKEN: "ghp_x" } },
  );
  assert.equal(r.exitCode, 0);
  assert.match(r.output, /sock=none tok=none/);
  assert.match(r.output, /\[profile test-profile\]/);
  assert.match(r.output, /ua=pi-agent/);
});

test("sandbox: only allowed hosts through the proxy, no direct connections", { skip: !mac }, async () => {
  const r = await runWithCredentials(
    [
      'curl -s -o /dev/null -w "aws=%{http_code}\\n" -m 10 https://sts.amazonaws.com/',
      'curl -s -o /dev/null -w "evil=%{http_code}\\n" -m 10 https://example.com/ || echo "evil=blocked"',
      'curl -s -o /dev/null -w "direct=%{http_code}\\n" -m 5 --noproxy "*" https://sts.amazonaws.com/ || echo "direct=blocked"',
      'nc -z -w 2 127.0.0.1 22 && echo "ssh=open" || echo "ssh=blocked"',
    ].join("; "),
    creds,
    { timeoutMs: 30_000, sandbox: {}, baseEnv: { PATH: process.env.PATH, HOME: "/tmp" } },
  );
  assert.match(r.output, /aws=\d{3}/);
  assert.doesNotMatch(r.output, /aws=000/);
  assert.match(r.output, /evil=(blocked|000|403)/);
  assert.match(r.output, /direct=(blocked|000)/);
  assert.match(r.output, /ssh=blocked/);
  assert.ok(r.refusedHosts.some((h) => h.startsWith("example.com")));
});
