// Hardening for commands run with credentials: a minimal environment, a throwaway
// AWS config dir per command, and an optional macOS sandbox that only lets the
// command reach AWS (through a local filtering proxy).

import { mkdtempSync, rmSync, writeFileSync, chmodSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

/** Variables a command may inherit from pi. Everything else (SSH_AUTH_SOCK, API keys, ...) is dropped. */
export const ENV_ALLOW = [
  "PATH", "HOME", "USER", "LOGNAME", "SHELL", "LANG", "LC_ALL", "LC_CTYPE", "TERM", "TMPDIR", "TZ",
  "PWD", "COLORTERM", "NO_COLOR", "VIRTUAL_ENV", "UV_CACHE_DIR",
  // tooling that just changes behaviour, never carries secrets
  "TF_PLUGIN_CACHE_DIR", "TF_IN_AUTOMATION", "TF_CLI_ARGS", "TF_LOG",
  "AWS_PAGER", "AWS_CA_BUNDLE", "SSL_CERT_FILE", "REQUESTS_CA_BUNDLE", "NODE_EXTRA_CA_CERTS",
] as const;

/** Variables that carry credentials for something other than AWS: never passed even if allow-listed by prefix. */
const NEVER = /(TOKEN|SECRET|PASSWORD|PASSWD|API_KEY|_KEY$|AUTH|COOKIE|CREDENTIAL)/i;

export function minimalEnv(base: Record<string, string | undefined>, extraAllow: readonly string[] = []): Record<string, string> {
  const out: Record<string, string> = {};
  for (const key of [...ENV_ALLOW, ...extraAllow]) {
    const value = base[key];
    if (value !== undefined && !(NEVER.test(key) && !extraAllow.includes(key))) out[key] = value;
  }
  return out;
}

/**
 * A private, empty AWS config dir for one command. `aws configure set` / `sso login`
 * etc. write here and it is deleted afterwards, so no credentials end up in ~/.aws.
 * The region and profile still come from the injected env vars.
 */
export function scratchAwsDir(profile?: string, region?: string): { env: Record<string, string>; cleanup: () => void } {
  const dir = mkdtempSync(join(tmpdir(), "aws-exec-"));
  chmodSync(dir, 0o700);
  const config = join(dir, "config");
  const credentials = join(dir, "credentials");
  // AWS_PROFILE must name a profile that exists, or the CLI/SDKs refuse to start; the
  // stanza has no credentials, so the env vars are what authenticate.
  const stanza = profile && profile !== "default" ? `[profile ${profile}]` : "[default]";
  writeFileSync(config, `${stanza}\n${region ? `region = ${region}\n` : ""}`, { mode: 0o600 });
  writeFileSync(credentials, "", { mode: 0o600 });
  return {
    env: {
      AWS_CONFIG_FILE: config,
      AWS_SHARED_CREDENTIALS_FILE: credentials,
      // CLI caches (sso/cli) and the SDK's ini lookups stay inside the dir too
      AWS_CLI_CACHE_DIR: join(dir, "cache"),
      AWS_SDK_LOAD_CONFIG: "1",
      AWS_EC2_METADATA_DISABLED: "true", // never fall back to some other identity
      AWS_SDK_UA_APP_ID: "pi-agent", // shows up in CloudTrail userAgent
      AWS_PAGER: "",
    },
    cleanup: () => rmSync(dir, { recursive: true, force: true }),
  };
}

/** Hosts the sandboxed command may reach (through the proxy). */
export const DEFAULT_ALLOWED_HOSTS = [
  "*.amazonaws.com",
  "*.amazonaws.com.cn",
  "*.aws.amazon.com",
  "*.api.aws",
  // terraform provider/module downloads
  "registry.terraform.io",
  "releases.hashicorp.com",
  "*.hashicorp.com",
  // GitHub downloads only (release assets, archive tarballs used by uv git deps);
  // api.github.com / gist / raw uploads are NOT allowed
  "objects.githubusercontent.com",
  "release-assets.githubusercontent.com",
  "codeload.github.com",
  "github.com",
  // python packages (uv run in project dirs)
  "pypi.org",
  "files.pythonhosted.org",
];

export function hostAllowed(host: string, allowed: readonly string[]): boolean {
  const h = host.toLowerCase().replace(/\.$/, "");
  return allowed.some((pattern) => {
    const p = pattern.toLowerCase();
    if (p.startsWith("*.")) return h.endsWith(p.slice(1)) && h.length > p.length - 1;
    return h === p;
  });
}

/**
 * sandbox-exec profile: everything allowed except outbound network, which may only
 * go to the local proxy port and to unix sockets under the temp dirs (used by
 * python multiprocessing, terraform plugins, ...). DNS lookups fail inside, which is
 * fine: proxied clients send the hostname to the proxy.
 */
export function sandboxProfile(proxyPort: number): string {
  return [
    "(version 1)",
    "(allow default)",
    "(deny network-outbound)",
    `(allow network-outbound (remote ip "localhost:${proxyPort}"))`,
    '(allow network-outbound (remote unix-socket (subpath "/private/tmp")))',
    '(allow network-outbound (remote unix-socket (subpath "/private/var/folders")))',
    // the SSH agent socket lives under /private/tmp/com.apple.launchd.*; deny it explicitly
    '(deny network-outbound (remote unix-socket (regex #"^/private/tmp/com\\.apple\\.launchd\\.[^/]+/Listeners$")))',
  ].join("\n");
}

export function proxyEnv(port: number): Record<string, string> {
  const url = `http://127.0.0.1:${port}`;
  return {
    HTTPS_PROXY: url, https_proxy: url, HTTP_PROXY: url, http_proxy: url, ALL_PROXY: url,
    NO_PROXY: "", no_proxy: "",
  };
}
