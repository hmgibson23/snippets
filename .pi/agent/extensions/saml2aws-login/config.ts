import { readFileSync } from "node:fs";

export interface Saml2AwsConfig {
  defaultProfile: string;
  sessionDuration: number;
  awsRegion: string;
  extraEnv: Record<string, string>;
  /** Wipe the in-memory credentials after this many idle minutes (0 = never). */
  idleMinutes: number;
  /** Run aws_exec commands under the macOS network sandbox (AWS + allowedHosts only). */
  sandbox: boolean;
  /** Extra hosts the sandbox lets through (e.g. "*.example.com"). */
  allowedHosts: string[];
  /** Extra variables commands may inherit from pi (default: a small safe list). */
  inheritEnv: string[];
  /** Account ids shown in red in the status bar (prod). */
  prodAccounts: string[];
}

export interface ResolvedSaml2AwsConfig extends Saml2AwsConfig {
  profile: string;
}

export function loadConfig(path: string): Saml2AwsConfig {
  const raw = JSON.parse(readFileSync(path, "utf8")) as {
    defaultProfile?: unknown;
    sessionDuration?: unknown;
    awsRegion?: unknown;
    extraEnv?: Record<string, unknown>;
    idleMinutes?: unknown;
    sandbox?: unknown;
    allowedHosts?: unknown;
    inheritEnv?: unknown;
    prodAccounts?: unknown;
  };
  const strings = (v: unknown): string[] => (Array.isArray(v) ? v.map(String) : []);

  return {
    defaultProfile: String(raw.defaultProfile ?? ""),
    sessionDuration: Number(raw.sessionDuration ?? 3600),
    awsRegion: String(raw.awsRegion ?? "us-east-1"),
    extraEnv: Object.fromEntries(
      Object.entries(raw.extraEnv ?? {}).map(([key, value]) => [key, String(value)]),
    ),
    idleMinutes: Number(raw.idleMinutes ?? 20),
    sandbox: raw.sandbox === undefined ? process.platform === "darwin" : Boolean(raw.sandbox),
    allowedHosts: strings(raw.allowedHosts),
    inheritEnv: strings(raw.inheritEnv),
    prodAccounts: strings(raw.prodAccounts),
  };
}

export function resolveConfig(config: Saml2AwsConfig, args: string): ResolvedSaml2AwsConfig {
  const profile = args.trim() || config.defaultProfile;
  if (!profile) throw new Error("No saml2aws profile configured or supplied");

  return {
    ...config,
    profile,
  };
}
