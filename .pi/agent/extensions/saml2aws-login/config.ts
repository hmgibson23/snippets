import { readFileSync } from "node:fs";

export interface Saml2AwsConfig {
  defaultProfile: string;
  sessionDuration: number;
  awsRegion: string;
  extraEnv: Record<string, string>;
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
  };

  return {
    defaultProfile: String(raw.defaultProfile ?? ""),
    sessionDuration: Number(raw.sessionDuration ?? 3600),
    awsRegion: String(raw.awsRegion ?? "us-east-1"),
    extraEnv: Object.fromEntries(
      Object.entries(raw.extraEnv ?? {}).map(([key, value]) => [key, String(value)]),
    ),
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
