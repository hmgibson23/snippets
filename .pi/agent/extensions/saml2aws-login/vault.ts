// In-memory credential vault. Credentials live only in this module and are never
// written to process.env, so pi's bash tool (and anything it spawns) cannot see them.

export const SECRET_KEYS = [
  "AWS_ACCESS_KEY_ID",
  "AWS_SECRET_ACCESS_KEY",
  "AWS_SESSION_TOKEN",
  "AWS_SECURITY_TOKEN",
] as const;

export interface VaultEntry {
  env: Record<string, string>; // everything injected into exec'd commands
  label: string;
  account?: string;
  /** How commands are run (from the config at login time). */
  run?: { sandbox: boolean; allowedHosts: string[]; inheritEnv: string[] };
  /** Wipe after this long without an aws_exec call (ms, 0 = never). */
  idleMs?: number;
  lastUsed?: number;
}

let entry: VaultEntry | undefined;

export function setVault(next: VaultEntry): void {
  entry = next;
}

export function clearVault(): void {
  entry = undefined;
}

export function getVault(): VaultEntry | undefined {
  return entry;
}

/** Mark the credentials as used now (resets the idle timer). */
export function touchVault(now: number = Date.now()): void {
  if (entry) entry.lastUsed = now;
}

/** Clear the vault if it has been idle too long; returns true if it was cleared. */
export function expireIdle(now: number = Date.now()): boolean {
  if (!entry?.idleMs) return false;
  if (now - (entry.lastUsed ?? now) < entry.idleMs) return false;
  entry = undefined;
  return true;
}

/** Remove credential variables from a process env copy / process.env. */
export function scrubSecretKeys(env: Record<string, string | undefined>): void {
  for (const key of [...SECRET_KEYS, "AWS_CREDENTIAL_EXPIRATION"]) delete env[key];
}

/** Shapes of AWS credentials, masked even when they are not the vault's own (e.g. a role's keys printed by sts). */
const CREDENTIAL_SHAPES: RegExp[] = [
  /\b(?:AKIA|ASIA|AROA|AIDA|ANPA|ANVA|AGPA)[A-Z0-9]{16}\b/g, // access key / unique ids
  /(?<![A-Za-z0-9/+=])[A-Za-z0-9/+]{40}(?![A-Za-z0-9/+=])/g, // secret access keys are 40 chars of base64 alphabet
  /\b(?:IQoJb3JpZ2lu|FwoGZXIvYXdz)[A-Za-z0-9/+=]{100,}/g, // session tokens ("IQoJb3JpZ2lu" = base64 "\x21\n\torigin")
  /"(?:SecretAccessKey|SessionToken|secret_access_key|session_token)"\s*:\s*"[^"]+"/g, // JSON fields from sts/sso
];

/** A 40-char run is only masked as a secret key if it is mixed-case + digits and not hex (git SHAs, paths). */
function looksLikeSecretKey(m: string): boolean {
  return /[A-Z]/.test(m) && /[a-z]/.test(m) && /[0-9]/.test(m) && !/^[0-9a-f]{40}$/i.test(m);
}

/** Every encoding of a secret worth masking: raw, URL-encoded, base64 (3 alignments), hex, reversed. */
function variants(value: string): string[] {
  const out = [value, encodeURIComponent(value), value.split("").reverse().join("")];
  const bytes = Buffer.from(value);
  out.push(bytes.toString("hex"), bytes.toString("hex").toUpperCase());
  for (let shift = 0; shift < 3; shift++) {
    // base64 of the secret inside a longer string: only the middle is stable, so mask the
    // aligned middle part (drop the first chars that depend on what came before)
    const b64 = Buffer.concat([Buffer.alloc(shift), bytes]).toString("base64");
    // the first 4-char group mixes in the preceding bytes, the last one the following bytes
    out.push(b64.slice(shift ? 4 : 0, b64.length - 4));
  }
  return out;
}

/** Replace every occurrence of a secret value (and credential-shaped strings) with a marker. */
export function redact(text: string, env: Record<string, string>): string {
  const needles = new Set<string>();
  for (const key of SECRET_KEYS) {
    const value = env[key];
    if (value) for (const v of variants(value)) needles.add(v);
  }
  let out = text;
  // longest first so a short value never partially masks a longer one
  for (const needle of [...needles].sort((a, b) => b.length - a.length)) {
    if (needle.length >= 12) out = out.split(needle).join("[REDACTED]");
  }
  for (const shape of CREDENTIAL_SHAPES) {
    out = out.replace(shape, (m) => {
      if (m.startsWith('"')) return m.replace(/:\s*"[^"]+"$/, ': "[REDACTED]"');
      if (m.length === 40 && !looksLikeSecretKey(m)) return m;
      return "[REDACTED]";
    });
  }
  return out;
}

export function assertNotExpired(env: Record<string, string>, now: Date = new Date()): void {
  const exp = env.AWS_CREDENTIAL_EXPIRATION;
  if (!exp) return;
  const at = new Date(exp);
  if (!Number.isNaN(at.getTime()) && at.getTime() <= now.getTime()) {
    throw new Error(`AWS credentials expired at ${exp}. Ask the user to run /saml2aws-login.`);
  }
}
