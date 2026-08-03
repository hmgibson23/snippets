---
name: audit-trail-analysis
description: "Analyse an audit trail (CloudTrail, IAM/access logs, CI/CD job logs, git history, application audit logs, or agent action logs) for suspicious or anomalous behaviour. Use when the user hands you a log file, export, or paste and asks you to review it for security incidents, misuse, exfiltration, privilege escalation, or anomalies. Produces an evidence-cited findings report with severities and recommended actions. Read-only: never mutates the source trail."
---

# Audit Trail Analysis

Take an audit trail. Detect suspicious behaviour. Produce an evidence-cited report.

You are an executor. Follow the steps in order. Do not skip steps. Do not invent
findings. Every finding MUST quote the exact log line(s) and timestamp(s) it came from.

## Hard rules (non-negotiable)

1. **Evidence or it didn't happen.** Every finding cites the raw line(s): timestamp, actor,
   action, source. If you can't quote it, don't report it.
2. **Never fabricate.** No guessed IPs, actors, times, or events. If a field is missing, write
   `unknown` — never fill it in.
3. **Read-only.** Never edit, delete, truncate, reorder, or "clean up" the source trail.
   Work from a copy if you must transform.
4. **Never print full secret values.** If the trail contains tokens/keys/passwords, redact to
   first 4 + last 2 chars (`AKIA…9X`) in the report and flag the exposure as a finding.
5. **Separate fact from inference.** State what the log shows, then your interpretation, clearly
   labelled. Rate your confidence (high / medium / low) on every finding.
6. **No false-positive dumping.** Benign-but-noisy events go in a short "reviewed & cleared"
   note, not the findings list. Findings are things a human should actually action.

## Step 0 — Ingest & identify

1. Locate the trail: a file path, a pasted block, or stdin. If ambiguous, ask once for the path.
2. Get size/shape before reading it all:
   ```bash
   wc -l <file>; head -5 <file>; tail -5 <file>
   ```
3. Identify the format. Match one:
   - **AWS CloudTrail** — JSON with `Records[]`, `eventName`, `eventSource`, `userIdentity`, `sourceIPAddress`.
   - **IAM / STS** — `AssumeRole*`, `GetSessionToken`, `ConsoleLogin`.
   - **CI/CD job log** (CircleCI/GitHub Actions) — step names, `assume-role`, env exports, deploy commands.
   - **Access/HTTP log** — method, path, status, IP, user-agent.
   - **Git history** — `git log`, commit authors/times/messages.
   - **Application/agent audit log** — actor + action + resource + timestamp rows.
   - **Unknown/mixed** — treat as line-oriented; infer columns from the header or first rows.
4. Establish the baseline: time range (first→last event), distinct actors, distinct source IPs,
   distinct actions. Record these — anomalies are deviations from this baseline.

If the file is large (>50k lines), don't read it all into reasoning. Use `grep`/`jq`/`awk` to
extract the signal (see Step 2), then reason over the extracts.

## Step 1 — Normalise (mentally or with a tool)

Reduce every event to five fields. Use `jq` for CloudTrail, `awk`/`grep` for line logs:

```
WHEN (timestamp) | WHO (actor/principal/IP) | WHAT (action/event) | TARGET (resource) | RESULT (success/deny/error)
```

CloudTrail extract example:
```bash
jq -r '.Records[] | [.eventTime, (.userIdentity.arn // .userIdentity.type), .eventName, .eventSource, (.errorCode // "OK"), .sourceIPAddress] | @tsv' <file> | sort
```

## Step 2 — Run the detection battery

Check every category below. For each, run the suggested extraction, then judge. A hit becomes a
candidate finding (confirm with evidence in Step 3).

### A. Authentication & access anomalies
- Failed logins / `AccessDenied` bursts, then a success (brute force / probing).
- `ConsoleLogin` without MFA (`additionalEventData.MFAUsed = "No"`).
- Logins/actions from new or unusual IPs, geographies, or ASNs vs baseline.
- Impossible travel: same actor, two far-apart IPs within minutes.
- Access at unusual hours vs the actor's normal window.
```bash
jq -r '.Records[] | select(.errorCode=="AccessDenied") | [.eventTime,.userIdentity.arn,.eventName] | @tsv' <file>
```

### B. Privilege escalation
- `AttachRolePolicy`, `PutRolePolicy`, `PutUserPolicy`, `CreatePolicyVersion`, `AddUserToGroup`.
- New admin/wildcard policy (`"Action":"*"`, `"Resource":"*"`).
- `CreateAccessKey`, `CreateUser`, `CreateLoginProfile` (esp. for another principal).
- `iam:PassRole` to a higher-privilege role; new trust-policy edits (`UpdateAssumeRolePolicy`).
- Role/token used far beyond its normal scope of actions.

### C. Credential & secret access
- `GetSecretValue`, `Decrypt`, `GetParameter` (SecureString), `sts:AssumeRole*` — esp. new
  principals, bulk reads, or reads of secrets outside the actor's normal set.
- Any raw secret/token/key printed into the trail itself (exposure finding — redact + flag).
- OIDC/web-identity assumptions from unexpected issuers or projects.

### D. Data access / exfiltration
- Bulk `GetObject`/`ListObjects`, `Scan`, `SelectObjectContent`, large `Download*`.
- `PutBucketPolicy`/`PutBucketAcl` → public; `PutObjectAcl` public-read.
- New/unusual data egress: cross-account share, snapshot copy/share, replication config.
- Sudden volume spike vs baseline for a given actor/resource.

### E. Destructive / anti-forensic actions
- `Delete*`, `TerminateInstances`, `PutRetentionPolicy` (shortening), `DeleteLogGroup`.
- **CloudTrail tampering** — `StopLogging`, `DeleteTrail`, `UpdateTrail`, `PutEventSelectors`.
- **Config/Guard tampering** — disabling GuardDuty, Config, SecurityHub, WAF.
- KMS: `ScheduleKeyDeletion`, `DisableKey`, key-policy changes.

### F. Reconnaissance
- Enumeration bursts: `List*`/`Describe*`/`Get*` across many services from one actor in a short window.
- `GetCallerIdentity`, `GetAccountAuthorizationDetails`, `SimulatePrincipalPolicy`.

### G. CI/CD & supply-chain (for job logs / git history)
- Unexpected deploy targets, hardcoded creds in steps, `curl … | bash` from untrusted sources.
- New/renamed workflows or steps that assume roles or read secrets.
- Git: commits by unknown authors, force-pushes, backdated commits, secrets added then removed,
  large or off-hours history rewrites.

### H. Temporal & behavioural
- Off-hours activity, weekend spikes, machine-speed action bursts (automation misuse).
- Dormant principal suddenly active; service account doing interactive-style actions.
- Sequences that form a kill-chain (recon → escalate → access secret → exfil → delete logs).

## Step 3 — Correlate & score

1. Group related candidate events into **incidents** (same actor / same session / same window /
   same attack chain). One incident > many scattered lines.
2. Assign severity:
   - **Critical** — active compromise, exfil, log tampering, admin escalation, exposed live secret.
   - **High** — successful unauthorised access, privilege change, destructive action.
   - **Medium** — suspicious pattern needing human confirmation (unusual IP, off-hours, recon burst).
   - **Low** — hygiene/noise worth noting (no-MFA login, verbose denies).
3. Assign confidence (high/medium/low) from how directly the evidence supports the claim.
4. Drop anything you can't back with a quoted line.

## Step 4 — Report (fixed template)

Output exactly this structure. Markdown.

```markdown
# Audit Trail Analysis

## Scope
- Source: <file/paste>  ·  Format: <detected format>  ·  Events: <count>
- Time range: <first> → <last>
- Actors: <n distinct>  ·  Source IPs: <n distinct>

## Verdict
<one line: "No suspicious activity detected" | "N findings (X critical, Y high, …) — see below">

## Findings
### [SEV] <short title>  (confidence: <high/med/low>)
- **What the log shows:** <fact>
- **Evidence:**
  ```
  <exact quoted line(s), timestamps included>
  ```
- **Why it's suspicious:** <inference, labelled as interpretation>
- **Actor / IP / window:** <who, where, when>
- **Recommended action:** <specific next step: revoke key X, isolate role Y, contact Z, deeper query>
<repeat per finding, ordered Critical → Low>

## Attack-chain (only if events correlate into a sequence)
1. <ts> recon → 2. <ts> escalate → 3. <ts> secret read → 4. <ts> exfil → 5. <ts> log-delete

## Reviewed & cleared
<one or two lines: notable-but-benign patterns you checked and dismissed, so the reader knows they were considered>

## Gaps / caveats
- Missing fields, truncated log, no baseline, or data the trail can't answer. Suggest what extra log source would confirm.
```

## Step 5 — Handoff

- If any finding is **Critical**, say so in the first line and list the immediate containment
  steps (revoke credential, disable key, isolate principal, preserve logs) before anything else.
- Offer the exact follow-up queries (jq/grep) a responder can run to pivot on the top finding.
- Do not open tickets, message anyone, or change infra unless explicitly asked — this skill
  ends at the report.

## When stuck
- Format unrecognised → normalise to the 5-field shape from the header/first rows and proceed.
- Too big to read → extract with `jq`/`grep`/`awk` first, reason over extracts, state the sampling.
- No baseline available → say so in Gaps; downgrade "unusual vs baseline" findings to Medium/low confidence.
