# nono-pi design

Goal: the agent (pi and everything it runs, including a compromised dependency)
never holds a credential, yet the user just types the command and it works.
"Seamless" is the requirement; "locked down" is the constraint. When they
conflict, the answer is a narrowly scoped, time-boxed grant, not a prompt per use.

## The experience

```
$ pis                       # in any project
```

- Starts (or reattaches to) zellij session `pi-<project>`: pi in the main pane,
  a one-line status pane (AWS account, active grants, time left).
- First run of the day: one keychain unlock (Touch ID / password). That's it.
- Inside, everything just works with no credential handling by agent or user:
  `git clone/pull/push`, `git commit` (auto-signed), `gh pr create`,
  `aws ... / terraform`, `npm install`, `pi` on Bedrock/Copilot.
- The only prompts during a normal day:
  - first push to a repo (then silent for 4h),
  - an AWS account not picked at launch (then 4h), plus Okta code when expired,
  - a host not on the allowlist,
  - interactive `ssh <host>` (every time),
  - a brand new API token (once, then stored).
- Prompts are one keystroke (`enter` = the default scope) and wait in the
  nono-pi zellij tab (Alt-2); never a pop-up, never the pi pane.
- Reconnect: `pis` again reattaches; closing the terminal keeps the session.

## Tiers

| | Trusted tier | Agent tier |
|---|---|---|
| where | macOS host; on Linux the Arch devbox (shares creds with the desktop) | inside the trusted tier |
| runs | supervisor, nono proxy, brokers, prompts, keychain, saml2aws, private ssh-agent, zellij | pi + all tools |
| backend | — | Gondolin microVM devbox (default; nested inside the devbox on Linux) or `--nono` (Seatbelt/Landlock on the host) |
| holds credentials | yes | never: no tokens, keys, agent sockets, AWS keys |

## VM devbox (default since 2026-11-23)

One VM per session, owned by `vm/vmd.mjs` on the trusted side; pty sessions
(fish shells, pi, commands) are multiplexed over `RUN_DIR/vmd.sock` (0600 in a
0700 dir, never mounted into the guest). zellij panes are thin clients
(`lib/nonopi/attach.py`); `--default-shell` makes every new pane a VM shell. The
main shell pane hosts the supervisor + VmHost; exiting it ends the session.
rootfs is copy-on-write and discarded: only `/workspace` and `~/.pi` persist.
Your shell and pi share the guest: the guest is untrusted as a whole, so a
human typing a secret into it leaks it to pi - secrets go only to the panel.
New access (credential route, mount) = new proxy profile + recreated VM
(`vm-apply`); panes reconnect, pi resumes with `--continue`.

## Supervisor

Long-running parent of the session (replaces today's exec-into-nono). Owns:

- **Grant store**: `(kind, resource, scope, expires)`, e.g.
  `git:push:github.com/newsuk/donk 4h`, `aws:nuk-cdn-dev 4h`,
  `net:registry.npmjs.org always(project)`, `ssh:linux once`.
  Per-project persistence for `always`; session grants in memory.
- **Prompt service**: in zellij, a queue shown in the nono-pi tab (the bar pill
  turns yellow); outside zellij, /dev/tty, else an OS dialog. Facts come from
  the broker (host, repo, operation, account); agent-supplied reasons are shown
  separately and labelled.
- **Audit log**: every credential use and decision (append-only, per session).
- **Private ssh-agent**: not the login agent. Keys loaded only while a grant is
  live (`ssh-add -t <remaining>`), removed at expiry.
- **Dedicated keychain** `nono-pi.keychain`: API tokens, Okta password, AWS
  session cache, and the ssh private keys themselves (the keys have no
  passphrase, so moving them in is what makes them unlock-gated). Auto-locks
  after the grant window (default 4h) and on sleep. Linux: devbox keystore TBD.
- Restart-and-resume of the agent tier when launch-time config must change.

## Brokers

| Need | Path | Agent sees |
|---|---|---|
| API token | nono proxy `cmd://` route → helper → supervisor → keychain | phantom token |
| AWS | nono proxy SigV4 (`aws_auth` profiles) → `credential_process` → supervisor → saml2aws | placeholder keys |
| new host | nono `approval_backends` webhook (unix socket) → supervisor (`allow` / `allow_session` / `deny`) | allowed / refused |
| git over ssh | `GIT_SSH_COMMAND` shim → supervisor socket → host `ssh` with private agent (nono mode); Gondolin ssh egress + `execPolicy` → supervisor (VM mode) | nothing |
| commit signing | `gpg.ssh.program` shim (via `GIT_CONFIG_*` env) → supervisor → `ssh-keygen -Y sign -n git` | signature |
| interactive ssh | same broker as git, always prompts | the remote shell |

### git broker rules
- Only `git-upload-pack` / `git-receive-pack` / `git-upload-archive` + one repo
  path; anything else is refused (no arbitrary remote commands).
- Fetch/clone/archive from any repo: auto (no prompt).
- Push: grant per `host/org/repo`, default 4h (configurable).
- Host keys verified on the host (`~/.ssh/known_hosts`).

### signing broker rules
- Payload must parse as a git commit or tag object; signs with namespace `git`
  only (so a signature can never be an ssh login signature).
- Automatic: every `git commit` is signed without a prompt; each signature is
  audited and shown in the status pane. Pushing still needs a grant.

## Grant defaults

| Action | Default |
|---|---|
| commit signing | auto, audited |
| fetch / clone (any repo) | auto |
| push | prompt per repo → 4h |
| AWS: accounts picked at launch | auto |
| AWS: other accounts | prompt → 4h (+ Okta password/code via prompts) |
| allowlisted hosts | auto |
| new host | prompt → once / 4h / always (project) |
| interactive ssh | prompt every time |
| secrets | keychain unlock once per window |

Windows are configurable per kind in `~/.config/nono-pi/config.json`.

## Prompt integrity
- The agent cannot drive zellij (socket unreachable from the sandbox; macOS
  Seatbelt, Linux af_unix_mediation) or run dialog tools (osascript/zenity denied).
- Secrets are never typed into the pi pane.
- Dedup identical pending requests; "deny new requests for 10 min" panic key.

## Threat model

Stops: credential reads (nothing present); silent key/token use; use outside a
grant's scope or after expiry; exfiltration to non-allowlisted hosts; minting
AWS credentials (`AssumeRole*`, `GetSessionToken`, ECR auth token denied).

Does not stop: misuse inside a live grant (e.g. a signed push to an approved
repo within 4h — mitigate with branch protection / shorter windows / audit);
data sent to allowlisted hosts; secrets in allowed AWS responses; pi's own LLM
tokens (`~/.pi/agent/auth.json`, readable by choice for now).

## Phases
1. Supervisor core + brokers (nono mode): grants, prompts (dialog), audit,
   keychain, private agent, git + signing brokers, network approvals, AWS and
   secrets through the supervisor, launch picker.
2. zellij: always-on session, requests in the nono-pi tab, zjstatus bar pill, reattach.
3. Credential requests: agent-filed + `/secret`, restart + resume.
4. VM mode (Gondolin) on the same brokers.
5. Linux devbox: nested KVM, keystore.

## Phase 4 spike results (2026-11-23, gondolin 0.14.0, macOS M3 Max)
- Boots via npx; krun and qemu backends both work; ~2-3s warm start.
- VM -> `--tcp-map` -> host `nono proxy` works: raw CONNECT, credential
  injection, CA from `nono proxy` (`--proxy-ca-cert/key` for a stable CA).
- Deny-all other egress: `createHttpHooks({allowedHosts: []})` -> SDK only
  (the node CLI has no empty-allowlist / hooks flag), so `--vm` drives the SDK.
- Workspace mount rw, fast enough (git status 0.16s warm on 812 files).
- Tooling image via `gondolin build` (~25s): node 24, git, aws-cli v2, gh, rg,
  python3, openssh. terraform not in Alpine 3.23 -> postBuild download or opentofu.
- ssh egress with host agent works, exec-only (git); interactive ssh refused.
  For our design the agent becomes the supervisor's private agent and the
  `execPolicy` callback asks the supervisor (push grants).
- pi runs inside the VM (pi on host would be unsandboxed).
- Linux: needs rw /dev/kvm in the devbox for krun, else QEMU TCG (slow).
Full notes: ~/gondolin-spike/NOTES.md
