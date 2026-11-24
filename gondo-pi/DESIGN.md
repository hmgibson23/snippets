# gondo-pi design

Goal: the agent (pi and everything it runs, including a compromised dependency)
never holds a credential, yet the user just types the command and it works.
"Seamless" is the requirement; "locked down" is the constraint. When they
conflict, the answer is a narrowly scoped, time-boxed grant, not a prompt per use.

## The experience

```
$ pis                       # in any project
```

- Starts (or reattaches to) zellij session `pi-<project>` and drops you in fish
  inside a microVM at `/workspace` (the project); `pi` when you want it. Tabs:
  shell / gondo-pi (the control panel). Every new pane is another shell in the
  same VM. `pis --no-zellij`: the same shell in the current terminal, requests
  answered with `gondo-pi panel` elsewhere.
- First credential use of the day: one keychain unlock. That's it.
- Inside, everything just works with no credential handling by agent or user:
  `git clone/pull/push`, `git commit` (auto-signed), `gh pr create`,
  `aws ... / terraform`, `npm install`, `pi` on Bedrock/Copilot.
- The only prompts during a normal day:
  - first push to a repo (then silent for 4h),
  - an AWS account not picked at launch (then 4h), plus the Okta code when expired,
  - a host not on the allowlist,
  - interactive `ssh <host>` (every time),
  - a brand new API token (once, then stored).
- Prompts are one keystroke (`enter` = the default scope) and wait in the
  gondo-pi tab (Alt-2) or `gondo-pi panel`; never a pop-up while a panel is open,
  never a VM pane.
- Reconnect: `pis` again reattaches; detaching keeps the session.

## Tiers

| | Trusted tier | Agent tier |
|---|---|---|
| where | macOS host / Linux host (needs a hypervisor: hvf, KVM) | Gondolin microVM |
| runs | supervisor, vmd + network policy, prompts, keychain, saml2aws, private ssh-agent, zellij | your shells, pi, all tools |
| holds credentials | yes | never: no tokens, keys, agent sockets, AWS keys |

## The VM

One VM per session, owned by `vm/vmd.mjs` on the trusted side. pty sessions
(fish shells, pi, commands) are multiplexed over `RUN_DIR/vmd.sock` (0600 in a
0700 dir, never mounted into the guest). zellij panes are thin clients
(`lib/gondopi/attach.py`); `--default-shell` makes every new pane a VM shell. The
main shell pane hosts the supervisor + `VmHost`; exiting it ends the session.

`~/.pi` is mounted; its symlinked entries (extensions, skills, models.json ->
`~/snippets/...`) are mounted at the paths the links resolve to. `~/.pi/agent/npm`
is overlaid with a Linux build of the same lockfile (`npm ci` in the guest,
cached in `~/.local/state/gondo-pi/vm-pi-npm`), since the host copy has native
binaries for the host OS. The guest `pi` wrapper waits for it on first use.

The rootfs is copy-on-write and discarded at exit. Persistent: `/workspace` (the
project), `~/.pi` (pi state), and a per-project tool cache
(`~/.local/state/gondo-pi/projects/<p>/vm-cache` -> `/gondo-pi-cache`: mise, uv,
npm, cargo, go, zig). Images: macOS arm64 = Alpine (`vm/image-arm64.json`);
Linux x86_64 = Arch (`vm/Containerfile.arch` built rootless with podman, wrapped
by `vm/image-x86_64-arch.json`).

Your shell and pi share the guest: the guest is untrusted as a whole, so a human
typing a secret into it leaks it to pi. Secrets go only to the panel.

### Guest egress
- **HTTP(S)**: Gondolin's userspace network stack terminates TLS with its own CA
  (trusted by the guest) and hands every request to `vm/netpolicy.mjs`:
  - host allowlist, else `net-approve` to the supervisor (panel), cached;
  - credential routes: the guest's per-session placeholder in the route's header
    is replaced with the real token, only on the route's host/paths; any other
    token (e.g. pi's Copilot login) passes through untouched;
  - AWS: the guest's `[default]` profile carries a "current account" key that
    vmd resolves per request (`aws-current`); `aws-use` / the panel switch it on
    the host (from the VM: needs the panel's OK once per window). Requests
    signed with a placeholder key are re-signed (SigV4,
    `@smithy/signature-v4`) with the real STS session from the supervisor;
    aws-chunked bodies are decoded and signed whole; `AssumeRole*`,
    `GetSessionToken`, `GetFederationToken`, ECR `GetAuthorizationToken`,
    `CreateAccessKey` are refused.
- **Broker**: one host-mapped TCP port to the supervisor (token-authenticated).
- **ssh**: Gondolin's host-side ssh proxy to git hosts, using the supervisor's
  private agent; every exec is checked by the supervisor (`vm-ssh-exec`).
- Nothing else: raw TCP/UDP to anywhere else gets no upstream.

New credential route or allowlist edit = policy reload, same VM (pi restarts
itself with `--continue` to pick up the new env var). New mount = recreated VM
(`vm-apply`, ~1s; panes reconnect fresh). Placeholders are stable per session.

## Supervisor

Long-running parent of the session. Owns:

- **Grant store**: `(kind, resource, scope, expires)`, e.g.
  `git-push:github.com/newsuk/donk 4h`, `aws:nuk-cdn-dev 4h`,
  `net:registry.npmjs.org always(project)`, `ssh:linux once`.
  Per-project persistence for `always`; session grants in memory.
- **Prompt service**: in zellij, a queue shown in the gondo-pi tab (the bar pill
  turns yellow); outside zellij, /dev/tty, else an OS dialog. Facts come from
  the broker (host, repo, operation, account); agent-supplied reasons are shown
  separately and labelled.
- **Audit log**: every credential use and decision (append-only, per session).
- **Private ssh-agent**: not the login agent. Keys loaded only while a grant is
  live (`ssh-add -t <remaining>`), removed at expiry.
- **Dedicated keystore**: macOS `gondo-pi.keychain` (Security.framework, no UI);
  Linux Secret Service (`secret-tool`). API tokens, Okta password, AWS session
  cache, ssh key passphrases. Auto-locks after the window (default 4h).

## Brokers

| Need | Path | Agent sees |
|---|---|---|
| API token | netpolicy route → `route-secret` → supervisor → keystore | placeholder |
| GitHub API | same, token from host `gh auth token` | placeholder |
| AWS | netpolicy SigV4 re-sign → `aws-creds` → supervisor → saml2aws (`--prompter pinentry-gondo-pi`) | placeholder keys |
| new host | netpolicy → `net-approve` → supervisor (panel) | allowed / 403 |
| git over ssh | Gondolin ssh egress + `execPolicy` → supervisor; host ssh with the private agent | nothing |
| commit signing | `gpg.ssh.program` shim → broker → `ssh-keygen -Y sign -n git` | signature |
| interactive ssh | `ssh` shim → broker, always prompts | the remote shell |

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
  audited. Pushing still needs a grant.

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
| secrets | keystore unlock once per window |

Windows are configurable per kind in `~/.config/gondo-pi/config.json`.

## Prompt integrity
- The guest can't reach zellij, the supervisor's ctl socket, vmd's socket, the
  private agent or the keystore: none of them exist in the VM.
- Secrets are never typed into a VM pane.
- Dedup identical pending requests; "deny new requests for 10 min" panic key.

## Threat model

Stops: credential reads (nothing present); silent key/token use; use outside a
grant's scope or after expiry; sending a placeholder-bound token to any other
host; exfiltration to non-allowlisted hosts; minting AWS credentials.

Does not stop: misuse inside a live grant (e.g. a signed push to an approved
repo within 4h — mitigate with branch protection / shorter windows / audit);
data sent to allowlisted hosts; secrets in allowed AWS responses; pi's own LLM
tokens (`~/.pi/agent/auth.json`, mounted by choice for now).
