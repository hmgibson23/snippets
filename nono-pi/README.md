# nono-pi

`pis` = an instant, fully isolated Linux devbox for the current project: a
[Gondolin](https://github.com/earendil-works/gondolin) microVM with your fish
config, pi already running next to your shell, and no secrets inside. git, gh,
aws, npm etc. still just work: credentials stay on the trusted side (this Mac, or
a Linux box) behind brokers that ask you once per scoped, time-limited grant.
Needs a hypervisor (macOS: built in; Linux: `/dev/kvm`), node, python3, zellij.

```sh
./install.sh          # links nono-pi + nono-pi-key, alias `pis`
nono-pi setup         # one-time: nono-pi keychain, ssh key passphrase
cd ~/some/project && pis
```

## What it feels like

- `pis` opens (or reattaches to) zellij session `pi-<project>` and drops you in
  **fish inside the VM** at `/workspace` (= the project). Type `pi` when you want
  it (your extensions, skills and models come along). Tabs: **shell** and
  **nono-pi** (the control panel, Alt-2). Every new pane or tab is another shell
  in the same VM. The VM is throwaway: installed packages, `/tmp` etc. vanish
  when the session ends; the project, `~/.pi` and tool caches persist. Exit the
  first shell (or Ctrl-D) and the session closes and cleans up.
- No zellij: `pis --no-zellij` (or `"zellij": false` in the config) gives you the
  same VM shell in the current terminal; answer requests with `nono-pi panel` in
  another terminal (no panel open: a desktop dialog after 20s).
- Your shell and pi share the VM, so treat it like pi's machine: never paste a
  real token into it - type secrets only in the nono-pi tab.
- First credential use of the day: unlock the nono-pi keychain once (4h).
- Then these just work, no tokens anywhere in the sandbox:
  `git clone/pull/push`, `git commit` (auto-signed), `gh …`, `aws … --profile X`,
  `terraform`, `npm`/`uv`/`cargo`, pi on Copilot/Bedrock/anything.
- Nothing ever pops up over your work. When something needs you (a push, a new
  host, an AWS account, the Okta password / 6-digit code, an API token) it waits
  in the **nono-pi** tab and the bar pill turns yellow: `⚠ 1 waiting · Alt-2`.
  Alt-2 → Enter (default scope) / a number / `d` deny; secrets are typed into a
  hidden-input box right there. Alt-1 back.
- Asked only for: first push to a repo (→ 4h), an AWS account you didn't
  pre-pick (→ 4h, plus Okta password/6-digit code when the session expired), a
  host not on the allowlist, interactive `ssh`, a new API credential.
- Choices (AWS accounts) are remembered per project; `pis --configure` re-picks.
- It's your normal zellij (your keybinds, theme) with the same zjstatus bar as
  your own sessions (`~/.config/zellij/parts/zjstatus-bar.kdl`): nono-pi adds one
  pill on the right (🔓/🔒 keychain · AWS account ✓time · ⇡ push grants) that turns
  yellow with "N waiting → tab nono-pi" when something needs you. Detach (session
  mode → `d`) to leave it running and `pis` again to reattach.
- First `pi` after a change to your pi packages: the VM installs them for Linux
  (~30s, cached; the host's copies have macOS-only native binaries).

## Logging in to AWS

Nothing to do up front: the first `aws …` the agent runs logs in by itself
(prompts for the Okta password once, then the 6-digit code when Okta asks).
To log in ahead of time or switch accounts:

- **nono-pi tab → AWS** → select the account → Enter (`R` re-login, `o` log out).
  The password / code prompts appear right there under *Requests*.
- in pi: `/aws login [account]` or the old `/saml2aws-login [account]`.
- any host terminal: `nono-pi-key aws-login [account]`.

The status line shows `AWS nuk-digital-dev-architecture ✓58m` while a session is live.
Inside nono-pi the old `aws_exec` tool is disabled: the agent just runs `aws`.

## Control panel (tab `nono-pi`)

| Tab | Keys |
|---|---|
| Requests | Enter = allow (default scope) · 1-5 = pick · d = deny |
| Grants | x = revoke |
| Network | a = add host to the allowlist (takes effect immediately) · x = remove |
| AWS | Enter/l = log in · R = re-login · o = log out · p = pre-approve (shows ✓ session time left) |
| Secrets | s = add/replace (hidden input) · x = delete |
| Log | recent decisions and credential use |
| any | ←/→ tabs · u = unlock keychain · L = lock now (+ drop ssh key) · P = deny all new requests 10 min · q = back to pi |

The panel is a trusted process (like the supervisor): the agent can reach
neither zellij's socket nor the supervisor's control socket, so it cannot
answer, add, or revoke anything here.

## Commands

| | |
|---|---|
| `pis` | start / reattach this project's session (zellij) |
| `pis --no-zellij` | the VM shell right here; requests via `nono-pi panel` |
| `pis -- <pi args>` | just pi in the VM, no zellij (scripts: `pis -- -p "…"`) |
| `nono-pi panel` | the control panel in any terminal |
| `pis --aws a,b` / `--aws none` | pre-approve AWS accounts (others ask on first use) |
| `nono-pi --print-policy` | the generated network/credential policy (placeholders only) |
| `pis --configure` | re-pick remembered choices |
| `nono-pi setup` | create keychain, migrate secrets, passphrase ssh keys |
| `nono-pi-key ls / set N / rm N` | manage stored secrets (host terminal) |
| `nono-pi-key status / revoke KEY` | live session grants |
| `nono-pi-key aws-refresh` | import every AWS account/role from `saml2aws list-roles` |
| in pi: `/grants`, `/grants revoke KEY`, `/aws`, `/aws login ACCT`, `/secret NAME https://host [Header] [format]` | |

pi can also file requests itself (`nono_request` tool): a new API credential
(you approve the exact host + header and type the value), read/write access to
another directory, or an AWS login. Granting restarts pi with the same session
(a new directory also restarts the VM: open shells reopen fresh).

## How credentials flow

| Need | Broker | Inside sees |
|---|---|---|
| API tokens | VM network policy swaps the placeholder → supervisor → nono-pi keychain | placeholder token |
| GitHub API (`gh`) | same, token from host `gh auth token` | placeholder |
| AWS | VM network policy re-signs SigV4 → supervisor → saml2aws (`--prompter pinentry-nono-pi`) | placeholder keys |
| new hosts | VM network policy → supervisor → nono-pi tab | allowed / 403 |
| git over ssh | Gondolin ssh proxy + exec policy → supervisor; host ssh with a private, time-limited agent | nothing |
| commit signing | `gpg.ssh.program` shim → supervisor (`-n git`, commit/tag objects only) | the signature |

Inside the VM there is no supervisor control socket, private agent, zellij,
keychain, `~/.ssh` key, `~/.aws` or nono-pi host config: only the project,
`~/.pi` and a tool cache are mounted. A placeholder is only ever swapped on its
own host, so sending `$GH_TOKEN` anywhere else sends a worthless string.

## Config: `~/.config/nono-pi/config.json`

Created on first run (AWS accounts from saml2aws state). Main knobs:
`services` (API routes: `upstream`, `env_var`, `format`, `header`, `secret`,
`paths`), `aws.accounts / prod_accounts / services`, `network.allow` (+ `ask`),
`grants.push|aws|net` (seconds, default 4h), `keychain.lock_after`, `ssh.keys`,
`zellij`, `extra_read` / `extra_allow`.

## The VM

Images: macOS = Alpine (`vm/image-arm64.json`); Linux x86_64 = Arch
(`vm/Containerfile.arch`, built rootless with podman). Both: fish, neovim/vim,
tmux, git, gh, aws v2, terraform, node + pi, python/uv, go, rust, build tools,
rg/fd/fzf/zoxide/bat/eza/jq/delta (Arch adds zig, clang, cmake, ninja, mise,
just, direnv, btop). Built on first use. Half the host's RAM (≤8G) and CPUs
(≤6); override with `"vm": {"memory": "4G", "cpus": 4}`. Boots in ~2s.

`vm/vmd.mjs` (trusted side) owns the one VM for the session and serves pty
sessions over `~/.local/state/nono-pi/run/vmd.sock` (host-only); each zellij pane
is a thin client (`nono-pi --vm-shell`, zellij's default shell for the session).
`vm/netpolicy.mjs` is the VM's only HTTP(S) exit (allowlist, approvals,
credential swap, AWS re-signing); git-over-ssh goes through Gondolin's host-side
proxy with the supervisor deciding each exec. Your fish config is copied in
(minus host-only plugins) with a `⬡ vm` prompt tag. The system disk is thrown
away each session; tool caches (mise, uv, npm, cargo, go, zig) persist per
project. A granted new credential or directory recreates the VM in ~1s: shells
reopen fresh, pi resumes the same conversation.

## Limits

- Inside a live grant a malicious dependency can act within it (e.g. push a
  signed commit to a repo you approved in the last 4h). Use branch protection,
  shorter windows (`grants.push`), and the audit log (`~/.local/state/nono-pi/audit`).
- Allowed hosts / allowed AWS responses can carry data out or secrets in
  (e.g. `secretsmanager get-secret-value`).
- Only HTTPS secrets are injectable; a tool needing a raw secret in a file/env can't be served without exposing it.
- pi's own LLM logins (`~/.pi/agent/auth.json`) are readable inside (by choice).
- One nono-pi session at a time. Linux keystore: Secret Service if present, else in-memory per session.

See DESIGN.md for the full design and threat model.
