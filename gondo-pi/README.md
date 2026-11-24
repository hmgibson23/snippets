# gondo-pi

`pis` = an instant, fully isolated Linux devbox for the current project: a
[Gondolin](https://github.com/earendil-works/gondolin) microVM with your fish
config, pi already running next to your shell, and no secrets inside. git, gh,
aws, npm etc. still just work: credentials stay on the trusted side (this Mac, or
a Linux box) behind brokers that ask you once per scoped, time-limited grant.
Needs a hypervisor (macOS: built in; Linux: `/dev/kvm`), node, python3, zellij.

```sh
./install.sh          # links gondo-pi + gondo-pi-key, alias `pis`
gondo-pi setup         # one-time: gondo-pi keychain, ssh key passphrase
cd ~/some/project && pis
```

## What it feels like

- `pis` opens (or reattaches to) zellij session `pi-<project>` and drops you in
  **fish inside the VM** at `/workspace` (= the project). Type `pi` when you want
  it (your extensions, skills and models come along). Tabs: **shell** and
  **gondo-pi** (the control panel, Alt-2). Every new pane or tab is another shell
  in the same VM. The VM is throwaway: installed packages, `/tmp` etc. vanish
  when the session ends; the project, `~/.pi` and tool caches persist. Exit the
  first shell (or Ctrl-D) and the session closes and cleans up.
- No zellij: `pis --no-zellij` (or `"zellij": false` in the config) gives you the
  same VM shell in the current terminal; answer requests with `gondo-pi panel` in
  another terminal (no panel open: a desktop dialog after 20s).
- Your shell and pi share the VM, so treat it like pi's machine: never paste a
  real token into it - type secrets only in the gondo-pi tab.
- First credential use of the day: unlock the gondo-pi keychain once (4h).
- Then these just work, no tokens anywhere in the sandbox:
  `git clone/pull/push`, `git commit` (auto-signed), `gh …`, `aws … --profile X`,
  `terraform`, `npm`/`uv`/`cargo`, pi on Copilot/Bedrock/anything.
- Nothing ever pops up over your work. When something needs you (a push, a new
  host, an AWS account, the Okta password / 6-digit code, an API token) it waits
  in the **gondo-pi** tab and the bar pill turns yellow: `⚠ 1 waiting · Alt-2`.
  Alt-2 → Enter (default scope) / a number / `d` deny; secrets are typed into a
  hidden-input box right there. Alt-1 back.
- Asked only for: first push to a repo (→ 4h), an AWS account you didn't
  pre-pick (→ 4h, plus Okta password/6-digit code when the session expired), a
  host not on the allowlist, interactive `ssh`, a new API credential.
- Choices (AWS accounts) are remembered per project; `pis --configure` re-picks.
- It's your normal zellij (your keybinds, theme) with the same zjstatus bar as
  your own sessions (`~/.config/zellij/parts/zjstatus-bar.kdl`): gondo-pi adds one
  pill on the right (🔓/🔒 keychain · AWS account ✓time · ⇡ push grants) that turns
  yellow with "N waiting → tab gondo-pi" when something needs you. Detach (session
  mode → `d`) to leave it running and `pis` again to reattach.
- First `pi` after a change to your pi packages: the VM installs them for Linux
  (~30s, cached; the host's copies have macOS-only native binaries).

## AWS: pick an account on demand

No picker at launch. The whole VM (plain `aws`, terraform, SDKs, pi) uses one
**current account**, switched on demand:

- **Alt-a** anywhere in the session: a floating fzf picker (✓ = live session, PROD tagged).
- `aws-use` in any VM shell (same picker), `aws-use <account>` (tab-completes),
  `aws-use --current`. In pi: `/aws use [account]`.
- gondo-pi tab → AWS → Enter (`l` log in only, `R` re-login, `o` log out).

Switching logs in if needed: the Okta password and 6-digit code are asked in the
gondo-pi tab. A switch started inside the VM also needs one Enter there per 4h
window (the VM can't prove it was you and not pi); switches in the panel don't.
`aws --profile <account>` still targets one call at a specific account (first use
asks too). The bar shows `☁ <account> ✓48m`. `pis --aws a,b` pre-approves accounts
and makes `a` current from the start.

## Control panel (tab `gondo-pi`)

| Tab | Keys |
|---|---|
| Requests | Enter = allow (default scope) · 1-5 = pick · d = deny |
| Grants | x = revoke |
| Network | a = add host to the allowlist (takes effect immediately) · x = remove |
| AWS | Enter = use (switch the VM's account, logs in if needed) · l = log in only · R = re-login · o = log out (● = current, ✓ = session time left) |
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
| `pis --no-zellij` | the VM shell right here; requests via `gondo-pi panel` |
| `pis -- <pi args>` | just pi in the VM, no zellij (scripts: `pis -- -p "…"`) |
| `gondo-pi panel` | the control panel in any terminal |
| `pis --aws a,b` | pre-approve AWS accounts; `a` is current from the start |
| `aws-use [account]` (in the VM), Alt-a | switch the VM's AWS account (fzf picker) |
| `gondo-pi --print-policy` | the generated network/credential policy (placeholders only) |
| `pis --configure` | re-pick remembered choices |
| `gondo-pi setup` | create keychain, migrate secrets, passphrase ssh keys |
| `gondo-pi-key ls / set N / rm N` | manage stored secrets (host terminal) |
| `gondo-pi-key status / revoke KEY` | live session grants |
| `gondo-pi-key aws-refresh` | import every AWS account/role from `saml2aws list-roles` |
| `gondo-pi doctor` | check the setup (hypervisor, image, patches, keychain, gh, ssh key on GitHub, ...) and how to fix it |
| `gondo-pi gc [--yes]` | disk used by images, caches, builds; `--yes` prunes the stale parts |
| `gondo-pi image rebuild` | rebuild the VM image (sessions warn when it's out of date) |
| in the VM: `gondo-pi forward PORT`, `gondo-pi ports` | open a VM port on your machine (127.0.0.1; asks in the panel) |
| in the VM: `open URL` / `xdg-open URL` / `$BROWSER` | open a link in your browser on the host (asks in the panel) |
| in the VM: `pbcopy` / `wl-copy` / `xclip`, nvim `"+y` | copy to your host's clipboard (OSC 52) |
| in pi: `/grants`, `/grants revoke KEY`, `/aws`, `/aws use [ACCT]`, `/aws login ACCT`, `/secret NAME https://host [Header] [format]` | |

pi can also file requests itself (`gondo_request` tool): a new API credential
(you approve the exact host + header and type the value), read/write access to
another directory, or an AWS login. Granting restarts pi with the same session
(a new directory also restarts the VM: open shells reopen fresh).

## How credentials flow

| Need | Broker | Inside sees |
|---|---|---|
| API tokens | VM network policy swaps the placeholder → supervisor → gondo-pi keychain | placeholder token |
| GitHub API (`gh`) | same, token from host `gh auth token` | placeholder |
| AWS | VM network policy re-signs SigV4 → supervisor → saml2aws (`--prompter pinentry-gondo-pi`) | placeholder keys |
| new hosts | VM network policy → supervisor → gondo-pi tab | allowed / 403 |
| git over ssh | Gondolin ssh proxy + exec policy → supervisor; host ssh with a private, time-limited agent | nothing |
| commit signing | `gpg.ssh.program` shim → supervisor (`-n git`, commit/tag objects only) | the signature |

Inside the VM there is no supervisor control socket, private agent, zellij,
keychain, `~/.ssh` key, `~/.aws` or gondo-pi host config: only the project,
`~/.pi` and a tool cache are mounted. A placeholder is only ever swapped on its
own host, so sending `$GH_TOKEN` anywhere else sends a worthless string.

## In the VM

- **Your neovim**: your config (read-only from the host, a writable copy in the VM), plugins,
  Mason tools and parsers built for Linux, seeded from your host's plugin checkouts and kept
  as one tarball on the host (warm on the next start). `"+y` copies to the host clipboard.
- **Ports**: a dev server listening in the VM shows up in the panel's **Ports** tab: Enter
  opens it on `http://127.0.0.1:<port>` on your machine (never other interfaces), `x` closes
  it, `a` adds one by number. `gondo-pi forward 3000` from the VM asks first.
- **Links**: `gh pr view --web`, OAuth logins etc. open in your browser on the host after you
  OK the host in the panel; an OAuth redirect back to `localhost:PORT` gets that port forwarded.
- **Notifications**: a request that waits 30s while you're not looking at the panel gives a
  desktop notification (Linux: notify-send, "Open" focuses the terminal in niri and switches
  to the gondo-pi tab; macOS: Notification Center). `GONDO_PI_NOTIFY=0` turns it off.
- **Shell history** and zoxide are kept per project.

## Per-project settings

`~/.config/gondo-pi/projects/<project-id>.json` (yours) and `.gondo-pi.json` in the repo
(shareable; writable from the VM, so it only applies after you approve its exact content, and
every change asks again). Keys: `allow` (extra hosts), `aws` (current account at start),
`ports` (forward when they start listening), `vm` (`memory`, `cpus`). Your file wins.

## Config: `~/.config/gondo-pi/config.json`

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
sessions over `~/.local/state/gondo-pi/run/vmd.sock` (host-only); each zellij pane
is a thin client (`gondo-pi --vm-shell`, zellij's default shell for the session).
`vm/netpolicy.mjs` is the VM's only HTTP(S) exit (allowlist, approvals,
credential swap, AWS re-signing); git-over-ssh goes through Gondolin's host-side
proxy with the supervisor deciding each exec. Your fish config is copied in
(minus host-only plugins) with a `⬡ vm` prompt tag. The system disk is thrown
away each session; tool caches (mise, uv, npm, cargo, go, zig) persist per
project. A granted new credential or directory recreates the VM in ~1s: shells
reopen fresh, pi resumes the same conversation.

## What the VM can't touch on the host

- pi's code and config (`~/.pi`, extensions, skills, `models.json`, settings): read-only, copied
  into the VM. Only pi's sessions / data dirs are written back; `auth.json` (refreshed OAuth
  tokens) goes back through the supervisor, merged per provider (newer expiry wins).
- Files in the project that make *host* tools run commands: dangerous `.git/config` keys
  (`core.fsmonitor`, `core.hooksPath`, filters, aliases, `includeIf`, ...) are reverted, new git
  hooks are moved to `.git/gondo-pi-quarantine/`, changes to `.envrc`, `.mise.toml`,
  `.vscode/tasks.json`, `Makefile` ... are reported in the panel's log.
- With your GitHub token: no account/org/repo admin (keys, collaborators, hooks, secrets,
  branch protection, delete/transfer); repo content writes need the push grant; gists and new
  repos ask. Tokens or AWS keys that are *not* yours (brought into the VM) ask before use.

## Limits

- Inside a live grant a malicious dependency can act within it (e.g. push a
  signed commit to a repo you approved in the last 4h). Use branch protection,
  shorter windows (`grants.push`), and the audit log (`~/.local/state/gondo-pi/audit`).
- Allowed hosts / allowed AWS responses can carry data out or secrets in
  (e.g. `secretsmanager get-secret-value`).
- Only HTTPS secrets are injectable; a tool needing a raw secret in a file/env can't be served without exposing it.
- pi's own LLM logins (`~/.pi/agent/auth.json`) are readable inside (by choice).
- One gondo-pi session at a time. Linux keystore: Secret Service if present, else in-memory per session.

See DESIGN.md for the full design and threat model.
