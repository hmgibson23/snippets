#!/bin/bash
# End-to-end in a real microVM with an isolated gondo-pi state + memory keystore.
# Signing + the ssh broker use a throwaway key (no passphrase, in the temp dir), loaded
# only into the supervisor's private agent on the host. git over ssh to GitHub is then
# refused by GitHub (key unknown) - that still proves the broker path end to end.
#   tests/e2e_vm.sh
set -u
T=$(mktemp -d "$HOME/.gondo-pi-e2e.XXXX"); chmod 700 "$T"
export GONDO_PI_STATE="$T/state" GONDO_PI_CONFIG="$T/config" GONDO_PI_KEYSTORE=memory
export GONDO_PI_PROMPT_AUTO='{"git-push":{"choice":"deny"},"net":{"choice":"deny"},"ssh":{"choice":"deny"}}'
mkdir -p "$T/config" "$T/proj"
ssh-keygen -q -t ed25519 -N "" -C gondo-pi-e2e -f "$T/e2e_key"
python3 - "$T" <<'PY'
import json, sys
t = sys.argv[1]
c = json.load(open(__import__("os").path.expanduser("~/.config/gondo-pi/config.json")))
c.setdefault("ssh", {})["keys"] = [f"{t}/e2e_key"]
json.dump(c, open(f"{t}/config/config.json", "w"))
PY
printf 't@t %s\n' "$(cat "$T/e2e_key.pub")" > "$T/allowed_signers"
cd "$T/proj" && git init -q && git -c user.email=t@t -c user.name=t -c commit.gpgsign=false commit -q --allow-empty -m init
cat > "$T/proj/probe.sh" <<'P'
#!/bin/sh
echo "== in VM: $(uname -sm) host=$(hostname) PI_SANDBOX=$PI_SANDBOX"
echo "== ssh private key in guest? $(ls ~/.ssh/id_ed25519 ~/.ssh/id_rsa 2>/dev/null | wc -l) (want 0)"
echo "== SSH_AUTH_SOCK: ${SSH_AUTH_SOCK:-unset}"
echo "== host secrets visible? $(ls -d /Users/*/.ssh /Users/*/.aws /Users/*/.local/state/gondo-pi /Users/*/Library /home/*/.ssh/id_* 2>/dev/null | grep -v '\.pub$' | head -3) (want empty)"
echo "== guest aws config has only placeholders: $(grep -h aws_access_key_id ~/.aws/config 2>/dev/null | grep -vc GONDOP) non-placeholder key(s)"
echo "== GH_TOKEN is placeholder: $(echo "$GH_TOKEN" | cut -c1-14)"
echo "== gh api user: $(gh api user -q .login 2>&1 | head -1)"
echo "== git fetch over ssh (host ssh w/ throwaway key: GitHub refuses it): $(git ls-remote git@github.com:earendil-works/gondolin.git HEAD 2>&1 | grep -oiE 'permission denied[^.]*|^[0-9a-f]{12}' | head -1)"
echo "== git push (denied by canned answer): $(git push git@github.com:hmgibson23/gondo-pi-e2e-nonexistent.git HEAD:refs/heads/x 2>&1 | grep -io 'gondo-pi[^.]*' | head -1)"
echo "== interactive ssh (denied): $(ssh -T git@github.com 2>&1 | head -1 | cut -c1-60)"
git -c user.email=t@t -c user.name=t -c commit.gpgsign=true -c gpg.format=ssh -c user.signingkey="key::$(cat /workspace/e2e_key.pub)" commit -q --allow-empty -m signed 2>&1 | head -2
echo "== signed commit: $(git cat-file commit HEAD | grep -c 'BEGIN SSH SIGNATURE') sig block(s)"
echo "== allowlisted (pypi): $(curl -s -o /dev/null -w %{http_code} -m 15 https://pypi.org/simple/)"
echo "== not allowlisted (denied): $(curl -s -o /dev/null -w %{http_code} -m 15 https://example.com)"
echo "== placeholder never leaves for other hosts: $(curl -s -o /dev/null -w %{http_code} -m 15 -H "Authorization: token $GH_TOKEN" https://httpbin.org/headers)"
echo "== raw tcp (non-HTTP) out: $(printf 'SSH-2.0-x\r\n' | timeout 6 nc -w4 1.1.1.1 443 2>/dev/null | head -c 12 | tr -d '\r\n')$(echo) (want empty/punt! = no upstream)"
echo "== plain http to an IP: $(printf 'GET / HTTP/1.0\r\nHost: 1.1.1.1\r\n\r\n' | timeout 6 nc -w4 1.1.1.1 80 2>/dev/null | head -1 | tr -d '\r') (want 403)"
echo "== broker status: $(python3 -c 'import sys; sys.path.insert(0,"/gondo-pi/bin"); import gondopi_client as c; d=c.call({"op":"status"}); print("signed",d["signed"],"vm",d["vm"])')"
P
chmod +x "$T/proj/probe.sh"; cp "$T/e2e_key.pub" "$T/proj/e2e_key.pub"
python3 - "$T" <<'PY'
import sys, os, subprocess
L = os.path.expanduser("~/snippets/gondo-pi/lib"); sys.path.insert(0, L)
from gondopi.supervisor import Supervisor
from gondopi.vm import VmHost
s = Supervisor("e2e", os.getcwd(), "e2e-proj", vm=True); s.serve()
h = VmHost(s, s.cfg, {"aws": []}); s.vmhost = h
try:
    h.start()
    code = f"import sys; sys.path.insert(0, {L!r}); from gondopi import attach; attach.run('cmd', ['sh', '/workspace/probe.sh'])"
    print(subprocess.run([sys.executable, "-c", code], stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=180).stdout)
finally:
    h.stop(); s.shutdown()
PY
echo "--- verify signature on the host: $(git -c gpg.ssh.allowedSignersFile=$T/allowed_signers verify-commit HEAD 2>&1 | head -1)"
echo "--- audit:"; cat "$T"/state/audit/*.jsonl | cut -c1-140
pkill -f "ssh-agent -a $T" 2>/dev/null
cd / && rm -rf "$T"
