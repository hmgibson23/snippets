#!/bin/bash
# End-to-end inside the real nono sandbox with an isolated nono-pi state + memory keystore.
# Uses the real ~/.ssh/id_ed25519 ONLY via the supervisor's private agent (host side).
set -u
T=$(mktemp -d "$HOME/.nono-pi-e2e.XXXX"); chmod 700 "$T"
export NONO_PI_STATE="$T/state" NONO_PI_CONFIG="$T/config" NONO_PI_KEYSTORE=memory
export NONO_PI_PROMPT_AUTO='{"git-push":{"choice":"deny"},"net":{"choice":"deny"},"ssh":{"choice":"deny"}}'
mkdir -p "$T/config" "$T/proj"
cp ~/.config/nono-pi/config.json "$T/config/config.json"
cd "$T/proj" && git init -q && git -c user.email=t@t -c user.name=t commit -q --allow-empty -m init 2>/dev/null
LIB=~/snippets/nono-pi/lib
# start supervisor directly (no zellij), then run a probe inside nono with the generated profile
python3 - "$T" <<'PY' &
import sys, os, time
sys.path.insert(0, os.path.expanduser("~/snippets/nono-pi/lib"))
from nonopi.supervisor import Supervisor
s = Supervisor("e2e", os.getcwd(), "e2e-proj"); s.picked_aws = []
s.serve()
open(sys.argv[1] + "/sup.ready", "w").close()
while True: time.sleep(1)
PY
SUP=$!
for i in $(seq 50); do [ -f "$T/sup.ready" ] && break; sleep 0.1; done
cat > "$T/probe.sh" <<'P'
#!/bin/bash
echo "== env: SSH_AUTH_SOCK=${SSH_AUTH_SOCK:-unset} broker=${PI_BROKER##*/}"
echo "== private agent reachable? $(python3 -c "import socket,os,glob;s=socket.socket(socket.AF_UNIX);p=os.environ['PI_BROKER'].replace('broker.sock','agent.sock');
try: s.connect(p); print('YES (BAD)')
except Exception as e: print('no:',type(e).__name__)")"
echo "== ctl reachable? $(python3 -c "import socket,os;s=socket.socket(socket.AF_UNIX);p=os.environ['PI_BROKER'].replace('broker.sock','ctl.sock');
try: s.connect(p); print('YES (BAD)')
except Exception as e: print('no:',type(e).__name__)")"
echo "== private key readable? $(head -c 5 ~/.ssh/id_ed25519 2>&1 | tail -1)"
echo "== git fetch (auto): $(git ls-remote git@github.com:nolabs-ai/nono.git HEAD 2>&1 | cut -c1-50)"
echo "== git https->ssh rewrite: $(git ls-remote https://github.com/nolabs-ai/nono.git HEAD 2>&1 | cut -c1-50)"
echo "== git push (denied by canned answer): $(git push git@github.com:hmgibson23/nono-pi-e2e-nonexistent.git HEAD:refs/heads/x 2>&1 | grep -i nono-pi | head -1)"
echo "== interactive ssh (denied): $(ssh -T git@github.com 2>&1 | head -1)"
echo "== arbitrary remote cmd via git ssh: $($GIT_SSH_COMMAND git@github.com 'id' 2>&1 | head -1)"
git -c user.email=t@t -c user.name=t -c commit.gpgsign=true -c gpg.format=ssh -c user.signingkey="$HOME/.ssh/id_ed25519.pub" commit -q --allow-empty -m signed 2>&1 | head -2
echo "== signed commit: $(git cat-file commit HEAD | grep -c 'BEGIN SSH SIGNATURE') sig block(s)"
echo "== verify: $(git -c gpg.ssh.allowedSignersFile=$HOME/.config/git/allowed_signers verify-commit HEAD 2>&1 | head -1)"
echo "== non-allowlisted host (net denied): $(curl -s -o /dev/null -w %{http_code} -m 15 https://example.com)"
echo "== allowlisted host: $(curl -s -o /dev/null -w %{http_code} -m 15 https://pypi.org/simple/)"
echo "== gh: $(gh api /user --jq .login 2>&1 | head -1)"
echo "== status: $(nono-pi status 2>&1 | python3 -c 'import json,sys;d=json.load(sys.stdin);print("grants",[g["key"] for g in d["grants"]],"signed",d["signed"])')"
P
chmod +x "$T/probe.sh"
python3 - "$T" <<'PY'
import sys, os, json
sys.path.insert(0, os.path.expanduser("~/snippets/nono-pi/lib"))
from nonopi import profile, paths
cfg = paths.load_config(); cfg["extra_read"] = cfg.get("extra_read", []) + [sys.argv[1]]
p, env = profile.build(cfg, {"aws": []})
json.dump(p, open(sys.argv[1] + "/p.json", "w"))
PY
nono run --silent --profile "$T/p.json" --allow-cwd -- ~/snippets/nono-pi/sandbox-bin/nono-pi-entry "$T/probe.sh" 2>&1
echo "--- audit:"; cat "$T"/state/audit/*.jsonl | cut -c1-140
kill $SUP 2>/dev/null; wait $SUP 2>/dev/null
rm -rf "$T"
