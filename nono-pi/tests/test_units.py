"""Unit tests: grants, git/ssh validation, signing payload checks, list-roles parsing."""
import os, sys, tempfile, time, unittest
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
os.environ.setdefault("NONO_PI_STATE", tempfile.mkdtemp())
os.environ.setdefault("NONO_PI_CONFIG", tempfile.mkdtemp())
os.environ["NONO_PI_KEYSTORE"] = "memory"
from nonopi import awsdisc, grants as G  # noqa
from nonopi.supervisor import Supervisor  # noqa


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
        os.environ["NONO_PI_PROMPT_AUTO"] = '{"git-push": {"choice": "window"}}'
        self.s = Supervisor("u-" + str(time.time()), "/tmp", "proj")

    def test_fetch_auto_push_asks(self):
        self.assertTrue(self.s.check_git("git@github.com", "22", "git-upload-pack 'newsuk/donk.git'")[0])
        self.assertTrue(self.s.check_git("git@github.com", "22", "git-receive-pack 'newsuk/donk.git'")[0])
        self.assertEqual(self.s.grants.check("git-push", "github.com/newsuk/donk"), "allow")

    def test_push_denied(self):
        os.environ["NONO_PI_PROMPT_AUTO"] = '{"git-push": {"choice": "deny"}}'
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




class ProfileValidates(unittest.TestCase):
    """Every generated profile variant must pass `nono profile validate` (catches reserved
    NONO_* env names, schema drift, bad routes) before a user ever launches it."""

    def test_variants(self):
        import json, shutil, subprocess
        from nonopi import paths, profile
        if not shutil.which("nono"):
            self.skipTest("nono not installed")
        cfg = paths.load_config()
        accounts = sorted(cfg["aws"].get("accounts", {}))
        for zj in ("", "1"):
            for aws in ([], accounts[:1]):
                with self.subTest(zjstatus=zj, aws=aws):
                    os.environ["NONO_PI_ZJSTATUS"] = zj
                    p, _ = profile.build(cfg, {"aws": aws})
                    reserved = [k for k in p["environment"]["set_vars"] if k.upper().startswith("NONO_")]
                    self.assertEqual(reserved, [], "NONO_* is reserved by nono")
                    f = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False)
                    json.dump(p, f); f.close()
                    r = subprocess.run(["nono", "profile", "validate", f.name], capture_output=True, text=True)
                    os.unlink(f.name)
                    self.assertIn("Result: valid", r.stdout + r.stderr, r.stdout + r.stderr)
        os.environ.pop("NONO_PI_ZJSTATUS", None)


class ConfigMerge(unittest.TestCase):
    def test_user_allow_adds_to_defaults(self):
        import json
        from nonopi import paths
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
