"""Discover AWS accounts from saml2aws state (role ARNs + regions only, never keys)."""
from __future__ import annotations

import configparser
import os
import re

HOME = os.path.expanduser("~")


def discover() -> dict:
    """Accounts from existing saml2aws sessions in ~/.aws/credentials (x_principal_arn)."""
    path = os.path.join(HOME, ".aws", "credentials")
    if not os.path.exists(path):
        return {}
    cp = configparser.RawConfigParser()
    try:
        cp.read(path)
    except configparser.Error:
        return {}
    accounts = {}
    for name in cp.sections():
        arn = cp.get(name, "x_principal_arn", fallback="")
        parts = arn.split(":")
        if len(parts) == 6 and parts[5].startswith("assumed-role/"):
            accounts[name] = {"role_arn": f"arn:aws:iam::{parts[4]}:role/{parts[5].split('/')[1]}",
                              "region": cp.get(name, "region", fallback="eu-west-2")}
    return accounts


def parse_list_roles(text: str, default_region: str = "eu-west-2") -> dict:
    """Parse `saml2aws list-roles` output:

        Account: nuk-cdn-dev (387331467362)
        arn:aws:iam::387331467362:role/nct-admin
    """
    accounts: dict = {}
    current = None
    for line in text.splitlines():
        line = line.strip()
        m = re.match(r"^Account:\s*(.+?)\s*\((\d{12})\)\s*$", line)
        if m:
            current = (m.group(1), m.group(2))
            continue
        m = re.match(r"^arn:aws:iam::(\d{12}):role/(\S+)$", line)
        if m:
            acct_id, role = m.group(1), m.group(2)
            base = current[0] if current and current[1] == acct_id else acct_id
            name = re.sub(r"[^A-Za-z0-9_.-]", "-", base)
            if name in accounts and accounts[name]["role_arn"] != line:
                name = f"{name}-{re.sub(r'[^A-Za-z0-9_.-]', '-', role)}"
            accounts[name] = {"role_arn": line, "region": default_region, "account_id": acct_id}
    return accounts
