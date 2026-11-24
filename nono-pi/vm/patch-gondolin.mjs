#!/usr/bin/env node
// Workaround for gondolin 0.14.0 image builds on Linux (seen on CachyOS / node 26):
// fs.cpSync of the kernel-modules tree fails with EACCES in the build work dir,
// although every directory is ours and writable. A plain recursive copy works.
// Idempotent; run by install.sh after `npm ci`.
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const file = path.join(here, "node_modules/@earendil-works/gondolin/dist/src/alpine/kernel-modules.js");
const MARK = "// nono-pi: plain recursive copy";
const OLD = "    fs.cpSync(srcDir, dstDir, { recursive: true, dereference: false });";
const NEW = `    ${MARK} (fs.cpSync hits EACCES here on some Linux hosts)
    const walk = (s, d) => {
        const st = fs.lstatSync(s);
        if (st.isDirectory()) {
            fs.mkdirSync(d, { recursive: true });
            for (const e of fs.readdirSync(s)) walk(path.join(s, e), path.join(d, e));
        } else if (st.isSymbolicLink()) {
            fs.symlinkSync(fs.readlinkSync(s), d);
        } else {
            fs.copyFileSync(s, d);
        }
    };
    walk(srcDir, dstDir);`;

if (!fs.existsSync(file)) process.exit(0);
const src = fs.readFileSync(file, "utf8");
if (src.includes(MARK)) process.exit(0);
if (!src.includes(OLD)) {
	console.error("nono-pi: gondolin kernel-modules.js changed upstream; patch not applied");
	process.exit(0);
}
fs.writeFileSync(file, src.replace(OLD, NEW));
console.log("nono-pi: patched gondolin kernel-modules copy");
