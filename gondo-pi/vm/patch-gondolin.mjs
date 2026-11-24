#!/usr/bin/env node
// Workarounds for gondolin 0.14.0. Idempotent; run by install.sh after `npm ci`.
//  1. image builds on Linux: fs.cpSync of the kernel-modules tree fails with EACCES in the
//     build work dir, although every directory is ours and writable. A plain copy works.
//  2. TLS interception: a client's 2nd connection to a host resumes the TLS 1.3 session and
//     gondolin's MITM server failed it with "decrypt error" (every `git clone https://...`,
//     node keep-alive / fetch, curl with several URLs). Fixed by selecting the host
//     certificate before the handshake (gondolin's own SNI pre-parse, normally Bun-only).
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const dist = path.join(here, "node_modules/@earendil-works/gondolin/dist/src");

function patch(file, mark, old, neu, label) {
	if (!fs.existsSync(file)) return;
	const src = fs.readFileSync(file, "utf8");
	if (src.includes(mark)) return;
	if (!src.includes(old)) {
		console.error(`gondo-pi: gondolin ${label} changed upstream; patch not applied`);
		return;
	}
	fs.writeFileSync(file, src.replace(old, neu));
	console.log(`gondo-pi: patched gondolin ${label}`);
}

// 1 ------------------------------------------------------------------------------------
patch(path.join(dist, "alpine/kernel-modules.js"), "// gondo-pi: plain recursive copy",
	"    fs.cpSync(srcDir, dstDir, { recursive: true, dereference: false });",
	`    // gondo-pi: plain recursive copy (fs.cpSync hits EACCES here on some Linux hosts)
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
    walk(srcDir, dstDir);`,
	"kernel-modules copy");

// 2 ------------------------------------------------------------------------------------
// Root cause (reproduced with plain node:tls): with SNICallback switching to a per-host
// context that is NOT the server's default secureContext, TLS 1.3 resumption fails with
// "decrypt error". gondolin already pre-parses the ClientHello SNI as an option (used on
// Bun); turning that on means every MITM socket gets its host context as THE secureContext.
const net = path.join(dist, "qemu/net.js");
patch(net, "// gondo-pi: sni preparse",
	"        this.tlsSniPreparse = options.tlsSniPreparse ?? isBunRuntime();",
	"        this.tlsSniPreparse = options.tlsSniPreparse ?? true; // gondo-pi: sni preparse (TLS resumption fix)",
	"TLS SNI pre-parse (session resumption)");
