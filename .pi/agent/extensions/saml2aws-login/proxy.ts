// A tiny local HTTP(S) proxy that only forwards to allowed hosts. Sandboxed commands can
// only open TCP connections to this proxy, so it is the single exit to the network.
// HTTPS goes through CONNECT (end-to-end TLS, the proxy never sees the content).

import { createServer, request as httpRequest, type Server } from "node:http";
import { connect, type Socket } from "node:net";
import { hostAllowed } from "./harden";

export interface ProxyHandle {
  port: number;
  /** Hosts that were refused since start (for the tool result). */
  refused: Set<string>;
  close: () => Promise<void>;
}

function splitHostPort(target: string, defaultPort: number): { host: string; port: number } {
  const m = /^\[?([^\]]+?)\]?(?::(\d+))?$/.exec(target);
  return { host: m?.[1] ?? target, port: m?.[2] ? Number(m[2]) : defaultPort };
}

export function startProxy(allowed: readonly string[]): Promise<ProxyHandle> {
  const refused = new Set<string>();
  const sockets = new Set<Socket>();

  const server: Server = createServer((req, res) => {
    // plain http: absolute-form URL
    let url: URL;
    try { url = new URL(req.url ?? ""); } catch { res.writeHead(400).end(); return; }
    if (!hostAllowed(url.hostname, allowed)) {
      refused.add(url.hostname);
      res.writeHead(403, { "content-type": "text/plain" }).end(`aws_exec sandbox: ${url.hostname} is not allowed\n`);
      return;
    }
    const upstream = httpRequest(
      { host: url.hostname, port: url.port || 80, path: url.pathname + url.search, method: req.method, headers: req.headers },
      (up) => { res.writeHead(up.statusCode ?? 502, up.headers); up.pipe(res); },
    );
    upstream.on("error", () => { if (!res.headersSent) res.writeHead(502); res.end(); });
    req.on("error", () => upstream.destroy());
    res.on("error", () => upstream.destroy());
    req.pipe(upstream);
  });

  // a client that hangs up (e.g. after a 403) must never crash pi
  server.on("clientError", (_err, socket) => socket.destroy());
  server.on("connect", (req, client: Socket, head: Buffer) => {
    client.on("error", () => client.destroy());
    const { host, port } = splitHostPort(req.url ?? "", 443);
    if (!hostAllowed(host, allowed) || (port !== 443 && port !== 80)) {
      refused.add(`${host}:${port}`);
      client.end("HTTP/1.1 403 Forbidden\r\ncontent-type: text/plain\r\n\r\naws_exec sandbox: host not allowed\n");
      return;
    }
    const upstream = connect(port, host, () => {
      client.write("HTTP/1.1 200 Connection Established\r\n\r\n");
      if (head.length) upstream.write(head);
      upstream.pipe(client);
      client.pipe(upstream);
    });
    sockets.add(upstream).add(client);
    const drop = () => { upstream.destroy(); client.destroy(); sockets.delete(upstream); sockets.delete(client); };
    upstream.on("error", drop).on("close", drop);
    client.on("error", drop).on("close", drop);
  });

  return new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      if (!address || typeof address === "string") { reject(new Error("proxy has no port")); return; }
      resolve({
        port: address.port,
        refused,
        close: () => new Promise<void>((done) => {
          for (const s of sockets) s.destroy();
          server.close(() => done());
          server.closeAllConnections?.();
        }),
      });
    });
  });
}
