#!/usr/bin/env python3
"""Capture Binance spot public depth and trade streams.

Stdlib only: TLS websocket plus REST snapshot and exchangeInfo. No API key.
No order endpoints are called.

This process writes raw messages. Normalization and depth-sync live in the
OCaml engine (`queue_positioning normalize`). Receive time and exchange time are
stored as different fields. Their difference is not a latency.

Raw line schema:
  {"rx_us": <int>, "kind": "ws"|"snapshot"|"interrupt", "payload": <object>}
"""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import socket
import ssl
import struct
import sys
import time
import urllib.request
from pathlib import Path

GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
HOST = "stream.binance.com"
REST = "https://api.binance.com"


def ws_accept(key: str) -> str:
    digest = hashlib.sha1((key + GUID).encode("ascii")).digest()
    return base64.b64encode(digest).decode("ascii")


def encode_client_frame(opcode: int, payload: bytes) -> bytes:
    """RFC 6455 client frame. Clients must mask."""
    if opcode < 0 or opcode > 15:
        raise ValueError("opcode")
    mask_key = os.urandom(4)
    masked = bytes(b ^ mask_key[i % 4] for i, b in enumerate(payload))
    n = len(payload)
    header = bytearray([0x80 | opcode])
    if n < 126:
        header.append(0x80 | n)
    elif n < 65536:
        header.append(0x80 | 126)
        header.extend(struct.pack("!H", n))
    else:
        header.append(0x80 | 127)
        header.extend(struct.pack("!Q", n))
    header.extend(mask_key)
    header.extend(masked)
    return bytes(header)


def pop_frames(buf: bytearray) -> list[tuple[int, bytes]]:
    """Pop complete frames from buf. Supports masked and unmasked payloads."""
    frames: list[tuple[int, bytes]] = []
    while True:
        if len(buf) < 2:
            break
        b0, b1 = buf[0], buf[1]
        opcode = b0 & 0x0F
        masked = (b1 & 0x80) != 0
        length = b1 & 0x7F
        offset = 2
        if length == 126:
            if len(buf) < offset + 2:
                break
            length = struct.unpack("!H", buf[offset : offset + 2])[0]
            offset += 2
        elif length == 127:
            if len(buf) < offset + 8:
                break
            length = struct.unpack("!Q", buf[offset : offset + 8])[0]
            offset += 8
        if masked:
            if len(buf) < offset + 4:
                break
            mask_key = bytes(buf[offset : offset + 4])
            offset += 4
        else:
            mask_key = b""
        if len(buf) < offset + length:
            break
        data = bytes(buf[offset : offset + length])
        del buf[: offset + length]
        if masked:
            data = bytes(b ^ mask_key[i % 4] for i, b in enumerate(data))
        frames.append((opcode, data))
    return frames


def fixed_scale_and_int(text: str) -> tuple[int, int]:
    """Decimal string -> (scale, integer). Trailing fractional zeros are stripped.
    '0.01000000' -> (2, 1). '10' -> (0, 10). No rounding.
    """
    s = text.strip()
    if s.startswith("+"):
        s = s[1:]
    if not s or s[0] == "-" or any(c in s for c in "eE"):
        raise ValueError(f"unsupported decimal {text!r}")
    if "." in s:
        whole, frac = s.split(".", 1)
        frac = frac.rstrip("0")
        scale = len(frac)
        digits = (whole + frac).lstrip("0") or "0"
    else:
        scale = 0
        digits = s.lstrip("0") or "0"
    if not digits.isdigit() or (whole_ok(s) is False):
        raise ValueError(f"unsupported decimal {text!r}")
    return scale, int(digits)


def whole_ok(s: str) -> bool:
    body = s.split(".", 1)[0]
    return body.isdigit() or body == ""


def fetch_json(url: str, timeout: float) -> dict:
    req = urllib.request.Request(url, headers={"User-Agent": "queue_positioning-capture/1"})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        raw = resp.read()
    body = json.loads(raw.decode("utf-8"))
    if not isinstance(body, dict):
        raise RuntimeError(f"expected object from {url}")
    return body


def instrument_from_exchange_info(symbol: str, info: dict) -> dict:
    symbols = info.get("symbols") or []
    row = next((s for s in symbols if s.get("symbol") == symbol), None)
    if row is None:
        raise RuntimeError(f"exchangeInfo has no {symbol}")
    filters = {f.get("filterType"): f for f in row.get("filters") or []}
    tick_s = (filters.get("PRICE_FILTER") or {}).get("tickSize")
    step_s = (filters.get("LOT_SIZE") or {}).get("stepSize")
    if not tick_s or not step_s:
        raise RuntimeError("missing PRICE_FILTER.tickSize or LOT_SIZE.stepSize")
    price_scale, tick = fixed_scale_and_int(str(tick_s))
    qty_scale, lot = fixed_scale_and_int(str(step_s))
    if tick <= 0 or lot <= 0:
        raise RuntimeError("non-positive tick or lot")
    return {
        "symbol": symbol,
        "price_scale": price_scale,
        "qty_scale": qty_scale,
        "tick": tick,
        "lot": lot,
        "source": "binance rest /api/v3/exchangeInfo",
        "tickSize": str(tick_s),
        "stepSize": str(step_s),
    }


def connect_ws(path: str, timeout: float) -> tuple[ssl.SSLSocket, bytearray]:
    raw = socket.create_connection((HOST, 9443), timeout=timeout)
    ctx = ssl.create_default_context()
    sock = ctx.wrap_socket(raw, server_hostname=HOST)
    key = base64.b64encode(os.urandom(16)).decode("ascii")
    req = (
        f"GET {path} HTTP/1.1\r\n"
        f"Host: {HOST}:9443\r\n"
        "Upgrade: websocket\r\n"
        "Connection: Upgrade\r\n"
        f"Sec-WebSocket-Key: {key}\r\n"
        "Sec-WebSocket-Version: 13\r\n"
        "User-Agent: queue_positioning-capture/1\r\n"
        "\r\n"
    )
    sock.sendall(req.encode("ascii"))
    data = b""
    while b"\r\n\r\n" not in data:
        chunk = sock.recv(4096)
        if not chunk:
            raise RuntimeError("websocket handshake closed")
        data += chunk
        if len(data) > 65536:
            raise RuntimeError("websocket handshake too large")
    header, rest = data.split(b"\r\n\r\n", 1)
    status = header.decode("iso-8859-1").split("\r\n", 1)[0]
    if " 101 " not in status:
        raise RuntimeError("websocket upgrade failed: " + status)
    accept = None
    for line in header.decode("iso-8859-1").split("\r\n")[1:]:
        if line.lower().startswith("sec-websocket-accept:"):
            accept = line.split(":", 1)[1].strip()
    if accept != ws_accept(key):
        raise RuntimeError("websocket accept mismatch")
    return sock, bytearray(rest)


def write_raw(fh, kind: str, payload: dict) -> None:
    rx_us = time.time_ns() // 1000
    fh.write(json.dumps({"rx_us": rx_us, "kind": kind, "payload": payload}, separators=(",", ":")) + "\n")
    fh.flush()


def run_capture(symbol: str, seconds: int, out: Path) -> int:
    if seconds < 1:
        print("duration-seconds must be >= 1", file=sys.stderr)
        return 2
    out.mkdir(parents=True, exist_ok=True)
    info = fetch_json(f"{REST}/api/v3/exchangeInfo?symbol={symbol}", 15)
    inst = instrument_from_exchange_info(symbol, info)
    # The OCaml loader reads symbol/price_scale/qty_scale/tick/lot only.
    (out / "instrument.json").write_text(
        json.dumps(
            {
                "symbol": inst["symbol"],
                "price_scale": inst["price_scale"],
                "qty_scale": inst["qty_scale"],
                "tick": inst["tick"],
                "lot": inst["lot"],
            }
        )
        + "\n"
    )
    (out / "capture_notes.json").write_text(
        json.dumps(
            {
                "symbol": symbol,
                "market": "spot",
                "host": HOST,
                "streams": [f"{symbol.lower()}@depth@100ms", f"{symbol.lower()}@trade"],
                "snapshot": "/api/v3/depth?limit=1000",
                "duration_seconds": seconds,
                "api_key": False,
                "orders_submitted": False,
                "note": (
                    "rx_us is the local clock at write time. Exchange timestamps "
                    "stay inside the payload. Do not treat rx_us - E as latency. "
                    "Public depth is market-by-price, not order-by-order."
                ),
            },
            indent=2,
        )
        + "\n"
    )
    path = "/stream?streams=" + "/".join(
        [f"{symbol.lower()}@depth@100ms", f"{symbol.lower()}@trade"]
    )
    deadline = time.monotonic() + seconds
    raw_path = out / "raw.jsonl"
    messages = 0
    interrupts = 0
    with raw_path.open("w") as fh:
        while time.monotonic() < deadline:
            try:
                sock, buf = connect_ws(path, 15)
            except Exception as exc:
                interrupts += 1
                write_raw(fh, "interrupt", {"reason": f"connect failed: {exc}"})
                if time.monotonic() >= deadline:
                    break
                time.sleep(0.5)
                continue
            try:
                snap = fetch_json(f"{REST}/api/v3/depth?symbol={symbol}&limit=1000", 15)
                write_raw(fh, "snapshot", snap)
                sock.settimeout(0.5)
                while time.monotonic() < deadline:
                    try:
                        chunk = sock.recv(65536)
                    except socket.timeout:
                        continue
                    if not chunk:
                        raise RuntimeError("websocket closed")
                    buf.extend(chunk)
                    for opcode, payload in pop_frames(buf):
                        if opcode == 0x1:
                            write_raw(fh, "ws", json.loads(payload.decode("utf-8")))
                            messages += 1
                        elif opcode == 0x9:
                            sock.sendall(encode_client_frame(0xA, payload))
                        elif opcode == 0x8:
                            raise RuntimeError("websocket close")
                        elif opcode == 0xA:
                            pass
                        elif opcode == 0x0:
                            # Continuation is not expected from this stream.
                            raise RuntimeError("unexpected websocket continuation")
            except Exception as exc:
                interrupts += 1
                write_raw(fh, "interrupt", {"reason": str(exc)})
                if time.monotonic() >= deadline:
                    break
                time.sleep(0.4)
            finally:
                try:
                    sock.close()
                except Exception:
                    pass
    print(
        f"captured symbol={symbol} messages={messages} interrupts={interrupts} out={out}",
        flush=True,
    )
    return 0


def self_test() -> int:
    payload = b'{"e":"trade","ok":true}'
    framed = encode_client_frame(0x1, payload)
    buf = bytearray(framed)
    frames = pop_frames(buf)
    if frames != [(0x1, payload)] or buf:
        raise AssertionError("masked roundtrip failed")
    # Unmasked server frame, 16-bit length.
    body = b"x" * 200
    raw = bytes([0x81, 126]) + struct.pack("!H", len(body)) + body
    buf = bytearray(raw[:10])
    if pop_frames(buf):
        raise AssertionError("partial frame consumed")
    buf.extend(raw[10:])
    got = pop_frames(buf)
    if got != [(0x1, body)] or buf:
        raise AssertionError("unmasked frame failed")
    scale, tick = fixed_scale_and_int("0.01000000")
    if (scale, tick) != (2, 1):
        raise AssertionError((scale, tick))
    scale, lot = fixed_scale_and_int("0.00001000")
    if (scale, lot) != (5, 1):
        raise AssertionError((scale, lot))
    if ws_accept("dGhlIHNhbXBsZSBub25jZQ==") != "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=":
        raise AssertionError("rfc6455 accept vector")
    print("capture self-test ok")
    return 0


def main(argv: list[str]) -> int:
    p = argparse.ArgumentParser(description="Binance spot public capture (no API key)")
    p.add_argument("--symbol", default="BTCUSDT")
    p.add_argument("--duration-seconds", type=int, default=30)
    p.add_argument("--out", default="data/raw/capture")
    p.add_argument("--self-test", action="store_true")
    args = p.parse_args(argv)
    if args.self_test:
        return self_test()
    symbol = args.symbol.upper()
    if not symbol.isalnum() or not (2 <= len(symbol) <= 20):
        print("symbol must be 2-20 alphanumeric characters", file=sys.stderr)
        return 2
    return run_capture(symbol, args.duration_seconds, Path(args.out))


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1:]))
    except Exception as exc:
        print(f"capture failed: {exc}", file=sys.stderr)
        sys.exit(1)
