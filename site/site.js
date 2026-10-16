/* Load committed report files and draw them. Does not invent rows. */
(function () {
  const MODELS = ["conservative", "probabilistic", "optimistic"];
  const COLORS = { conservative: "#141414", probabilistic: "#6a6a6a", optimistic: "#b5b5b5" };

  function fail(msg) {
    const el = document.getElementById("results-error");
    el.hidden = false;
    el.textContent = msg;
  }

  function parseCSV(text) {
    const lines = text.replace(/\r/g, "").trim().split("\n");
    if (!lines.length) return [];
    const head = lines[0].split(",");
    return lines.slice(1).filter(Boolean).map((line) => {
      const cells = line.split(",");
      const row = {};
      head.forEach((h, i) => { row[h] = cells[i] == null ? "" : cells[i]; });
      return row;
    });
  }

  async function getText(path) {
    const res = await fetch(path);
    if (!res.ok) throw new Error("missing " + path + " (" + res.status + ")");
    return res.text();
  }

  async function getJSON(path) {
    return JSON.parse(await getText(path));
  }

  function fmt(n, d) {
    if (n == null || n === "" || Number.isNaN(Number(n))) return "";
    return Number(n).toFixed(d);
  }

  function table(headers, rows) {
    const wrap = document.createElement("div");
    const t = document.createElement("table");
    const thead = document.createElement("thead");
    const trh = document.createElement("tr");
    headers.forEach((h) => {
      const th = document.createElement("th");
      th.textContent = h.label;
      if (h.num) th.className = "num";
      trh.appendChild(th);
    });
    thead.appendChild(trh);
    t.appendChild(thead);
    const tb = document.createElement("tbody");
    rows.forEach((row) => {
      const tr = document.createElement("tr");
      headers.forEach((h) => {
        const td = document.createElement("td");
        td.textContent = h.fmt ? h.fmt(row) : row[h.key];
        if (h.num) td.className = "num";
        tr.appendChild(td);
      });
      tb.appendChild(tr);
    });
    t.appendChild(tb);
    wrap.className = "table-wrap";
    wrap.appendChild(t);
    return wrap;
  }

  function svgEl(name, attrs) {
    const el = document.createElementNS("http://www.w3.org/2000/svg", name);
    Object.entries(attrs || {}).forEach(([k, v]) => el.setAttribute(k, v));
    return el;
  }

  function nice(v) {
    if (!isFinite(v) || v === 0) return 0;
    const p = Math.pow(10, Math.floor(Math.log10(Math.abs(v))));
    return Math.sign(v) * Math.ceil(Math.abs(v) / p) * p;
  }

  function chartFrame(w, h, pad, yMin, yMax, xLabels) {
    const svg = svgEl("svg", { viewBox: `0 0 ${w} ${h}`, role: "img" });
    const innerW = w - pad.l - pad.r;
    const innerH = h - pad.t - pad.b;
    const y0 = yMin;
    const y1 = yMax === yMin ? yMin + 1 : yMax;
    const yScale = (v) => pad.t + innerH - ((v - y0) / (y1 - y0)) * innerH;
    const n = Math.max(xLabels.length, 1);
    const xScale = (i) => pad.l + (n === 1 ? innerW / 2 : (i / (n - 1)) * innerW);
    svg.appendChild(svgEl("line", { x1: pad.l, y1: pad.t, x2: pad.l, y2: pad.t + innerH, stroke: "#e4e4e4" }));
    svg.appendChild(svgEl("line", { x1: pad.l, y1: pad.t + innerH, x2: pad.l + innerW, y2: pad.t + innerH, stroke: "#e4e4e4" }));
    [y0, (y0 + y1) / 2, y1].forEach((v) => {
      const y = yScale(v);
      svg.appendChild(svgEl("line", { x1: pad.l, y1: y, x2: pad.l + innerW, y2: y, stroke: "#f0f0f0" }));
      const label = svgEl("text", { x: pad.l - 8, y: y + 3, "text-anchor": "end", fill: "#8a8a8a", "font-size": "11", "font-family": "IBM Plex Mono, ui-monospace, monospace" });
      label.textContent = Math.abs(v) >= 100 ? v.toFixed(0) : v.toFixed(2);
      svg.appendChild(label);
    });
    xLabels.forEach((lab, i) => {
      const label = svgEl("text", { x: xScale(i), y: h - 8, "text-anchor": "middle", fill: "#5c5c5c", "font-size": "11", "font-family": "IBM Plex Mono, ui-monospace, monospace" });
      label.textContent = lab;
      svg.appendChild(label);
    });
    return { svg, xScale, yScale, innerW, n };
  }

  function groupedBars(spec) {
    const w = 520, h = 280, pad = { l: 52, t: 16, r: 12, b: 32 };
    const delays = spec.x;
    let yMin = spec.yMin != null ? spec.yMin : 0;
    let yMax = spec.yMax != null ? spec.yMax : 0;
    spec.series.forEach((s) => s.values.forEach((v) => {
      if (v == null || Number.isNaN(v)) return;
      yMax = Math.max(yMax, v);
      yMin = Math.min(yMin, v);
    }));
    if (spec.yMax == null) yMax = yMax === 0 ? 1 : nice(yMax * 1.12);
    if (spec.yMin == null && yMin < 0) yMin = -nice(Math.abs(yMin) * 1.12);
    const frame = chartFrame(w, h, pad, yMin, yMax, spec.xLabels);
    const group = frame.innerW / Math.max(delays.length, 1);
    const barW = Math.min(18, (group * 0.7) / spec.series.length);
    spec.series.forEach((s, si) => {
      s.values.forEach((v, i) => {
        if (v == null || Number.isNaN(v)) return;
        const cx = frame.xScale(i) - ((spec.series.length - 1) * barW) / 2 + si * barW;
        const y = frame.yScale(v);
        const yBase = frame.yScale(Math.max(yMin, 0));
        const rect = svgEl("rect", {
          x: cx - barW / 2 + 1,
          y: Math.min(y, yBase),
          width: barW - 2,
          height: Math.max(1, Math.abs(yBase - y)),
          fill: COLORS[s.name] || "#141414"
        });
        rect.appendChild(svgEl("title", {})).textContent = s.name + " " + spec.xLabels[i] + ": " + v.toFixed(4);
        frame.svg.appendChild(rect);
        if (s.lo && s.hi && s.lo[i] != null && s.hi[i] != null) {
          const y1 = frame.yScale(s.lo[i]);
          const y2 = frame.yScale(s.hi[i]);
          frame.svg.appendChild(svgEl("line", { x1: cx, y1: y1, x2: cx, y2: y2, stroke: COLORS[s.name], "stroke-width": 1 }));
        }
      });
    });
    frame.svg.setAttribute("aria-label", spec.title);
    return frame.svg;
  }

  function lines(spec) {
    const w = 520, h = 280, pad = { l: 52, t: 16, r: 12, b: 32 };
    let yMin = Infinity, yMax = -Infinity;
    spec.series.forEach((s) => s.values.forEach((v) => {
      if (v == null || Number.isNaN(v)) return;
      yMin = Math.min(yMin, v);
      yMax = Math.max(yMax, v);
    }));
    if (!isFinite(yMin)) { yMin = 0; yMax = 1; }
    const span = yMax - yMin || 1;
    yMin -= span * 0.12;
    yMax += span * 0.12;
    const frame = chartFrame(w, h, pad, yMin, yMax, spec.xLabels);
    spec.series.forEach((s) => {
      const pts = s.values.map((v, i) => (v == null ? null : frame.xScale(i) + "," + frame.yScale(v))).filter(Boolean);
      frame.svg.appendChild(svgEl("polyline", { points: pts.join(" "), fill: "none", stroke: COLORS[s.name] || "#141414", "stroke-width": 1.4 }));
      s.values.forEach((v, i) => {
        if (v == null) return;
        const c = svgEl("circle", { cx: frame.xScale(i), cy: frame.yScale(v), r: 2.6, fill: COLORS[s.name] || "#141414" });
        c.appendChild(svgEl("title", {})).textContent = s.name + " " + spec.xLabels[i] + ": " + v.toFixed(4);
        frame.svg.appendChild(c);
      });
    });
    frame.svg.setAttribute("aria-label", spec.title);
    return frame.svg;
  }

  function simpleBars(labels, values, title) {
    const w = 520, h = 240, pad = { l: 52, t: 16, r: 12, b: 32 };
    const yMax = Math.max(1, ...values) * 1.15;
    const frame = chartFrame(w, h, pad, 0, yMax, labels);
    const group = frame.innerW / Math.max(labels.length, 1);
    values.forEach((v, i) => {
      const cx = frame.xScale(i);
      const y = frame.yScale(v);
      const yBase = frame.yScale(0);
      const rect = svgEl("rect", { x: cx - Math.min(28, group * 0.28), y: y, width: Math.min(56, group * 0.56), height: Math.max(1, yBase - y), fill: "#141414" });
      rect.appendChild(svgEl("title", {})).textContent = labels[i] + ": " + v;
      frame.svg.appendChild(rect);
    });
    frame.svg.setAttribute("aria-label", title);
    return frame.svg;
  }

  function midPath(rows) {
    const step = Math.max(1, Math.floor(rows.length / 240));
    const pts = [];
    for (let i = 0; i < rows.length; i += step) pts.push(rows[i]);
    if (rows.length) pts.push(rows[rows.length - 1]);
    const xs = pts.map((r) => Number(r.time_us) / 1e6);
    const ys = pts.map((r) => Number(r.mid));
    const w = 1040, h = 260, pad = { l: 72, t: 16, r: 16, b: 32 };
    const yMin = Math.min(...ys) - 2;
    const yMax = Math.max(...ys) + 2;
    const labels = [xs[0].toFixed(1) + "s", xs[Math.floor(xs.length / 2)].toFixed(1) + "s", xs[xs.length - 1].toFixed(1) + "s"];
    const frame = chartFrame(w, h, pad, yMin, yMax, ["", "", ""]);
    const x0 = xs[0], x1 = xs[xs.length - 1] || 1;
    const coords = xs.map((x, i) => {
      const px = pad.l + ((x - x0) / (x1 - x0 || 1)) * (w - pad.l - pad.r);
      const py = frame.yScale(ys[i]);
      return px + "," + py;
    });
    frame.svg.appendChild(svgEl("polyline", { points: coords.join(" "), fill: "none", stroke: "#141414", "stroke-width": 1.2 }));
    const cap = svgEl("text", { x: w / 2, y: h - 8, "text-anchor": "middle", fill: "#5c5c5c", "font-size": "11", "font-family": "IBM Plex Mono, ui-monospace, monospace" });
    cap.textContent = "event time (s) · " + labels.join("  ");
    frame.svg.appendChild(cap);
    frame.svg.setAttribute("aria-label", "Synthetic session mid path");
    return frame.svg;
  }

  function legend() {
    const d = document.createElement("div");
    d.className = "legend";
    MODELS.forEach((m) => {
      const s = document.createElement("span");
      s.innerHTML = `<i class="swatch" style="background:${COLORS[m]}"></i>${m}`;
      d.appendChild(s);
    });
    return d;
  }

  function card(title, note, source, node, wide) {
    const el = document.createElement("article");
    el.className = "chart" + (wide ? " wide" : "");
    const h = document.createElement("h3");
    h.textContent = title;
    const p = document.createElement("p");
    p.className = "note";
    p.textContent = note;
    const src = document.createElement("p");
    src.className = "src";
    src.textContent = source;
    el.appendChild(h);
    el.appendChild(p);
    el.appendChild(legend());
    el.appendChild(node);
    el.appendChild(src);
    return el;
  }

  function renderMarkdown(md) {
    const lines = md.replace(/\r/g, "").split("\n");
    const out = [];
    let i = 0;
    const esc = (s) => s.replace(/[&<>]/g, (c) => ({ "&": "&", "<": "<", ">": ">" }[c]));
    const inline = (s) => esc(s).replace(/`([^`]+)`/g, "<code>$1</code>");
    while (i < lines.length) {
      const line = lines[i];
      if (line.startsWith("|")) {
        const rows = [];
        while (i < lines.length && lines[i].startsWith("|")) {
          if (!/^\|\s*-+/.test(lines[i])) rows.push(lines[i].slice(1, -1).split("|").map((c) => c.trim()));
          i++;
        }
        const head = rows.shift() || [];
        out.push("<table><thead><tr>" + head.map((c) => "<th>" + inline(c) + "</th>").join("") + "</tr></thead><tbody>" +
          rows.map((r) => "<tr>" + r.map((c) => "<td>" + inline(c) + "</td>").join("") + "</tr>").join("") + "</tbody></table>");
        continue;
      }
      if (line.startsWith("### ")) { out.push("<h3>" + inline(line.slice(4)) + "</h3>"); i++; continue; }
      if (line.startsWith("## ")) { out.push("<h2>" + inline(line.slice(3)) + "</h2>"); i++; continue; }
      if (line.startsWith("# ")) { out.push("<h1>" + inline(line.slice(2)) + "</h1>"); i++; continue; }
      if (line.startsWith("- ")) {
        const items = [];
        while (i < lines.length && lines[i].startsWith("- ")) { items.push("<li>" + inline(lines[i].slice(2)) + "</li>"); i++; }
        out.push("<ul>" + items.join("") + "</ul>");
        continue;
      }
      if (line.trim() === "") { i++; continue; }
      out.push("<p>" + inline(line) + "</p>");
      i++;
    }
    return out.join("");
  }

  async function main() {
    const [sensText, markH, offset, divExec, book, bench, quality, meta, baseline, capture, divQ, report, cfgS, cfgD, cfgB] = await Promise.all([
      getText("data/sensitivity.csv"),
      getText("data/markout_horizons.csv"),
      getText("data/fill_rate_by_offset.csv"),
      getText("data/divergence_executions.csv"),
      getText("data/book_series.csv"),
      getJSON("data/bench.json"),
      getJSON("data/data_quality.json"),
      getJSON("data/queue_sensitivity.json"),
      getText("data/baseline_sensitivity.csv"),
      getJSON("data/capture_attempt.json"),
      getJSON("data/divergence_quality.json"),
      getText("data/execution_research.md"),
      getJSON("data/queue_sensitivity_config.json"),
      getJSON("data/divergence_config.json"),
      getJSON("data/baseline_config.json")
    ]);

    const sens = parseCSV(sensText);
    const delays = [...new Set(sens.map((r) => r.submit_delay_us))];
    const by = (model) => delays.map((d) => sens.find((r) => r.model === model && r.submit_delay_us === d));

    const facts = document.getElementById("facts");
    const eps = bench.events_per_second_synthetic_median;
    facts.innerHTML = `
      <div class="fact"><span>Language</span><b>OCaml ${bench.ocaml}</b><small>stdlib + Yojson · profile ${bench.profile}</small></div>
      <div class="fact"><span>Committed run</span><b>synthetic</b><small>${quality.counts.depth} depth · ${quality.counts.trade} trades · gaps ${quality.gaps}</small></div>
      <div class="fact"><span>Replay, this machine</span><b>${(eps / 1e6).toFixed(2)}e6 ev/s</b><small>${bench.cpu.split("@")[0].trim()}</small></div>
      <div class="fact"><span>Live capture</span><b>none committed</b><small>${capture.messages} messages stored</small></div>`;

    const rates = MODELS.map((m) => by(m).map((r) => Number(r.fill_rate)));
    const gap = Math.max(...rates.flat()) - Math.min(...rates.flat());
    document.getElementById("finding").innerHTML =
      `<p>On this synthetic session the three queue models do not separate fill rates. Largest gap at a fixed submit delay: <span class="mono">${gap.toFixed(4)}</span>. That is a property of the generator — many fills are trade-throughs or consume the whole displayed level — not a claim that the models agree on Binance flow.</p>
       <p>The five-event divergence fixture is where the rules are forced apart. Dataset SHA-256 <span class="mono">${meta.dataset_sha256.slice(0, 16)}…</span>. Book hash <span class="mono">${meta.book_hash}</span>. Engine git field <span class="mono">${meta.git}</span>.</p>`;

    const charts = document.getElementById("charts");
    const xLabels = delays.map((d) => d === "0" ? "0" : (Number(d) / 1000) + " ms");
    charts.appendChild(card(
      "Fill rate by submit delay",
      "Fraction of accepted orders with any fill. Whiskers are the block-bootstrap percentile interval. Synthetic session, n = 80 accepted per cell.",
      "Source: reports/sensitivity.csv · synthetic · price-level fixture, not Binance",
      groupedBars({
        x: delays, xLabels, yMin: 0, yMax: 1, title: "Fill rate by submit delay",
        series: MODELS.map((m) => ({
          name: m,
          values: by(m).map((r) => Number(r.fill_rate)),
          lo: by(m).map((r) => Number(r.fill_rate_ci_lo)),
          hi: by(m).map((r) => Number(r.fill_rate_ci_hi))
        }))
      })
    ));
    charts.appendChild(card(
      "Mean signed markout by submit delay",
      "Pooled across horizons 5 ms, 20 ms, and 100 ms. Positive means the later mid moved in the order’s favor. Price units. Overlapping orders are dependent.",
      "Source: reports/sensitivity.csv · synthetic · not a confidence interval on a live book",
      lines({
        xLabels, title: "Mean markout by submit delay",
        series: MODELS.map((m) => ({ name: m, values: by(m).map((r) => Number(r.mean_markout)) }))
      })
    ));
    charts.appendChild(card(
      "Mean spread capture",
      "Buy: mid at fill minus fill price. Sell: fill price minus mid. Price units. Positive is not net P&L; fees are in the ledger, not in this column.",
      "Source: reports/sensitivity.csv · synthetic",
      lines({
        xLabels, title: "Mean spread capture",
        series: MODELS.map((m) => ({ name: m, values: by(m).map((r) => Number(r.mean_spread_capture)) }))
      })
    ));
    charts.appendChild(card(
      "Mean implementation shortfall",
      "Buy: fill price minus decision mid. Sell: decision mid minus fill price. Price units. Unfilled orders are not given a counterfactual price.",
      "Source: reports/sensitivity.csv · synthetic",
      lines({
        xLabels, title: "Mean implementation shortfall",
        series: MODELS.map((m) => ({ name: m, values: by(m).map((r) => Number(r.mean_implementation_shortfall)) }))
      })
    ));

    const mh = parseCSV(markH);
    const horizons = [...new Set(mh.map((r) => r.horizon_us))];
    charts.appendChild(card(
      "Markout by horizon",
      "Mean signed markout at each configured horizon, still on the synthetic session. Units are price units, not basis points.",
      "Source: reports/figures/markout_horizons.csv · derived from reports/markouts.csv · synthetic",
      lines({
        xLabels: horizons.map((h) => (Number(h) / 1000) + " ms"),
        title: "Markout by horizon",
        series: MODELS.map((m) => ({ name: m, values: horizons.map((h) => Number(mh.find((r) => r.model === m && r.horizon_us === h).signed_markout)) }))
      })
    ));

    const off = parseCSV(offset);
    charts.appendChild(card(
      "Fill rate by offset from touch",
      "Offset 0 is the touch. Offset 1 is one tick behind. Same synthetic session, submit delay pooled in the figure file. Models do not separate here either.",
      "Source: reports/figures/fill_rate_by_offset.csv · synthetic",
      groupedBars({
        x: ["0", "1"], xLabels: ["touch", "1 tick back"], yMin: 0, yMax: 1, title: "Fill rate by offset",
        series: MODELS.map((m) => ({ name: m, values: ["0", "1"].map((o) => Number(off.find((r) => r.model === m && r.offset_ticks === o).fill_rate)) }))
      })
    ));

    const div = parseCSV(divExec);
    const divCard = document.createElement("article");
    divCard.className = "chart";
    divCard.innerHTML = `<h3>Constructed disagreement</h3><p class="note">One passive buy, quantity 4, on a five-event book built so the cancel rules can disagree. Not a sample from a market. Probabilistic result is the draw at seed 1, not a third bound.</p>`;
    divCard.appendChild(simpleBars(
      div.map((r) => r.model),
      div.map((r) => Number(r.filled_qty)),
      "Divergence filled quantity"
    ));
    const src = document.createElement("p");
    src.className = "src";
    src.textContent = "Source: reports/divergence/executions.csv · synthetic · SYN · " + divQ.notes;
    divCard.appendChild(src);
    charts.appendChild(divCard);

    const benchCard = document.createElement("article");
    benchCard.className = "chart";
    benchCard.innerHTML = `<h3>Replay time, this machine</h3><p class="note">Median wall-clock after one discarded warmup. Dataset replay is the 5,357-event session. Synthetic replay is ${bench.synthetic_events.toLocaleString()} events from ${bench.synthetic_steps.toLocaleString()} steps. Do not reuse the rate on other hardware.</p>`;
    benchCard.appendChild(simpleBars(
      ["session", "200k-step"],
      [bench.dataset_replay.median_s * 1000, bench.synthetic_replay.median_s * 1000],
      "Replay time in milliseconds"
    ));
    const bsrc = document.createElement("p");
    bsrc.className = "src";
    bsrc.textContent = `Source: reports/bench.json · ${bench.cpu} · OCaml ${bench.ocaml} · ${bench.profile} · ${(eps / 1e6).toFixed(2)}e6 events/s on the long synthetic replay`;
    benchCard.appendChild(bsrc);
    charts.appendChild(benchCard);

    const bookRows = parseCSV(book).filter((r) => r.mid !== "");
    const midCard = document.createElement("article");
    midCard.className = "chart wide";
    midCard.innerHTML = `<h3>Displayed mid, synthetic session</h3><p class="note">Downsampled from ${bookRows.length} book observations. Spread is 10 price units on every row of this file. Vertical axis does not start at zero. Not a Binance mid.</p>`;
    midCard.appendChild(midPath(bookRows));
    const msrc = document.createElement("p");
    msrc.className = "src";
    msrc.textContent = "Source: reports/book_series.csv · synthetic session · price units · price_scale 2";
    midCard.appendChild(msrc);
    charts.appendChild(midCard);

    const sensHost = document.getElementById("sensitivity-table");
    sensHost.appendChild(table([
      { key: "model", label: "model" },
      { key: "submit_delay_us", label: "delay µs", num: true },
      { key: "n_accepted", label: "n", num: true },
      { key: "n_filled", label: "filled", num: true },
      { key: "fill_rate", label: "fill rate", num: true, fmt: (r) => fmt(r.fill_rate, 4) },
      { key: "fill_rate_ci_lo", label: "CI low", num: true, fmt: (r) => fmt(r.fill_rate_ci_lo, 4) },
      { key: "fill_rate_ci_hi", label: "CI high", num: true, fmt: (r) => fmt(r.fill_rate_ci_hi, 4) },
      { key: "mean_filled_qty", label: "mean qty", num: true, fmt: (r) => fmt(r.mean_filled_qty, 2) },
      { key: "mean_spread_capture", label: "spread cap.", num: true, fmt: (r) => fmt(r.mean_spread_capture, 3) },
      { key: "mean_implementation_shortfall", label: "IS", num: true, fmt: (r) => fmt(r.mean_implementation_shortfall, 3) },
      { key: "mean_markout", label: "markout", num: true, fmt: (r) => fmt(r.mean_markout, 3) }
    ], sens));

    const configs = [
      ["queue_sensitivity", cfgS],
      ["divergence", cfgD],
      ["baseline", cfgB]
    ];
    document.getElementById("config-table").appendChild(table([
      { key: "name", label: "config" },
      { key: "dataset", label: "dataset" },
      { key: "models", label: "models" },
      { key: "delays", label: "submit delay µs" },
      { key: "qty", label: "qty / orders" },
      { key: "fee", label: "fee ppm", num: true }
    ], configs.map(([name, c]) => ({
      name,
      dataset: c.dataset,
      models: c.queue_models.join(", "),
      delays: c.submit_delay_grid_us.join(", "),
      qty: c.intents.mode === "explicit" ? c.intents.orders.map((o) => o.qty).join(", ") : c.intents.qty + " × " + c.intents.max_orders,
      fee: String(c.fee_ppm)
    }))));

    document.getElementById("baseline-table").appendChild(table([
      { key: "model", label: "model" },
      { key: "submit_delay_us", label: "delay µs", num: true },
      { key: "n_accepted", label: "n", num: true },
      { key: "fill_rate", label: "fill rate", num: true, fmt: (r) => fmt(r.fill_rate, 4) },
      { key: "mean_filled_qty", label: "mean qty", num: true, fmt: (r) => fmt(r.mean_filled_qty, 2) },
      { key: "mean_markout", label: "markout", num: true, fmt: (r) => fmt(r.mean_markout, 3) }
    ], parseCSV(baseline)));

    document.getElementById("quality-table").appendChild(table([
      { key: "k", label: "check" },
      { key: "v", label: "session" }
    ], [
      { k: "ok", v: String(quality.ok) },
      { k: "sync", v: quality.sync_mode },
      { k: "gaps / duplicates / crossed", v: quality.gaps + " / " + quality.duplicates + " / " + quality.crossed_observations },
      { k: "events", v: String(quality.events_consumed) },
      { k: "book hash", v: quality.book_hash },
      { k: "synthetic", v: String(quality.synthetic) }
    ]));

    document.getElementById("report").innerHTML = renderMarkdown(report);
  }

  main().catch((err) => fail(String(err && err.message ? err.message : err)));
})();
