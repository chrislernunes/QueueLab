#!/usr/bin/env python3
"""Write PNG and PDF figures from OCaml outputs. Run from the repository root."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import analysis


def main() -> int:
    analysis.save_session_figures(analysis.FIG)
    print(f"wrote figures under {analysis.FIG}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
