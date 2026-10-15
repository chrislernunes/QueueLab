#!/usr/bin/env python3
"""Execute notebooks in-process from the repository root and save outputs."""

from pathlib import Path

import nbformat
from nbclient import NotebookClient

ROOT = Path(__file__).resolve().parents[1]


def main() -> int:
    paths = sorted((ROOT / "notebooks").glob("*.ipynb"))
    if len(paths) != 6:
        raise SystemExit(f"expected 6 notebooks, found {len(paths)}")
    for path in paths:
        book = nbformat.read(path, as_version=4)
        client = NotebookClient(
            book,
            timeout=180,
            kernel_name="queuelab",
            resources={"metadata": {"path": str(ROOT)}},
        )
        client.execute()
        nbformat.write(book, path)
        print("executed", path.name)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
