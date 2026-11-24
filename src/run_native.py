"""Command line of the in-memory engine: run_native.py accessibility|coverage, then the
options of the matching R script (accessibilityAnalysis.R, geoCoverageAnalysis.R)."""

import sys

from native import accessibility, coverage

ANALYSES = {"accessibility": accessibility.main, "coverage": coverage.main}

if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] not in ANALYSES:
        sys.exit(f"usage: run_native.py {'|'.join(ANALYSES)} [options]")
    ANALYSES[sys.argv[1]](sys.argv[2:])
