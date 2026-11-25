"""In-memory engine for the accessibility and coverage analyses.

It reproduces the R + GRASS engine's outputs (same rules, same files) without a GRASS
session: rasters are read once, and one compiled routine computes travel time.
"""
