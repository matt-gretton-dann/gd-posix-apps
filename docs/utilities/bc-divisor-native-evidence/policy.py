#!/usr/bin/env python3
"""Shared symmetric A/A qualification, separate from directional regressions.

All intervals are nominal; this policy cannot remove hosted-runner contention.
SPDX-License-Identifier: Apache-2.0
"""
import hashlib
import math
from pathlib import Path


def bounds(first):
    interval = first.get('ci95')
    if not isinstance(interval, (list, tuple)) or len(interval) != 2:
        return None
    if any(isinstance(value, bool) or not isinstance(value, (int, float)) or
           not math.isfinite(value) for value in interval):
        return None
    low, high = interval
    return (low, high) if 0 < low <= high else None


def aa_direction(first):
    interval = bounds(first)
    if interval is None:
        return 0
    return 1 if interval[0] > 1.05 else -1 if interval[1] < .95 else 0


def aa_status(first, second=None):
    """Pass only a wholly in-band control; confirm persistent directional bias."""
    interval = bounds(first)
    if interval is None:
        return 'inconclusive'
    direction = aa_direction(first)
    if direction:
        same = second is not None and aa_direction(second) == direction
        return 'confirmed A/A bias' if same else 'inconclusive'
    return 'pass' if interval[0] >= .95 and interval[1] <= 1.05 else 'inconclusive'


def source_sha256():
    return hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
