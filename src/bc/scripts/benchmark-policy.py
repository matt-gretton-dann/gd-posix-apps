#!/usr/bin/env python3
"""Shared controls and acceptance, separate from raw regression observations.

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


ACCEPTANCE_POLICY = '2026-11-06-competitive-margin-v1'


def regression_acceptance(first, competitive, control, system, second=None,
                          substantive=True, measured=True, competitive_measured=True,
                          required_pairs=3):
    """Assess one row only; worthwhile gains, resources and merge need review.

    Collectors establish matched, output-checked roles. Statistics must retain
    the declared paired sample count. Startup and one-shot latency cannot use
    this exception. Raw regression and control labels are never rewritten.
    """
    if not substantive:
        return 'excluded observation'
    cap = {'Linux': 1.05, 'Windows': 1.05, 'Darwin': 1.0, 'macOS': 1.0}.get(system)
    if (cap is None or control != 'pass' or measured is not True or
            isinstance(required_pairs, bool) or not isinstance(required_pairs, int) or
            required_pairs < 3):
        return 'inconclusive'

    def qualified(value):
        if not isinstance(value, dict):
            return None
        pairs = value.get('pairs')
        if isinstance(pairs, bool) or not isinstance(pairs, int) or pairs != required_pairs:
            return None
        return bounds(value)

    intervals = [qualified(first)]
    if second is not None:
        intervals.append(qualified(second))
    if any(interval is None for interval in intervals):
        return 'inconclusive'
    if all(interval[1] <= cap for interval in intervals):
        return 'within regression limit'
    margin = qualified(competitive) if competitive_measured is True else None
    if margin is not None and margin[1] <= .90:
        return 'allowed with Howard margin'
    if len(intervals) == 2 and all(interval[0] > cap for interval in intervals):
        return 'exceeds regression limit'
    return 'inconclusive'


def acceptance_review(rows, system, kind, required_pairs):
    """Add a separate review to current or retained normalized collector rows."""
    if kind not in ('arithmetic', 'math'):
        raise ValueError('unknown benchmark kind')
    results = []
    for row in rows:
        if kind == 'math':
            duration = row.get('duration_target_met') or {}
            measured = all(duration.get(role) is True for role in ('gd-portable', 'gd-base'))
            arguments = dict(first=row.get('candidate_over_base'),
                             second=row.get('regression_confirmation'),
                             competitive=row.get('candidate_over_standard'),
                             control=row.get('aa_status'),
                             substantive=(row.get('parameters') or {}).get('mode') == 'repeated',
                             measured=measured,
                             competitive_measured=duration.get('howard-standard') is True,
                             required_pairs=required_pairs)
        else:
            raw = row.get('status', '')
            arguments = dict(first=row.get('ratio'), second=row.get('confirmation'),
                             competitive=row.get('candidate_over_howard'),
                             control=row.get('aa_control_status'),
                             substantive=row.get('comparison') == 'paired' and
                                         row.get('name') != 'startup',
                             measured='duration' not in raw and 'timing bias' not in raw,
                             required_pairs=row.get('fixed_samples', required_pairs))
        results.append(dict(name=row.get('name'), preset=row.get('preset'),
                            comparison=row.get('comparison'),
                            raw_status=row.get('regression_status', row.get('status')),
                            acceptance=regression_acceptance(system=system, **arguments)))
    return dict(policy=ACCEPTANCE_POLICY, policy_sha256=source_sha256(), advisory=True,
                results=results, merge_eligible=False,
                remaining_review=['worthwhile measured gain elsewhere', 'complete cohort',
                                  'startup and excluded latency', 'memory, allocations and code size',
                                  'correctness and final integration'])
