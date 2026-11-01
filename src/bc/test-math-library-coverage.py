#!/usr/bin/env python3
"""Validate test-side probes and collect exact interpreted decision evidence."""
import argparse
from fractions import Fraction
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT / 'scripts'))
from math_library_coverage import instrument, strip_probes
from math_oracle import classify, decimal_fraction


def run(binary, source, program):
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
    environment.update(LC_ALL='C', BC_ENV_ARGS='', BC_LINE_LENGTH='70')
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / 'instrumented.bc'
        path.write_text(source, encoding='utf-8')
        return subprocess.run([binary, str(path)], input=program,
                              universal_newlines=True, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, env=environment, timeout=400)


def validate_transformer(binary):
    # Exhausted, skipped and broken loops, nested decisions, return, local
    # state and a side-effecting condition must retain their original behavior.
    source = '''define f(x) {
auto i,r;
r=0
for (i=0;i<x;i++) {
  if (i==2) break
  r+=i
}
while (r<2) {
  r+=1
}
if (x<0) return(7)
return(r)
}
define g(x) {
auto r;
r=0
while (x-->0) {
  r+=1
  if (r==2) return(r)
}
return(r)
}
'''
    program = 'f(0)\nf(5)\nf(-1)\ng(0)\ng(3)\n'
    original = run(binary, source, program)
    transformed, _ = instrument(source)
    traced = run(binary, transformed, program)
    assert original.returncode == traced.returncode == 0, (original.stderr, traced.stderr)
    plain, outcomes = strip_probes(traced.stdout)
    assert plain == original.stdout.splitlines() == ['2', '2', '7', '0', '2']
    assert outcomes


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    if hasattr(sys, 'set_int_max_str_digits'):
        sys.set_int_max_str_digits(1000000)
    validate_transformer(args.bc)
    source = (ROOT / 'math-library.bc').read_text(encoding='utf-8')
    transformed, decisions = instrument(source)
    cases = []
    for filename in ('math-oracles.json', 'math-crossover-oracles.json'):
        cases.extend(json.loads((ROOT / 'fixtures' / filename).read_text())['cases'])
    program = ''.join('scale={}\n{}({})\n'.format(
        case['scale'], case['function'], ','.join(case['arguments'])) for case in cases)
    result = run(args.bc, transformed, program)
    assert result.returncode == 0 and not result.stderr, result.stderr
    plain, outcomes = strip_probes(result.stdout)
    assert len(plain) == len(cases), (len(plain), len(cases))
    for value, case in zip(plain, cases):
        status = classify(decimal_fraction(value), Fraction(case['lower']),
                          Fraction(case['upper']), case['scale'])
        assert status == 'pass', (case['id'], case['scale'], status)
    # These are guard-behavior cases, not mathematical accuracy claims for
    # logarithms outside their domain. Keep their accounting separate.
    domain = run(args.bc, transformed, 'scale=20\nl(0)\nl(-1)\n')
    assert domain.returncode == 0 and not domain.stderr, domain.stderr
    domain_plain, domain_outcomes = strip_probes(domain.stdout)
    assert domain_plain == ['0', '0']
    outcomes.update(domain_outcomes)
    # GD rejects an unrepresentable scale before readback. A separate portable
    # host model clips precisely one setter at a time and must reach that
    # setter's defensive error branch, without returning a numeric result.
    modeled = set()
    j_guards = [item['line'] for item in decisions
                if item['function'] == 'j' and item['expression'] == 'scale!=w']
    assert len(j_guards) == 2
    for decision in decisions:
        if decision['expression'] != 'scale!=w':
            continue
        lines = source.splitlines()
        setter = decision['line'] - 2
        assert lines[setter].strip() == 'scale=w'
        lines[setter] = lines[setter].replace('scale=w', 'scale=0')
        model, model_decisions = instrument('\n'.join(lines) + '\n')
        assert model_decisions == decisions
        function = decision['function']
        call = {'a': 'a(0)', 'e': 'e(0)', 'l': 'l(1)',
                's': 's(0)', 'c': 'c(0)'}.get(function)
        if function == 'j':
            call = 'j(0,1000)' if decision['line'] == j_guards[0] else 'j(0,1)'
        fault = run(args.bc, model, call + '\n')
        assert fault.returncode != 0 and fault.stderr
        fault_plain, fault_outcomes = strip_probes(fault.stdout)
        assert not fault_plain
        assert (decision['line'], 'true') in fault_outcomes
        modeled.update(fault_outcomes)
    report = dict(schema=1, source_sha256=hashlib.sha256(source.encode()).hexdigest(),
                  scope='test-side interpreted decision outcomes; not compiler MC/DC',
                  certified_results=len(cases), undefined_domain_guard_calls=2,
                  fault_model='each working-scale setter independently clipped to zero',
                  decisions=[])
    for decision in decisions:
        item = dict(decision)
        item['observed'] = [outcome for outcome in ('true', 'false')
                            if (decision['line'], outcome) in outcomes]
        item['modeled'] = [outcome for outcome in ('true', 'false')
                           if (decision['line'], outcome) in modeled]
        missing = set(('true', 'false')) - set(item['observed']) - set(item['modeled'])
        if missing:
            # Positive Taylor terms, positive squaring and a positive
            # reciprocal cannot produce a negative exponential coefficient.
            expected = {'r>=0': {'false'}, 'r<0': {'true'}}
            assert item['function'] == 'e' and missing == expected.get(item['expression']), item
            item['exception'] = 'exponential coefficient remains nonnegative by construction'
        report['decisions'].append(item)
    report['total_outcomes'] = 2 * len(decisions)
    report['observed_outcomes'] = sum(len(item['observed']) for item in report['decisions'])
    report['observed_or_modeled_outcomes'] = sum(
        len(set(item['observed']) | set(item['modeled'])) for item in report['decisions'])
    report['infeasible_outcomes'] = sum(
        2 - len(set(item['observed']) | set(item['modeled']))
        for item in report['decisions'] if item.get('exception'))
    if args.output:
        args.output.write_bytes((json.dumps(report, indent=2, sort_keys=True) + '\n').encode())
    else:
        expected = json.loads((ROOT / 'fixtures/math-library-decisions.json').read_text())
        assert report == expected, 'interpreted decision evidence needs review and regeneration'
    print('Certified {} results; observed {}/{} interpreted decision outcomes'.format(
        len(cases), report['observed_outcomes'], report['total_outcomes']))
    for item in report['decisions']:
        if len(item['observed']) < 2:
            print('Uncovered:', item)


if __name__ == '__main__':
    main()
