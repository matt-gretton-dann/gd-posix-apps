#!/usr/bin/env python3
"""Run output-checked, bounded and paired bc process benchmarks.

Use --list to inspect the corpus. A JSON configuration supplies named argv lists;
--output receives a new directory containing programs, oracles, raw samples and
provenance. Every warm-up and measured invocation must pass its exact-output
or certified decimal-ULP oracle.
Python 3.6+; no shell command construction or optional numerical dependencies.
SPDX-License-Identifier: Apache-2.0
"""

import argparse
import csv
from datetime import datetime, timezone
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import platform
import random
import re
import shutil
import statistics
import subprocess
import sys
import time
from fractions import Fraction


class BenchmarkError(RuntimeError):
    """Invalid experiment or a failed correctness/process check."""


def oracle_payload(case):
    """Preserve the actual decision input, including mathematical enclosures."""
    if 'oracle' in case:
        return (json.dumps(case['oracle'], sort_keys=True, indent=2) + '\n').encode('utf-8')
    return ('\n'.join(case['expected']) + '\n').encode('utf-8')


def check_output(case, actual, label):
    if 'oracle' not in case:
        expected = canonical_output(oracle_payload(case))
        if actual != expected:
            raise BenchmarkError(label + ' produced incorrect output')
        return
    oracle = case['oracle']
    if oracle.get('kind') != 'decimal-ulp':
        raise BenchmarkError(label + ' has an unknown numerical oracle')
    state = oracle.get('expected_state', [])
    if len(actual) != 1 + len(state):
        raise BenchmarkError(label + ' must produce exactly one mathematical result and its state')
    if actual[1:] != state:
        raise BenchmarkError(label + ' changed the required mathematical result scale or caller state')
    path = Path(__file__).with_name('math_oracle.py')
    spec = importlib.util.spec_from_file_location('bc_benchmark_math_oracle', str(path))
    checker = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(checker)
    try:
        status = checker.classify(checker.decimal_fraction(actual[0]),
                                  Fraction(oracle['lower']), Fraction(oracle['upper']),
                                  oracle['scale'])
    except (KeyError, TypeError, ValueError, ZeroDivisionError) as error:
        raise BenchmarkError(label + ' has an invalid numerical oracle: ' + str(error))
    if status == 'refine':
        raise BenchmarkError(label + ' reference needs refinement at the decimal-ULP boundary')
    if status != 'pass':
        raise BenchmarkError(label + ' exceeds the one-decimal-ULP error bound')


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def digest_file(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def integer_sqrt(value):
    """Exact floor root, also on Python versions predating math.isqrt."""
    if value == 0:
        return 0
    root = 1 << ((value.bit_length() + 1) // 2)
    while True:
        next_root = (root + value // root) // 2
        if next_root >= root:
            return root
        root = next_root


def scaled(coefficient, places):
    if coefficient == 0:
        return "0"
    sign = "-" if coefficient < 0 else ""
    text = str(abs(coefficient))
    if places:
        text = text.zfill(places + 1)
        text = text[:-places] + "." + text[-places:]
    return sign + text


def fibonacci(count):
    first, second = 0, 1
    for _ in range(count):
        first, second = second, first + second
    return first


def math_corpus(preset='full', selected=None, listing=False):
    """Explicit-library one-shot/repeated calls with independent enclosures.

    Fixed counts are deliberately recorded, not claimed calibrated. Large
    argument/precision correctness remains broader than the timing subset.
    Each role's argv chooses -l or an ordinary library file; the harness never
    substitutes a mathematical implementation or special-cases an executable.
    """
    path = Path(__file__).resolve().parents[1] / 'fixtures/math-benchmark-oracles.json'
    references = json.loads(path.read_text(encoding='utf-8'))
    by_key = {(case['id'], case['scale']): case for case in references['cases']}
    scales = [2, 20, 100] if preset == 'quick' else [20, 100, 1000]
    corpus_name = 'holdout' if preset == 'holdout' else 'full'
    regimes = [case for case in references['cases']
               if case['corpus'] == corpus_name and case['scale'] == scales[0]]
    if len(regimes) != 10:
        raise BenchmarkError('mathematical reference regime inventory changed')
    cases, known = [], []
    for origin in regimes:
        function, regime, identifier = origin['function'], origin['regime'], origin['id']
        for scale in scales:
            reference = by_key[identifier, scale]
            arguments = list(reference['arguments'])
            for mode in ('one_shot', 'repeated'):
                count = 1 if mode == 'one_shot' else (2 if preset == 'quick' else
                                                    {20: 50, 100: 20, 1000: 3}[scale])
                if preset == 'holdout' and mode == 'repeated':
                    count = max(2, count - max(1, count // 8))
                name = 'math_{}_{}_scale{}_{}_x{}'.format(
                    regime, identifier, scale, mode, count)
                known.append(name)
                if selected and name not in selected:
                    continue
                call = '{}({})'.format(function, ','.join(arguments))
                program = 'scale={}\n'.format(scale)
                program += ('r=' + call if mode == 'one_shot' else
                            'for(i=0;i<{};i++) r={}'.format(count, call))
                program += '\nr\nscale\nibase\nscale(r)\n'
                cases.append(dict(name=name, group='math_' + function + '_' + regime,
                                  program=program, expected=None,
                                  parameters=dict(function=function, arguments=arguments,
                                                  scale=scale, iterations=count, mode=mode,
                                                  calibration='fixed count; not calibrated'),
                                  oracle=dict(kind='decimal-ulp', scale=scale,
                                              lower=reference['lower'], upper=reference['upper'],
                                              reference_id=identifier,
                                              expected_state=[str(scale), '10', str(scale)])))
    if selected and set(selected) - set(known):
        raise BenchmarkError('unknown workload: ' + ', '.join(sorted(set(selected) - set(known))))
    return cases


def math_iterations(case, count):
    """Rebuild a repeated mathematical program from explicit dimensions."""
    parameters = dict(case['parameters'])
    if parameters.get('mode') != 'repeated' or case.get('oracle', {}).get('kind') != 'decimal-ulp':
        raise BenchmarkError('duration calibration requires a repeated mathematical case')
    if type(count) is not int or count < 2:
        raise BenchmarkError('repeated mathematical count must be at least two')
    parameters['iterations'] = count
    call = '{}({})'.format(parameters['function'], ','.join(parameters['arguments']))
    program = 'scale={}\nfor(i=0;i<{};i++) r={}\nr\nscale\nibase\nscale(r)\n'.format(
        parameters['scale'], count, call)
    name, changed = re.subn(r'_x[0-9]+$', '_x' + str(count), case['name'])
    if changed != 1:
        raise BenchmarkError('mathematical case name does not identify its iteration count')
    return dict(case, name=name, program=program, parameters=parameters)


def calibrate_math(cases, tools, target, samples, maximum, timeout, output):
    """Choose each role's bounded duration count, then freeze their maximum.

    Calibration timings are separate diagnostics. No paired trial is used to
    tune counts, and later shortfall is reported rather than triggering reruns.
    """
    directory = output / 'calibration'
    programs = directory / 'workloads'
    programs.mkdir(parents=True)
    chosen, calibrated = [], []
    with (directory / 'samples.jsonl').open('w', encoding='utf-8') as log:
        for case in cases:
            if case['parameters']['mode'] == 'one_shot':
                calibrated.append(case)
                chosen.append(dict(original=case['name'], status='one-shot latency; not batched'))
                continue
            roles = {}
            for tool in tools:
                count = 2
                while True:
                    trial = math_iterations(case, count)
                    payload = trial['program'].encode('utf-8')
                    (programs / (trial['name'] + '.bc')).write_bytes(payload)
                    (programs / (trial['name'] + '.expected')).write_bytes(oracle_payload(trial))
                    durations = []
                    for sample in range(samples):
                        result = run_checked(tool, trial, timeout)
                        row = dict(result, workload=trial['name'], original=case['name'],
                                   implementation=tool['name'], iterations=count, trial=sample,
                                   program_sha256=hashlib.sha256(payload).hexdigest())
                        log.write(json.dumps(row, sort_keys=True) + '\n')
                        log.flush()
                        durations.append(result['seconds'])
                    median = statistics.median(durations)
                    if median >= target:
                        roles[tool['name']] = dict(iterations=count, median_seconds=median)
                        break
                    if count == maximum:
                        raise BenchmarkError('{} / {} did not reach the duration target at '
                                             '{} iterations'.format(tool['name'], case['name'], count))
                    count = min(maximum, count * 2)
            count = max(role['iterations'] for role in roles.values())
            frozen = math_iterations(case, count)
            frozen['parameters']['calibration'] = 'bounded per-role median duration; common maximum'
            frozen['parameters']['duration_target_seconds'] = target
            calibrated.append(frozen)
            chosen.append(dict(original=case['name'], frozen=frozen['name'], iterations=count,
                               status='frozen before paired trials', roles=roles))
    report = dict(target_seconds=target, samples_per_count=samples, maximum_iterations=maximum,
                  method='independent per-role doubling, common maximum, no paired-data feedback',
                  cases=chosen)
    write_json(directory / 'summary.json', report)
    return calibrated, report


def _replay_corpus(directory, suite, selected=None):
    """Load the exact frozen programs/oracles, including calibrated counts."""
    manifest_path = directory / 'manifest.json'
    manifest = json.loads(manifest_path.read_text(encoding='utf-8'))
    if manifest.get('suite', 'arithmetic') != suite:
        raise BenchmarkError('replayed corpus suite does not match --suite')
    cases, names = [], set()
    for entry in manifest['corpus']:
        name = entry['name']
        if (not isinstance(name, str) or not re.fullmatch(r'[A-Za-z0-9_.-]+', name) or
                name in ('.', '..') or name in names):
            raise BenchmarkError('invalid or duplicate replayed workload name')
        names.add(name)
        if selected and name not in selected:
            continue
        payloads = {}
        for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
            payload = (directory / 'workloads' / (name + suffix)).read_bytes()
            if hashlib.sha256(payload).hexdigest() != entry[field]:
                raise BenchmarkError('replayed artifact hash mismatch: ' + name + suffix)
            payloads[suffix] = payload.decode('utf-8')
        case = dict(name=name, group=entry['group'], parameters=entry['parameters'],
                    program=payloads['.bc'], expected=None)
        if not isinstance(case['parameters'], dict) or not isinstance(case['group'], str):
            raise BenchmarkError('invalid replayed workload dimensions')
        if suite == 'math':
            case['oracle'] = json.loads(payloads['.expected'])
        else:
            case['expected'] = payloads['.expected'].splitlines()
        mode = entry.get('input_mode', 'stdin')
        if mode not in ('stdin', 'file'):
            raise BenchmarkError('unknown replayed input transport')
        if mode == 'file':
            case['input_mode'] = 'file'
        cases.append(case)
    if not cases or (selected and set(selected) - names):
        raise BenchmarkError('empty or unknown replayed workload selection')
    return cases, dict(source_manifest_sha256=digest_file(manifest_path),
                       source_directory=str(directory.resolve()),
                       original_preset=manifest.get('preset'),
                       original_corpus_seed=manifest.get('corpus_seed'),
                       original_reference_fixture_sha256=manifest.get('reference_fixture_sha256'),
                       scope='frozen program/oracle replay; no new duration calibration')


def replay_corpus(directory, suite, selected=None):
    try:
        return _replay_corpus(directory, suite, selected)
    except (OSError, KeyError, TypeError, ValueError) as error:
        raise BenchmarkError('invalid replay corpus: ' + str(error))


def corpus(preset="full", selected=None, listing=False, seed=197200, suite='arithmetic'):
    """Keep the historical sixteen cases, then add explicitly named classes.

    Quick mode reduces both dimensions and iterations; its parameterized names
    and manifest distinguish it from the full baseline. Oracles are evaluated
    only for selected cases, so startup-only validation stays inexpensive.
    """
    if suite == 'math':
        return math_corpus(preset, selected, listing)
    if suite != 'arithmetic':
        raise BenchmarkError('unknown benchmark suite: ' + suite)
    if hasattr(sys, "set_int_max_str_digits"):
        sys.set_int_max_str_digits(1000000)
    rng = random.Random(seed)
    cases, known = [], []
    def iterations(count):
        if preset == "quick":
            return max(1, count // 100)
        return max(1, count - count // 8) if preset == "holdout" else count

    def dimension(count):
        if preset == "quick":
            return max(1, count // 10)
        return count + max(1, count // 8) if preset == "holdout" else count

    def add(name, group, program, oracle, parameters=None):
        known.append(name)
        if selected and name not in selected:
            return
        expected = None if listing else (oracle() if callable(oracle) else oracle)
        if expected is not None:
            expected = ([str(value) for value in expected] if isinstance(expected, list)
                        else [str(expected)])
        cases.append({"name": name, "group": group, "program": program.rstrip() + "\n",
                      "expected": expected, "parameters": parameters or {}})

    def dense(digits):
        return str(rng.randrange(1, 10)) + "".join(str(rng.randrange(10)) for _ in range(digits - 1))

    def multiply(name, left, right, repeats, shape):
        add(name, "multiply", "a={}\nb={}\nfor(i=0;i<{};i++) c=a*b\nc".format(
            left, right, repeats), lambda: int(left) * int(right),
            {"left_digits": len(left), "right_digits": len(right),
             "iterations": repeats, "shape": shape})

    add("startup", "startup", "1+1", 2)
    count = iterations(1000000)
    add("loop_{}".format(count), "loop", "s=0\nfor(i=0;i<{};i++) s+=i\ns".format(count),
        lambda: count * (count - 1) // 2, {"iterations": count})
    count = iterations(200000)
    add("calls_{}".format(count), "calls",
        "define f(x) {\nreturn(x*x+1)\n}\ns=0\nfor(i=0;i<%d;i++) s+=f(i)\ns" % count,
        lambda: count * (count - 1) * (2 * count - 1) // 6 + count, {"iterations": count})
    add("array_{}".format(count), "array",
        "s=0\nfor(i=0;i<%d;i++) a[i]=i\nfor(i=0;i<%d;i++) s+=a[i]\ns" % (count, count),
        lambda: count * (count - 1) // 2, {"elements": count})
    digits, repeats = dimension(1000), iterations(100000)
    left = "7" * digits
    for position, right in (("high", "8" + left[1:]),
                            ("low", left[:-1] + "8"), ("equal", left)):
        add("compare_{}_{}d_x{}".format(position, digits, repeats), "comparison",
            "a={}\nb={}\ns=0\nfor(i=0;i<{};i++) {{if(a<=b) s+=1}}\ns".format(left, right, repeats),
            lambda: repeats, {"digits": digits, "iterations": repeats,
                              "first_unequal_limb": position})
    for size, repeats in ((1000, 100), (10000, 10)):
        size, repeats = dimension(size), iterations(repeats)
        body = Path(__file__).with_name("add-bench.bc").read_text(encoding="utf-8")
        add("fibonacci_{}_x{}".format(size, repeats), "fibonacci",
            body + "\nfor(k=0;k<%d;k++) e=f(%d)\ne" % (repeats, size),
            lambda: fibonacci(size), {"index": size, "iterations": repeats})
    for size, repeats in ((1000, 50), (5000, 5)):
        size, repeats = dimension(size), iterations(repeats)
        body = Path(__file__).with_name("mul-bench.bc").read_text(encoding="utf-8")
        add("factorial_{}_x{}".format(size, repeats), "factorial",
            body + "\nfor(k=0;k<%d;k++) e=f(%d)\ne" % (repeats, size),
            lambda: math.factorial(size), {"argument": size, "iterations": repeats})
    for digits, repeats in ((100, 100000), (1000, 10000), (10000, 1000)):
        digits, repeats = dimension(digits), iterations(repeats)
        multiply("multiply_{}d_x{}".format(digits, repeats), "9" * digits, "8" * digits,
                 repeats, "repeated")
    for places, repeats in ((100, 100000), (1000, 10000), (5000, 5000)):
        places, repeats = dimension(places), iterations(repeats)
        add("divide_scale{}_x{}".format(places, repeats), "divide_single_limb",
            "scale=%d\nfor(i=0;i<%d;i++) c=1/7\nc" % (places, repeats),
            lambda: scaled(10 ** places // 7, places),
            {"scale": places, "iterations": repeats, "divisor": "7"})
    for places, repeats in ((100, 5000), (1000, 50), (5000, 5)):
        places, repeats = dimension(places), iterations(repeats)
        add("sqrt_scale{}_x{}".format(places, repeats), "sqrt",
            "scale=%d\nfor(i=0;i<%d;i++) c=sqrt(2)\nc" % (places, repeats),
            lambda: scaled(integer_sqrt(2 * 10 ** (2 * places)), places),
            {"scale": places, "iterations": repeats})

    for digits, repeats in ((1000, 5000), (10000, 500)):
        digits, repeats = dimension(digits), iterations(repeats)
        multiply("multiply_dense_{}d_x{}".format(digits, repeats), dense(digits), dense(digits),
                 repeats, "seeded_dense")
    large, small, repeats = dimension(10000), dimension(100), iterations(2000)
    multiply("multiply_unbalanced_{}d_{}d_x{}".format(large, small, repeats),
             dense(large), dense(small), repeats, "unbalanced")
    digits, repeats = dimension(10000), iterations(1000)
    sparse = "1" + "0" * (digits - 2) + "1"
    multiply("multiply_sparse_{}d_x{}".format(digits, repeats), sparse, sparse,
             repeats, "sparse")
    for left_size, right_size, places, repeats in ((900, 450, 100, 1000),
                                                  (1000, 900, 1000, 500)):
        left_size, right_size = dimension(left_size), dimension(right_size)
        places, repeats = dimension(places), iterations(repeats)
        left, right = dense(left_size), dense(right_size)
        add("divide_multilimb_{}d_{}d_s{}_x{}".format(left_size, right_size, places, repeats),
            "divide_multilimb", "scale={}\na={}\nb={}\nfor(i=0;i<{};i++) c=a/b\nc".format(
                places, left, right, repeats),
            lambda: scaled(int(left) * 10 ** places // int(right), places),
            {"dividend_digits": left_size, "divisor_digits": right_size,
             "scale": places, "iterations": repeats})
    left_size, right_size, repeats = dimension(1000), dimension(350), iterations(3000)
    left, right = dense(left_size), dense(right_size)
    add("modulo_{}d_{}d_x{}".format(left_size, right_size, repeats), "modulo",
        "a={}\nb={}\nfor(i=0;i<{};i++) c=a%b\nc".format(left, right, repeats),
        lambda: int(left) % int(right), {"dividend_digits": left_size,
                                       "divisor_digits": right_size, "iterations": repeats})
    for digits, exponent, repeats in ((100, 17, 1000), (20, 32, 2000)):
        digits, repeats = dimension(digits), iterations(repeats)
        left = dense(digits)
        add("power_{}d_e{}_x{}".format(digits, exponent, repeats), "power",
            "a={}\nfor(i=0;i<{};i++) c=a^{}\nc".format(left, repeats, exponent),
            lambda: int(left) ** exponent,
            {"digits": digits, "exponent": exponent, "iterations": repeats})
    count = iterations(20000)
    add("ibase_literal_flip_{}".format(count), "literal_base",
        "define f() {\nreturn(10)\n}\ns=0\nfor(i=0;i<%d;i++) {\n"
        "ibase=2\ns+=f()\nibase=A\ns+=f()\n}\ns" % count,
        lambda: 12 * count, {"iterations": count, "bases": [2, 10]})
    depth, repeats = dimension(50), iterations(1000)
    add("recursion_{}_x{}".format(depth, repeats), "recursion",
        "define f(n) {\nif(n<=1) return(1)\nreturn(n*f(n-1))\n}\n"
        "for(i=0;i<%d;i++) a=f(%d)\na" % (repeats, depth),
        lambda: math.factorial(depth), {"depth": depth, "iterations": repeats})
    count = iterations(200000)
    add("sparse_array_{}".format(count), "array_sparse",
        "s=0\nfor(i=0;i<%d;i++) {\nk=(i%%31)*313\na[k]=i\ns+=a[k]\n}\ns" % count,
        lambda: count * (count - 1) // 2, {"iterations": count, "keys": 31, "stride": 313})
    count = iterations(2000)
    add("frontend_definitions_{}".format(count), "frontend_definition",
        "define f(x) {\nreturn(x*x+1)\n}\n" * count + "1", 1, {"definitions": count})
    digits, count = dimension(4000), iterations(100)
    value = dense(digits)
    add("decimal_input_{}d_x{}".format(digits, count), "decimal_input",
        ("a=" + value + "\n") * count + "a", value,
        {"digits": digits, "literals": count})
    digits, count = dimension(200), iterations(1000)
    value = dense(digits)
    add("decimal_output_{}d_x{}".format(digits, count), "decimal_output",
        "a={}\nfor(i=0;i<{};i++) a".format(value, count), lambda: [value] * count,
        {"digits": digits, "lines": count})
    count = iterations(20000)
    add("streaming_statements_{}".format(count), "streaming",
        "i=0\n" + "i+=1\ni\n" * count, lambda: list(range(1, count + 1)),
        {"statements": count * 2 + 1, "output_lines": count, "termination": "EOF"})
    # Append square cases so earlier seeded operands and hashes stay stable.
    for size, repeat_count in ((100, 100000), (1000, 10000), (10000, 500)):
        digits, count = dimension(size), iterations(repeat_count)
        value = dense(digits)
        add("square_{}d_x{}".format(digits, count), "square",
            "a={}\nfor(i=0;i<{};i++) c=a*a\nc".format(value, count),
            lambda value=value: int(value) ** 2,
            {"digits": digits, "iterations": count, "shape": "aliased"})
    # A single persistent literal executes repeatedly, including COW mutation
    # of each returned snapshot. No extra RNG draws change earlier programs.
    digits, count = dimension(2000), iterations(2000)
    value = "7" * digits
    add("literal_repeat_{}d_x{}".format(digits, count), "literal_repeat",
        "for(i=0;i<{};i++) {{\na={}\na+=1\n}}\na".format(count, value),
        lambda: int(value) + 1, {"digits": digits, "iterations": count})
    # Append output-heavy nondecimal classes without perturbing earlier seeds.
    # Separate groups keep both bases in every representative native PR subset.
    for label, output_base, code in (('binary', 2, 'b'), ('octal', 8, 'o')):
        digits, count = dimension(1000), iterations(300)
        value = dense(digits)
        add("{}_output_{}d_x{}".format(label, digits, count), label + "_output",
            "a={}\nobase={}\nfor(i=0;i<{};i++) a".format(value, output_base, count),
            lambda value=value, code=code, count=count: [format(int(value), code)] * count,
            {"digits": digits, "lines": count, "output_base": output_base})
    # Append cold source literals above the balanced-input dispatch boundary.
    # Repeating statements constructs separate instruction caches; a loop around
    # one literal would mainly measure cache hits rather than radix conversion.
    for label, input_base, expression, alphabet in (
            ('binary', 2, '2', '01'), ('hex', 16, 'A+6', '0123456789ABCDEF')):
        digits, count = dimension(100000), iterations(3)
        value = alphabet[-1] + ''.join(rng.choice(alphabet) for _ in range(digits - 1))
        add("{}_input_{}d_x{}".format(label, digits, count), label + "_input",
            "ibase={}\n".format(expression) + ("a=" + value + "\n") * count + "ibase=A\na",
            lambda value=value, input_base=input_base: int(value, input_base),
            {"digits": digits, "literals": count, "input_base": input_base})
    # A long left-associated expression exposes otherwise unique intermediate
    # coefficients. Append it after existing classes to preserve their hashes.
    digits, count = dimension(1000), iterations(50000)
    value = '7' * digits
    add("temporary_chain_{}d_x{}".format(digits, count), "temporary_chain",
        "define g(x) {{\nreturn(x+x+x+x+x+x+x+x)\n}}\na={}\n"
        "for(i=0;i<{};i++) r=g(a)\nr".format(value, count),
        lambda value=value: 8 * int(value),
        {"digits": digits, "iterations": count, "terms": 8})
    # File input is a separate class: an argv artifact with closed stdin,
    # not a stdin execution relabeled as file throughput. Append after existing
    # cases so their source/oracle dictionaries and RNG consumption stay stable.
    count = iterations(20000)
    file_case = "file_statements_{}".format(count)
    add(file_case, "file_streaming", "s=0\n" + "s+=1\n" * count + "s",
        count, {"statements": count})
    if cases and cases[-1]['name'] == file_case:
        cases[-1]['input_mode'] = 'file'
    # Append expanded sqrt regimes after every historical case. No RNG draws
    # or dictionary changes perturb the preceding full/held-out/quick corpus.
    places, count = dimension(10000), iterations(5)
    add("sqrt_large_scale{}_x{}".format(places, count), "sqrt_large_scale",
        "scale={}\nfor(i=0;i<{};i++) c=sqrt(2)\nc".format(places, count),
        lambda: scaled(integer_sqrt(2 * 10 ** (2 * places)), places),
        {"scale": places, "iterations": count, "radicand": "2"})
    # One large tiny-fraction root already gives a substantive trial; ten
    # repeats exceed the unchanged 30-second Howard deadline on Windows.
    for original_scale, repetitions in ((1024, 30), (4096, 1)):
        # Keep the input scale even so the exact root is a finite decimal in
        # every preset; report the actual adjusted dimension in its name.
        places = 2 * max(1, dimension(original_scale) // 2)
        count = iterations(repetitions)
        value = "." + "0" * (places - 1) + "4"
        add("sqrt_tiny_scale{}_x{}".format(places, count), "sqrt_tiny",
            "scale=0\nx={}\nfor(i=0;i<{};i++) c=sqrt(x)\nc".format(value, count),
            lambda places=places: scaled(2 * 10 ** (places // 2), places),
            {"input_scale": places, "scale": 0, "iterations": count,
             "coefficient": "4"})
    root_power, places, count = dimension(2000), dimension(100), iterations(10)
    value = "4" + "0" * (2 * root_power)
    add("sqrt_integer_power{}_scale{}_x{}".format(root_power, places, count),
        "sqrt_large_integer",
        "scale={}\nx={}\nfor(i=0;i<{};i++) c=sqrt(x)\nc".format(places, value, count),
        lambda: scaled(2 * 10 ** (root_power + places), places),
        {"root_power": root_power, "input_digits": len(value),
         "scale": places, "iterations": count, "coefficient": "4"})
    # Append scaled-numerator division after historical cases. A separate RNG
    # stream keeps all earlier operands and exact oracle payloads unchanged.
    fraction_digits = dimension(1000)
    repeats = iterations(5000)
    fractional_rng = random.Random(seed ^ 368)
    whole, divisor = (2469, 13) if preset == 'holdout' else (1234, 7)
    for shape in ('dense', 'trailing_zero'):
        fraction = (''.join(str(fractional_rng.randrange(10)) for _ in range(fraction_digits))
                    if shape == 'dense' else '0' * fraction_digits)
        value = str(whole) + '.' + fraction
        coefficient = int(str(whole) + fraction)
        # Equal precision is first so native PR selection includes the scalar
        # divisor opportunity; below/above precision remain in broad matrices.
        for places in (fraction_digits, fraction_digits // 2, 2 * fraction_digits):
            name = 'divide_scaled_{}_s{}_t{}_x{}'.format(shape, fraction_digits, places, repeats)
            add(name, 'divide_scaled_' + shape,
                'scale={}\na={}\nfor(i=0;i<{};i++) c=a/{}\nc'.format(
                    places, value, repeats, divisor),
                lambda coefficient=coefficient, places=places: scaled(
                    coefficient * 10 ** places // (divisor * 10 ** fraction_digits), places),
                dict(input_scale=fraction_digits, scale=places, iterations=repeats,
                     divisor=str(divisor), shape=shape, coefficient_digits=len(str(coefficient))))
    if selected:
        missing = set(selected) - set(known)
        if missing:
            raise BenchmarkError("unknown workload: " + ", ".join(sorted(missing)))
    return cases


def canonical_output(data):
    text = data.decode("utf-8").replace("\\\r\n", "").replace("\\\n", "")
    result = []
    for line in text.splitlines():
        if not re.fullmatch(r"-?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+)", line):
            raise ValueError("non-numeric output")
        negative = line.startswith("-")
        if negative:
            line = line[1:]
        integer, dot, fraction = line.partition(".")
        integer = integer.lstrip("0") or "0"
        zero = integer == "0" and (not fraction or set(fraction) == {"0"})
        result.append("0" if zero else ("-" if negative else "") + integer + dot + fraction)
    return result


def environment():
    result = {key: value for key, value in os.environ.items()
              if not key.startswith(("BC_", "DC_"))}
    result.update(LC_ALL="C", LANG="C", BC_LINE_LENGTH="70", BC_ENV_ARGS="")
    return result


def run_checked(implementation, case, timeout, program_path=None):
    label = implementation["name"] + " / " + case["name"]
    for artifact in implementation.get('input_artifacts', []):
        try:
            observed = digest_file(artifact['realpath'])
        except OSError as error:
            raise BenchmarkError(label + ' cannot read declared input artifact: ' + str(error))
        if observed != artifact['sha256']:
            raise BenchmarkError(label + ' declared input artifact changed before invocation')
    payload, child_environment = case["program"].encode("utf-8"), environment()
    argv = list(implementation["argv"])
    mode = case.get("input_mode", "stdin")
    if mode == "file":
        if program_path is None:
            raise BenchmarkError(label + " requires a staged file artifact")
        try:
            artifact = Path(program_path).read_bytes()
        except OSError as error:
            raise BenchmarkError(label + " cannot read file artifact: " + str(error))
        if artifact != payload:
            raise BenchmarkError(label + " file artifact does not match workload program")
        argv.append(str(program_path))
        payload = b""  # EOF after file input; never wait for a second input source.
    elif mode != "stdin":
        raise BenchmarkError(label + " has unknown input mode: " + str(mode))
    # Staging/validation is outside timing. File trials therefore use cached
    # artifact bytes, not a cold-filesystem or disk-bandwidth experiment.
    started = time.perf_counter()
    try:
        child = subprocess.run(argv, input=payload,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               env=child_environment, timeout=timeout)
    except subprocess.TimeoutExpired:
        # subprocess.run kills and waits for its direct child on timeout.
        raise BenchmarkError(label + " timed out after {} seconds".format(timeout))
    except OSError as error:
        raise BenchmarkError(label + " could not start: " + str(error))
    elapsed = time.perf_counter() - started
    if child.returncode:
        raise BenchmarkError(label + " exited with status " + str(child.returncode))
    if child.stderr:
        raise BenchmarkError(label + " wrote to stderr: " +
                             child.stderr.decode("utf-8", errors="replace")[:500])
    try:
        actual = canonical_output(child.stdout)
    except (UnicodeError, ValueError):
        raise BenchmarkError(label + " produced incorrect output (not numeric)")
    check_output(case, actual, label)
    return {"seconds": elapsed, "output_bytes": len(child.stdout), "checked": True,
            "output_sha256": hashlib.sha256(child.stdout).hexdigest()}


def quantile(values, fraction):
    ordered = sorted(values)
    position = (len(ordered) - 1) * fraction
    low = int(position)
    weight = position - low
    return ordered[low] * (1 - weight) + ordered[min(low + 1, len(ordered) - 1)] * weight


def paired_statistics(candidate, reference, seed, resamples=2000):
    if not candidate or len(candidate) != len(reference):
        raise BenchmarkError("paired measurements must have equal nonzero lengths")
    if any(not math.isfinite(value) or value <= 0
           for values in (candidate, reference) for value in values):
        raise BenchmarkError("paired measurements must be finite and positive")
    if resamples < 200:
        raise BenchmarkError("at least 200 bootstrap resamples are required")
    ratios = [left / right for left, right in zip(candidate, reference)]
    if any(not math.isfinite(value) for value in ratios):
        raise BenchmarkError('derived paired ratios must be finite')
    result = {"pairs": len(ratios), "median_paired_ratio": statistics.median(ratios),
              "ratio_of_medians": statistics.median(candidate) / statistics.median(reference),
              'ci95': None, 'iid_ci95': None, 'block_ci95': None,
              'block_length': None, 'autocorrelation_lag1': None,
              'method': 'union of paired IID and circular-block percentile intervals'}
    if len(ratios) >= 3:
        rng = random.Random(seed)
        bootstraps = [statistics.median([ratios[rng.randrange(len(ratios))]
                                        for _ in ratios]) for _ in range(resamples)]
        result['iid_ci95'] = [quantile(bootstraps, 0.025), quantile(bootstraps, 0.975)]
        # Preserve neighboring rounds when resampling. The predeclared block
        # length grows with sample count; it is not tuned to an observed win.
        length = max(2, int(math.sqrt(len(ratios))))
        result['block_length'] = length
        blocks = []
        for _ in range(resamples):
            sample = []
            while len(sample) < len(ratios):
                start = rng.randrange(len(ratios))
                sample.extend(ratios[(start + offset) % len(ratios)] for offset in range(length))
            blocks.append(statistics.median(sample[:len(ratios)]))
        result['block_ci95'] = [quantile(blocks, 0.025), quantile(blocks, 0.975)]
        result['ci95'] = [min(result['iid_ci95'][0], result['block_ci95'][0]),
                          max(result['iid_ci95'][1], result['block_ci95'][1])]
        # Correlation is scale invariant. Normalize before squaring so a
        # finite but large ratio cannot overflow this diagnostic's variance.
        largest = max(ratios)
        normalized = [value / largest for value in ratios]
        mean = statistics.mean(normalized)
        variance = sum((value - mean) ** 2 for value in normalized)
        result['autocorrelation_lag1'] = (sum((left - mean) * (right - mean)
                                             for left, right in zip(normalized[1:], normalized[:-1])) /
                                          variance if variance else 0.0)
        result['interval_limitations'] = ('Nominal percentile intervals; small samples and '
                                           'nonstationary host noise remain limitations. '
                                           'The union cannot be narrower than the IID interval.')
    else:
        result["interval_status"] = "insufficient_pairs"
    return result


def dispersion(values):
    median = statistics.median(values)
    return {"samples": len(values), "median_seconds": median, "min_seconds": min(values),
            "max_seconds": max(values),
            "mad_seconds": statistics.median([abs(value - median) for value in values])}


def implementations(path, require_provenance=False):
    try:
        entries = json.loads(path.read_text(encoding="utf-8"))["implementations"]
    except (OSError, ValueError, KeyError, TypeError) as error:
        raise BenchmarkError("invalid implementation configuration: " + str(error))
    if not isinstance(entries, list) or not entries:
        raise BenchmarkError("configuration needs at least one implementation")
    result, names = [], set()
    for entry in entries:
        if not isinstance(entry, dict) or not isinstance(entry.get("name"), str):
            raise BenchmarkError("each implementation needs a name and argv list")
        name = entry["name"]
        if not name or name in names:
            raise BenchmarkError("duplicate implementation or empty name: " + name)
        names.add(name)
        argv = entry.get("argv")
        if not isinstance(argv, list) or not argv or any(not isinstance(arg, str) for arg in argv):
            raise BenchmarkError(name + " needs a nonempty string argv list")
        binary = shutil.which(argv[0])
        if not binary:
            raise BenchmarkError(name + " executable not found: " + argv[0])
        metadata = entry.get("metadata", {})
        if not isinstance(metadata, dict):
            raise BenchmarkError(name + " metadata must be an object")
        if metadata.get("instrumented"):
            raise BenchmarkError(name + " is instrumented; use a separate timing binary")
        binary_sha256 = digest_file(binary)
        declarations = (metadata.get('binary_sha256'), entry.get('binary_sha256'))
        for declared in declarations:
            if declared is None:
                continue
            if not isinstance(declared, str) or not re.fullmatch('[0-9a-fA-F]{64}', declared):
                raise BenchmarkError(name + ' binary identity hash must be 64 hexadecimal characters')
            if declared.lower() != binary_sha256:
                raise BenchmarkError(name + ' binary identity hash does not match the executable')
        provenance_fields = ("source_revision", "compiler", "flags", "version")
        complete = (all(metadata.get(key) for key in provenance_fields) and
                    metadata.get("instrumented") is False and
                    any(declared is not None for declared in declarations))
        role = entry.get("role", "candidate")
        if require_provenance and role != "installed" and not complete:
            raise BenchmarkError(name + ' lacks source/compiler/flags/version provenance '
                                 'or a matching declared binary identity hash')
        declared_inputs = entry.get('input_artifacts', [])
        if not isinstance(declared_inputs, list):
            raise BenchmarkError(name + ' input_artifacts must be a list')
        artifacts = []
        for artifact in declared_inputs:
            if (not isinstance(artifact, dict) or not isinstance(artifact.get('path'), str) or
                    not isinstance(artifact.get('sha256'), str) or
                    not re.fullmatch('[0-9a-fA-F]{64}', artifact['sha256'])):
                raise BenchmarkError(name + ' needs declared input paths and SHA-256 identities')
            actual_path = Path(artifact['path']).resolve()
            present = False
            for arg in argv[1:]:
                try:
                    present = Path(arg).resolve() == actual_path
                except (OSError, ValueError):
                    # Command arguments can be programs or arbitrary values,
                    # not just legal filesystem names.
                    continue
                if present:
                    break
            if not present:
                raise BenchmarkError(name + ' declared input must appear in its actual argv')
            if digest_file(actual_path) != artifact['sha256'].lower():
                raise BenchmarkError(name + ' declared input artifact hash does not match')
            artifacts.append(dict(path=artifact['path'], realpath=str(actual_path),
                                  sha256=artifact['sha256'].lower(), purpose=artifact.get('purpose', 'input')))
        result.append({"name": name, "argv": [str(Path(binary).resolve())] + argv[1:],
                       "binary_sha256": binary_sha256, "metadata": metadata,
                       "role": role, "complete_provenance": complete,
                       **({'input_artifacts': artifacts} if artifacts else {})})
    return result


def summarize(cases, rows, reference, seed, resamples):
    workloads = []
    for index, case in enumerate(cases):
        values = {}
        for row in rows:
            if row["workload"] == case["name"] and row["phase"] == "measurement":
                values.setdefault(row["implementation"], []).append(row["seconds"])
        comparisons = {}
        for name in values:
            if name != reference:
                comparisons[name] = paired_statistics(values[name], values[reference],
                                                       seed + index, resamples)
        durations = {name: dispersion(times) for name, times in values.items()}
        target = case['parameters'].get('duration_target_seconds')
        adequacy = ({name: detail['median_seconds'] >= target for name, detail in durations.items()}
                    if target is not None else None)
        workloads.append({"name": case["name"], "group": case["group"],
                          "parameters": case["parameters"],
                          "implementations": durations,
                          **({'duration_target_met': adequacy,
                              'duration_status': ('target met' if all(adequacy.values()) else
                                                  'shortfall; not substantive duration evidence')}
                             if adequacy is not None else {}),
                          "candidate_over_reference": comparisons})
    return {"schema": 1, "reference": reference, "workloads": workloads,
            "performance_gate": "advisory; no aggregate or hard timing threshold"}


def positive_integer(value):
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError("must be positive")
    return number


def nonnegative_integer(value):
    number = int(value)
    if number < 0:
        raise argparse.ArgumentTypeError("must be nonnegative")
    return number


def positive_timeout(value):
    number = float(value)
    if not math.isfinite(number) or number <= 0:
        raise argparse.ArgumentTypeError("must be finite and positive")
    return number


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, help="JSON implementations with named argv lists")
    parser.add_argument("--output", type=Path, help="new experiment directory")
    parser.add_argument("--list", action="store_true", help="list case names and parameters")
    parser.add_argument("--preset", choices=("full", "quick", "holdout"), default="full")
    parser.add_argument('--suite', choices=('arithmetic', 'math'), default='arithmetic',
                        help='math requires each configured argv to load its chosen library')
    parser.add_argument("--workload", action="append", help="select an exact name; repeatable")
    parser.add_argument("--reference", help="reference implementation (default: first configured)")
    parser.add_argument("--samples", type=positive_integer, default=9)
    parser.add_argument("--warmups", type=nonnegative_integer, default=1)
    parser.add_argument("--timeout", type=positive_timeout, default=30)
    parser.add_argument("--seed", type=int, default=197200, help="schedule/bootstrap seed")
    parser.add_argument("--corpus-seed", type=int, default=197200, help="operand-generation seed")
    parser.add_argument("--bootstrap", type=positive_integer, default=2000)
    parser.add_argument("--require-provenance", action="store_true")
    parser.add_argument('--calibrate-seconds', type=positive_timeout,
                        help='target per-role median duration for repeated math cases')
    parser.add_argument('--calibration-samples', type=positive_integer, default=3)
    parser.add_argument('--calibration-max-iterations', type=positive_integer, default=1048576)
    parser.add_argument('--calibrate-only', action='store_true',
                        help='prepare the calibrated corpus without paired timing samples')
    parser.add_argument('--replay-corpus', type=Path,
                        help='reuse an experiment directory\'s exact hashed programs and oracles')
    args = parser.parse_args()
    if args.list:
        try:
            cases = (replay_corpus(args.replay_corpus, args.suite, args.workload)[0]
                     if args.replay_corpus else
                     corpus(args.preset, args.workload, listing=True, seed=args.corpus_seed,
                            suite=args.suite))
        except BenchmarkError as error:
            parser.error(str(error))
        print(json.dumps([{key: case[key] for key in ("name", "group", "parameters")}
                          for case in cases], indent=2))
        return 0
    if not args.config or not args.output:
        parser.error("--config and --output are required unless --list is used")
    if args.bootstrap < 200:
        parser.error("--bootstrap must be at least 200")
    if args.calibrate_only and args.calibrate_seconds is None:
        parser.error('--calibrate-only requires --calibrate-seconds')
    if args.replay_corpus and args.calibrate_seconds is not None:
        parser.error('replaying frozen counts and new duration calibration are mutually exclusive')
    if args.calibrate_seconds is not None:
        if args.suite != 'math':
            parser.error('duration calibration currently supports only the explicit math suite')
        if args.calibrate_seconds >= args.timeout or args.calibration_max_iterations < 2:
            parser.error('calibration needs a target below the process timeout and a count cap >=2')
    output = args.output.resolve()
    try:
        output.mkdir(parents=True, exist_ok=False)
    except OSError as error:
        parser.error("cannot create a fresh output directory: " + str(error))
    try:
        tools = implementations(args.config, args.require_provenance)
        for tool_index, tool in enumerate(tools):
            for input_index, artifact in enumerate(tool.get('input_artifacts', [])):
                directory = output / 'declared-inputs'
                directory.mkdir(exist_ok=True)
                archived = directory / '{}-{}-{}'.format(
                    tool_index, input_index, Path(artifact['realpath']).name)
                archived.write_bytes(Path(artifact['realpath']).read_bytes())
                if digest_file(archived) != artifact['sha256']:
                    raise BenchmarkError(tool['name'] + ' declared input changed while archiving')
                artifact['archived_as'] = str(archived.relative_to(output))
        # Keep enough identity information to reproduce a failed bounded
        # calibration even when no final measurement manifest can be written.
        write_json(output / 'setup.json', dict(
            scope='validated input snapshot before calibration or paired trials',
            implementations=tools, source_config_sha256=digest_file(args.config),
            harness_sha256=digest_file(__file__), requested_suite=args.suite,
            requested_preset=args.preset, timeout=args.timeout,
            calibration_target_seconds=args.calibrate_seconds,
            calibration_samples=args.calibration_samples,
            calibration_max_iterations=args.calibration_max_iterations))
        reference = args.reference or tools[0]["name"]
        if reference not in [tool["name"] for tool in tools]:
            raise BenchmarkError("reference is not a configured implementation")
        if args.require_provenance and next(tool for tool in tools
                                           if tool["name"] == reference)["role"] == "installed":
            raise BenchmarkError("an installed comparator cannot be the controlled reference")
        replay = None
        if args.replay_corpus:
            cases, replay = replay_corpus(args.replay_corpus, args.suite, args.workload)
        else:
            cases = corpus(args.preset, args.workload, seed=args.corpus_seed, suite=args.suite)
        calibration = None
        if args.calibrate_seconds is not None:
            cases, calibration = calibrate_math(cases, tools, args.calibrate_seconds,
                                                args.calibration_samples,
                                                args.calibration_max_iterations,
                                                args.timeout, output)
        programs = output / "workloads"
        programs.mkdir()
        for case in cases:
            # Preserve exactly the UTF-8 LF bytes hashed below and sent to the
            # child; native text-mode newline translation would change artifacts.
            (programs / (case["name"] + ".bc")).write_bytes(case["program"].encode("utf-8"))
            (programs / (case["name"] + ".expected")).write_bytes(
                oracle_payload(case))
        clock = time.get_clock_info("perf_counter")
        manifest = {"schema": 1, "started_utc": datetime.now(timezone.utc).isoformat(),
                    "implementations": tools, "reference": reference,
                    "preset": replay['original_preset'] if replay else args.preset,
                    'suite': args.suite,
                    "samples": args.samples, "warmups": args.warmups, "timeout": args.timeout,
                    "seed": args.seed,
                    "corpus_seed": replay['original_corpus_seed'] if replay else args.corpus_seed,
                    "harness_sha256": digest_file(__file__),
                    "bootstrap_resamples": args.bootstrap,
                    "platform": {"system": platform.system(), "release": platform.release(),
                                 "machine": platform.machine(), "python": platform.python_version()},
                    "clock": {"implementation": clock.implementation, "resolution": clock.resolution,
                              "monotonic": clock.monotonic},
                    "environment": {"LC_ALL": "C", "LANG": "C", "BC_LINE_LENGTH": "70",
                                    "BC_ENV_ARGS": ""},
                    "pending_classes": ([] if args.suite == 'math' else
                                        [{"group": "posix_math_library", "issue": 217,
                                          "status": "not selected; not a performance pass"}]),
                    "corpus": [{"name": case["name"], "group": case["group"],
                                "parameters": case["parameters"],
                                **({"input_mode": "file"} if case.get("input_mode") == "file" else {}),
                                "oracle_sha256": hashlib.sha256(
                                    oracle_payload(case)).hexdigest(),
                                "program_sha256": hashlib.sha256(
                                    case["program"].encode("utf-8")).hexdigest()}
                               for case in cases]}
        if args.suite == 'math':
            scripts = Path(__file__).resolve().parent
            manifest['oracle_checker_sha256'] = digest_file(scripts / 'math_oracle.py')
            manifest['reference_fixture_sha256'] = (replay['original_reference_fixture_sha256']
                if replay else digest_file(scripts.parent / 'fixtures/math-benchmark-oracles.json'))
        if calibration is not None:
            manifest['duration_calibration'] = calibration
        if replay is not None:
            manifest['replayed_corpus'] = replay
        write_json(output / "manifest.json", manifest)
        if args.calibrate_only:
            write_json(output / 'calibration-only.json', dict(status='prepared; no paired samples',
                                                             workloads=len(cases)))
            print('Prepared {} calibrated workloads without paired samples'.format(len(cases)))
            return 0
        rows, rng = [], random.Random(args.seed)
        fields = ("phase", "round", "order", "workload", "implementation", "seconds",
                  "checked", "output_bytes", "output_sha256")
        with (output / "samples.jsonl").open("w", encoding="utf-8") as raw, \
                (output / "samples.csv").open("w", newline="", encoding="utf-8") as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=fields)
            writer.writeheader()
            for phase, rounds in (("warmup", args.warmups), ("measurement", args.samples)):
                for round_number in range(rounds):
                    round_cases = list(cases)
                    rng.shuffle(round_cases)
                    for case in round_cases:
                        block = list(tools)
                        rng.shuffle(block)
                        for order, tool in enumerate(block):
                            row = run_checked(tool, case, args.timeout,
                                              programs / (case["name"] + ".bc"))
                            row.update(phase=phase, round=round_number, order=order,
                                       workload=case["name"], implementation=tool["name"])
                            rows.append(row)
                            raw.write(json.dumps(row, sort_keys=True) + "\n")
                            raw.flush()
                            writer.writerow(row)
                            csv_file.flush()
        summary = summarize(cases, rows, reference, args.seed, args.bootstrap)
        write_json(output / "summary.json", summary)
        print("Checked {} workloads across {} implementations; results: {}".format(
            len(cases), len(tools), output))
        return 0
    except (BenchmarkError, OSError, ValueError) as error:
        write_json(output / "failure.json", {"error": str(error), "validation": "failed"})
        print("Benchmark failed: " + str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
