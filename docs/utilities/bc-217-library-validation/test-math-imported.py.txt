#!/usr/bin/env python3
"""Run imported math programs with exact enclosure checks on their calls.

Only the imported call sites are redirected to ordinary test-side wrappers.
The production library, its internal calls and VM are unmodified. Original
program outputs are retained but are not compared to another library's rounding.
"""
import argparse
from fractions import Fraction
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT / 'scripts'))
from math_oracle import classify, decimal_fraction

CORPORA = ('exponent', 'log', 'pi', 'arctangent', 'sine', 'cosine', 'bessel',
           'atan-script', 'functions-script')
HELPERS = dict(zip('elascj', 'vwxyzh'))
BEGIN, END = '@GD-MATH-BEGIN ', '@GD-MATH-END '


def instrument(program):
    redirected = '\n'.join(
        line if line.lstrip().startswith('define ') else
        re.sub(r'\b([elascj])\s*\(', lambda match: HELPERS[match[1]] + '(', line)
        for line in program.splitlines()) + '\n'
    wrappers = []
    for function, helper in HELPERS.items():
        arguments = 'n,x' if function == 'j' else 'x'
        trace = 'n\nx' if function == 'j' else 'x'
        wrappers.append('define {}({}) {{\nauto r;\n"{}{} "\n{}\nscale\n'
                        'r={}({})\nr\n"{}"\n0\nreturn(r)\n}}\n'.format(
                            helper, arguments, BEGIN, function, trace, function,
                            arguments, END))
    return ''.join(wrappers) + redirected


def observations(binary, program):
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
    environment.update(LC_ALL='C', BC_ENV_ARGS='', BC_LINE_LENGTH='70')
    result = subprocess.run([binary, '-l'], input=instrument(program),
                            universal_newlines=True, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, env=environment, timeout=300)
    if result.returncode or result.stderr:
        raise AssertionError('imported program failed: ' + result.stderr)
    lines = iter(result.stdout.replace('\\\r\n', '').replace('\\\n', '').splitlines())
    rows = []
    for line in lines:
        if not line.startswith(BEGIN):
            continue
        function, first = line[len(BEGIN):].split(' ', 1)
        arguments = [first]
        if function == 'j':
            arguments.append(next(lines))
        scale, value = int(next(lines)), next(lines)
        if next(lines) != END + '0':
            raise AssertionError('broken imported-call trace')
        rows.append(dict(function=function, arguments=arguments, scale=scale,
                         value=value))
    if not rows:
        raise AssertionError('imported program made no traced mathematical calls')
    return rows


def certify(row, reference):
    if (row['function'], row['scale']) != (reference['function'], reference['scale']):
        raise AssertionError('imported call sequence or entry scale changed')
    actual = [decimal_fraction(value) for value in row['arguments']]
    original = [decimal_fraction(value) for value in reference['arguments']]
    if reference.get('user_redefinition'):
        if actual != original or decimal_fraction(row['value']) != actual[0]:
            raise AssertionError('imported user redefinition did not retain ordinary binding')
        return
    if reference.get('undefined'):
        if actual != original:
            raise AssertionError('undefined-domain input changed')
        return
    # The only computed imported inputs are trigonometric multiples of the
    # program's own approximation to pi. These three functions are globally
    # 1-Lipschitz, so widen the enclosure for the ACTUAL argument. Constant
    # exp/log inputs must agree exactly; no binary float tolerance is used.
    delta = sum(abs(a - b) for a, b in zip(actual, original))
    if len(actual) != len(original) or (delta and row['function'] not in 'sca'):
        raise AssertionError('reference needs regeneration for changed arguments')
    lower, upper = Fraction(reference['lower']) - delta, Fraction(reference['upper']) + delta
    status = classify(decimal_fraction(row['value']), lower, upper, row['scale'])
    if status != 'pass':
        raise AssertionError('imported {} call at scale {}: {}'.format(
            row['function'], row['scale'], status))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    parser.add_argument('corpus', type=Path)
    parser.add_argument('--references', type=Path,
                        default=ROOT / 'fixtures/math-imported-oracles.json')
    parser.add_argument('--record', action='store_true',
                        help='regenerate with the pinned optional FLINT environment')
    args = parser.parse_args()
    if hasattr(sys, 'set_int_max_str_digits'):
        sys.set_int_max_str_digits(1000000)
    if args.record:
        import flint
        if (flint.__version__, flint.__FLINT_VERSION__) != ('0.9.0', '3.6.0'):
            parser.error('regeneration requires python-flint 0.9.0 / FLINT 3.6.0')
        spec = importlib.util.spec_from_file_location('generator',
                                                      ROOT / 'scripts/generate-math-oracles.py')
        generator = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(generator)
        report = dict(schema=1, python_flint_version=flint.__version__,
                      flint_version=flint.__FLINT_VERSION__,
                      generator_sha256=hashlib.sha256(
                          (ROOT / 'scripts/generate-math-oracles.py').read_bytes()).hexdigest(),
                      recording_library_sha256=hashlib.sha256(
                          (ROOT / 'math-library.bc').read_bytes()).hexdigest(),
                      recording_bc_sha256=hashlib.sha256(Path(args.bc).read_bytes()).hexdigest(),
                      source_hash_scope='imported UTF-8 source with CRLF normalized to LF',
                      corpora={})
    else:
        report = json.loads(args.references.read_text(encoding='utf-8'))
    count = undefined = replacements = 0
    for name in CORPORA:
        generated = {'bessel': 'bessel', 'atan-script': 'atan'}
        path = (ROOT / ('fixtures/math-imported-' + generated[name] + '.bc') if name in generated
                else args.corpus / 'scripts/functions.bc' if name == 'functions-script'
                else args.corpus / (name + '.txt'))
        payload = path.read_bytes().replace(b'\r\n', b'\n')
        digest = hashlib.sha256(payload).hexdigest()
        program = payload.decode('utf-8')
        if name == 'functions-script':
            # Howard's bare return expression is an extension. Parenthesize
            # this identity return for the same test in strict POSIX mode.
            program, changed = re.subn(r'(?m)^([ \t]*)return x$', r'\1return(x)', program)
            if changed != 1:
                raise AssertionError('imported identity-return syntax changed')
        if name in generated:
            generator_payload = (args.corpus / ('scripts/' + generated[name] + '.bc')).read_bytes().replace(b'\r\n', b'\n')
            generator_digest = hashlib.sha256(generator_payload).hexdigest()
            # The imported generator ends its generated input with Howard's
            # halt extension. EOF terminates our ordinary-input harness.
            if not program.endswith('halt\n'):
                raise AssertionError('imported generator terminator changed')
            program = program[:-len('halt\n')]
        rows = observations(args.bc, program)
        if args.record:
            references = []
            for index, row in enumerate(rows):
                reference = {key: value for key, value in row.items() if key != 'value'}
                if name == 'functions-script' and index == 1:
                    reference['user_redefinition'] = 'identity replacement of e'
                elif row['function'] == 'l' and decimal_fraction(row['arguments'][0]) <= 0:
                    reference['undefined'] = 'nonpositive logarithm: POSIX domain undefined'
                elif row['function'] == 'j' and decimal_fraction(row['arguments'][0]).denominator != 1:
                    reference['undefined'] = 'noninteger Bessel order: POSIX domain undefined'
                else:
                    reference.update(generator.enclosure(row['function'], row['arguments'],
                                                         row['scale'], 16, 65536))
                references.append(reference)
            report['corpora'][name] = dict(sha256=digest, calls=references)
            if name in generated:
                report['corpora'][name]['generator_sha256'] = generator_digest
        corpus = report['corpora'][name]
        if digest != corpus['sha256'] or len(rows) != len(corpus['calls']):
            raise AssertionError('imported source digest or call count changed: ' + name)
        if name in generated and corpus['generator_sha256'] != generator_digest:
            raise AssertionError('imported source generator changed')
        for row, reference in zip(rows, corpus['calls']):
            certify(row, reference)
            undefined += bool(reference.get('undefined'))
            replacements += bool(reference.get('user_redefinition'))
            count += 1
    if args.record:
        args.references.write_bytes((json.dumps(report, indent=2, sort_keys=True) + '\n').encode())
    print('Checked {} imported calls; {} explicitly undefined-domain calls; '
          '{} ordinary user replacements'.format(count, undefined, replacements))


if __name__ == '__main__':
    main()
