#!/usr/bin/env python3
"""Compare matched software counters and native cold-process RSS, never elapsed time.

SPDX-License-Identifier: Apache-2.0
"""
import argparse
import copy
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import platform
import random
import re
import statistics
import sys


SCRIPTS = Path(__file__).resolve().parent
ROLES = ('base', 'candidate')
COUNTERS = ('allocation_calls', 'deallocation_calls', 'allocated_bytes',
            'live_bytes', 'peak_live_bytes')


def load(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n', encoding='utf-8')


def frame_cases(preset):
    """Finite warm reuse with different plans and identical escaped array state.

    Array parameters prevent treating these as scalar-only function entries.
    Tail sizes and rounds are fixed before any native resource observation.
    """
    if preset not in ('full', 'holdout'):
        raise ValueError('unknown frame diagnostic preset')
    tails = [1, 2, 4, 8, 16, 32, 64, 128] if preset == 'full' else [3, 5, 9, 17, 33, 65, 129, 257]
    rounds = 256 if preset == 'full' else 224
    functions = list('fghjklmn')
    definitions = []
    for name, count in zip(functions, tails):
        definitions.append('define ' + name + '(x,a[]) {\nif (x) return(x)\n'
                           'a[0]=0\n' + 'x=x+1\n' * count + 'return(x)\n}\n')
    cases = []
    for path, argument in (('early_return', 1), ('dense', 0)):
        calls = functions + functions[::-1]
        source = ''.join(definitions) + 'a[0]=7\ns=0\nfor(i=0;i<' + str(rounds) + ';i++) {\n'
        source += ''.join('s+=' + name + '(' + str(argument) + ',a[])\n' for name in calls)
        source += '}\ns\na[0]\n'
        result = 2 * rounds * (len(tails) if argument else sum(tails))
        cases.append(dict(name='resource_frame_' + preset + '_' + path, group='frame_reuse',
                          program=source, expected=[str(result), '7'],
                          parameters=dict(path=path, tail_additions=tails, rounds=rounds,
                                          scope='finite ascending/descending frame reuse with the platform layout default; not an elapsed sample')))
    return cases


def private_frame_cases(preset):
    """Closed scalar bodies with fixed narrow/wide recursion and reuse.

    Unused autos still belong to each activation. Both visitation orders
    expose retained arena padding and historical frame-width growth.
    """
    if preset not in ('full', 'holdout'):
        raise ValueError('unknown private frame diagnostic preset')
    depth, rounds = (24, 16) if preset == 'full' else (21, 14)
    widths = (1, 13, 26)
    names = 'fgh'
    letters = [c for c in 'abcdefghijklmnopqrstuvwxyz' if c != 'x']
    definitions = []
    for name, width in zip(names, widths):
        autos = 'auto ' + ','.join(letters[:width-1]) + '\n' if width>1 else ''
        definitions.append('define '+name+'(x) {\n'+autos+
            'if(x==0) return(0)\nreturn('+name+'(x-1)+1)\n}\n')
    cases=[]
    for order in ('ascending', 'descending'):
        sequence=names if order=='ascending' else names[::-1]
        program=''.join(definitions)+'s=0\nfor(i=0;i<'+str(rounds)+';i++) {\n'
        program+=''.join('s+='+name+'('+str(depth)+')\n' for name in sequence)+'}\ns\n'
        cases.append(dict(name='resource_private_frames_'+preset+'_'+order,group='private_frame_reuse',
            program=program,expected=[str(rounds*len(names)*depth)],
            parameters=dict(widths=list(widths),depth=depth,rounds=rounds,order=order,
                scope='fixed closed scalar recursion and width reuse; not an elapsed sample')))
    return cases


def native_function_cases(preset):
    """Bound all 26 persistent caches and exercise whole-call overflow fallback."""
    if preset not in ('full', 'holdout'):
        raise ValueError('unknown native function diagnostic preset')
    rounds = 64 if preset == 'full' else 53
    letters = 'abcdefghijklmnopqrstuvwxyz'
    source = ''.join('define ' + name + '(x) {\nreturn(x+1)\n}\n' for name in letters)
    source += 's=0\nfor(i=0;i<' + str(rounds) + ';i++) {\n'
    source += ''.join('s+=' + name + '(i)\n' for name in letters) + '}\ns\n'
    value = 999999999 if preset == 'full' else 999999997
    return [dict(name='resource_native_function_caches_' + preset, group='native_function_cache',
        program=source, expected=[str(26 * rounds * (rounds + 1) // 2)],
        parameters=dict(functions=26, rounds=rounds, native_function_outcome='complete',
            native_min_completions=26 * rounds,
            scope='all letter-indexed code caches alive together; selected counters exclude executable pages; cold RSS is descriptive')),
        dict(name='resource_native_function_overflow_' + preset, group='native_function_fallback',
            program='define f(x) {\nreturn(x*x*x+1)\n}\nf(' + str(value) + ')\n',
            expected=[str(value**3 + 1)], parameters=dict(input=value,
                native_function_outcome='decline', scope='integer-domain overflow must preserve the whole incoming argument for generic replay'))]


def word_function_cases(preset):
    """Fixed cache, conversion, promotion and rejection controls, never timing."""
    if preset not in ('full', 'holdout'):
        raise ValueError('unknown word function diagnostic preset')
    rounds = 64 if preset == 'full' else 53
    letters = 'abcdefghijklmnopqrstuvwxyz'
    source = ''.join('define ' + n + '(x) {\nreturn(x+1)\n}\n' for n in letters)
    source += 's=0\nfor(i=0;i<' + str(rounds) + ';i++) {\n'
    source += ''.join('s+=' + n + '(i)\n' for n in letters) + '}\ns\n'
    common = dict(scope='fixed correctness and selected-storage diagnostics; not elapsed samples')
    warm = 'for(i=0;i<8;i++) q=f(0)\n'
    return [
        dict(name='resource_word_caches_' + preset, group='word_cache', program=source,
             expected=[str(26 * rounds * (rounds + 1) // 2)], parameters=dict(common,
                 functions=26, rounds=rounds, word_outcome='complete',
                 word_min_completions=26 * (rounds - 7))),
        dict(name='resource_word_signed_conversion_' + preset, group='word_conversion',
             program='define f(x) {\nreturn(x+1)\n}\n' + warm +
                     'f(-9223372036854775808)\nf(9223372036854775807)\nf(9223372036854775808)\nf(.5)\n',
             expected=['-9223372036854775807', '9223372036854775808', '9223372036854775809', '1.5'],
             parameters=dict(common, word_outcome='complete', word_min_completions=3,
                             word_min_promotions=1)),
        dict(name='resource_word_promotion_' + preset, group='word_promotion',
             program='define f(x) {\nif(x==0) return(1)\nreturn(x*f(x-1))\n}\n' + warm + 'f(50)\n',
             expected=[str(math.factorial(50))], parameters=dict(common,
                 word_outcome='complete', word_min_completions=2, word_min_promotions=1)),
        dict(name='resource_word_demotion_' + preset, group='word_demotion',
             program='define f(x) {\nreturn(x*x-x*x)\n}\n' + warm + 'f(9223372036854775807)\n',
             expected=['0'], parameters=dict(common, word_outcome='complete',
                 word_min_completions=2, word_min_promotions=2, word_min_demotions=1)),
        dict(name='resource_word_rejection_' + preset, group='word_rejection',
             program='define f(x) {\ny+=1\nreturn(x)\n}\ny=0\n' + warm + 'f(.5)\ny\n',
             expected=['.5', '9'], parameters=dict(common, word_outcome='reject')),
        dict(name='resource_word_cold_' + preset, group='word_cold',
             program='define f(x) {\nreturn(x+1)\n}\nf(1)\n', expected=['2'],
             parameters=dict(common, word_outcome='cold'))]


def representatives(bench, preset, suite):
    """Fixed resource cases; no timing results influence their selection."""
    if suite == 'arithmetic':
        groups = {'startup', 'loop', 'calls', 'recursion', 'array', 'array_sparse',
                  'streaming', 'file_streaming', 'temporary_chain', 'storage_inline_prefix',
                  'storage_heap_snapshots', 'storage_promotion_churn'}
        # Match the elapsed controller's operand seed, independently of its
        # randomized measurement schedule. Seeded holdouts must keep their bytes.
        seed = 197201 if preset == 'holdout' else 197200
        cases = [case for case in bench.corpus(preset, seed=seed) if case['group'] in groups]
        if {case['group'] for case in cases} != groups:
            raise ValueError('resource corpus is missing a required arithmetic group')
        return cases + frame_cases(preset) + private_frame_cases(preset) + native_function_cases(preset) + word_function_cases(preset)
    if suite != 'math':
        raise ValueError('unknown resource suite')
    chosen, boundary = {}, []
    for case in bench.corpus(preset, suite='math'):
        parameters = case['parameters']
        if parameters['mode'] != 'one_shot':
            continue
        old = chosen.get(case['group'])
        if old is None or abs(parameters['scale'] - 100) < abs(old['parameters']['scale'] - 100):
            chosen[case['group']] = case
        if parameters['function'] in ('a', 'j') and parameters['scale'] == 20:
            boundary.append(case)
    selected = {case['name']: case for case in list(chosen.values()) + boundary}
    return list(selected.values())


def check_binary(bench, tool):
    argv, metadata = tool.get('argv'), tool.get('metadata')
    if (not isinstance(argv, list) or not argv or
            any(not isinstance(arg, str) for arg in argv) or not isinstance(metadata, dict)):
        raise ValueError('resource role requires executable argv and metadata')
    declared = metadata.get('binary_sha256')
    if not isinstance(declared, str) or not re.fullmatch('[0-9a-f]{64}', declared):
        raise ValueError('resource role requires a declared binary SHA-256')
    if bench.digest_file(argv[0]) != declared:
        raise ValueError('resource binary changed or disagrees with its declared identity')


def mode_identity(configuration, key):
    policy = configuration.get(key)
    if policy is None:
        return None
    if not isinstance(policy, dict):
        raise ValueError('missing resource role mode policy: ' + key)
    enabled = policy.get('enabled')
    if policy.get('status') == 'legacy-auto':
        if enabled is not None or policy.get('mode') != 'AUTO':
            raise ValueError('invalid legacy resource role mode')
    elif policy.get('status') != 'recorded' or (enabled is not True and enabled is not False):
        raise ValueError('invalid recorded resource role mode')
    elif policy.get('mode') not in ('AUTO', 'ON', 'OFF') or (
            policy['mode'] != 'AUTO' and enabled != (policy['mode'] == 'ON')):
        raise ValueError('contradictory recorded resource role mode')
    identity = {key: policy.get(key) for key in ('status', 'mode', 'enabled')}
    if key in ('native_scalar_policy', 'native_function_policy'):
        backends = (('native-expression-x64.cc', 'native-expression-arm64.cc')
                    if key == 'native_function_policy' else ('native-scalar.cc', 'native-scalar-x64.cc'))
        if policy.get('status') == 'recorded':
            configured, shared = policy.get('configured'), policy.get('shared_plan')
            if (type(configured) is not bool or type(shared) is not bool or
                    (enabled and (not configured or policy.get('backend') not in
                                  backends)) or
                    (not enabled and policy.get('backend') is not None)):
                raise ValueError('invalid ' + key + ' resource selection')
        identity.update({key: policy.get(key) for key in ('configured', 'backend', 'shared_plan')})
        if key == 'native_function_policy' and enabled:
            inputs = policy.get('source_inputs')
            if (shared is not True or not isinstance(inputs, list) or len(inputs) != 2 or
                    any(not isinstance(item, dict) or not isinstance(item.get('path'), str) or
                        not isinstance(item.get('sha256'), str) or
                        not re.fullmatch('[0-9a-f]{64}', item['sha256']) for item in inputs)):
                raise ValueError('invalid native_function_policy shared source inputs')
            identity['source_inputs'] = sorted((item['path'], item['sha256']) for item in inputs)
    if 'boundary' in policy:
        # Paths and hashes differ between ordinary and instrumented builds;
        # the compiled boundary policy must still describe the same mode.
        identity['boundary'] = policy['boundary']
    if key == 'word_function_policy' and policy.get('status') == 'recorded':
        units = ['word-function.cc', 'word-lowering.cc', 'word-runtime.cc']
        if (policy.get('translation_units') != (units if enabled else []) or
                (enabled and policy.get('configured') is not True) or
                (not enabled and policy.get('configured') is not None and policy.get('configured') is not False)):
            raise ValueError('invalid word-function resource selection')
        inputs = policy.get('source_inputs')
        if (not isinstance(inputs, list) or (enabled and len(inputs) != 6) or
                any(not isinstance(item, dict) or not isinstance(item.get('path'), str) or
                    not isinstance(item.get('sha256'), str) or
                    not re.fullmatch('[0-9a-f]{64}', item['sha256']) for item in inputs)):
            raise ValueError('invalid word-function source inputs')
        expected = {'src/bc/' + n for n in units + [n.replace('.cc', '.hh') for n in units]} if enabled else set()
        if {item['path'] for item in inputs} != expected:
            raise ValueError('invalid word-function source input inventory')
        identity.update(configured=policy.get('configured'), translation_units=policy['translation_units'],
                        source_inputs=sorted((item['path'], item['sha256']) for item in inputs))
    if key == 'prefix_result_policy' and policy.get('status') == 'recorded':
        present, configured = policy.get('implementation_present'), policy.get('configured')
        if (type(present) is not bool or
                (present and (type(configured) is not bool or configured is not enabled)) or
                (not present and (configured is not None or enabled is not False))):
            raise ValueError('invalid prefix_result_policy resource selection')
        inputs = policy.get('source_inputs')
        expected = {'src/bc/vm.cc', 'src/bc/CMakeLists.txt'}
        if present:
            expected.add('src/bc/prefix-result-buffer.hh')
        if (not isinstance(inputs, list) or len(inputs) != len(expected) or
                any(not isinstance(item, dict) or not isinstance(item.get('path'), str) or
                    not isinstance(item.get('sha256'), str) or
                    not re.fullmatch('[0-9a-f]{64}', item['sha256']) for item in inputs) or
                {item['path'] for item in inputs} != expected):
            raise ValueError('invalid prefix_result_policy source inputs')
        identity.update(implementation_present=present, configured=configured,
                        source_inputs=sorted((item['path'], item['sha256']) for item in inputs))
    if key == 'compact_digit_policy':
        inputs = policy.get('source_inputs')
        expected = {'src/bc/CMakeLists.txt', 'src/bc/number.hh'}
        if policy.get('status') == 'recorded':
            expected |= {'src/bc/limb-compact-digits.hh', 'src/bc/selected-digit-storage.hh', 'src/bc/digit-storage.hh'}
        if (not isinstance(inputs, list) or len(inputs) != len(expected) or
                any(not isinstance(item, dict) or not isinstance(item.get('path'), str) or
                    not re.fullmatch('[0-9a-f]{64}', str(item.get('sha256', ''))) for item in inputs) or
                {item['path'] for item in inputs} != expected):
            raise ValueError('invalid compact resource source inputs')
        identity['source_inputs'] = sorted((item['path'], item['sha256']) for item in inputs)
    return identity


def validate_roles(bench, profiles, ordinary):
    """Match source and common build policies, not full execution paths.

    Profiling can intentionally select a fallback, such as scalar argument
    packing on Apple arm64. Retain both profiling_execution_policy records;
    these checks cannot turn instrumented counts into ordinary-path counts.
    """
    if set(profiles) != set(ROLES) or set(ordinary) != set(ROLES):
        raise ValueError('resource comparison needs exactly base and candidate roles')
    for role in ROLES:
        profile, release = profiles[role], ordinary[role]
        for tool in (profile, release):
            check_binary(bench, tool)
        pm, rm = profile['metadata'], release['metadata']
        if not all(metadata.get(key) for metadata in (pm, rm)
                   for key in ('compiler', 'flags', 'version')):
            raise ValueError('resource roles lack compiler/flags/version provenance')
        if profile['argv'][1:] != release['argv'][1:]:
            raise ValueError('profile and ordinary resource arguments differ')
        if pm.get('instrumented') is not True or rm.get('instrumented') is not False:
            raise ValueError('profile and ordinary resource instrumentation roles are invalid')
        source = pm.get('source_revision')
        if not isinstance(source, str) or not re.fullmatch('[0-9a-f]{40}', source) or source != rm.get('source_revision'):
            raise ValueError('profile and ordinary resource source revisions differ')
        pc, rc = pm.get('configuration'), rm.get('configuration')
        if not isinstance(pc, dict) or not isinstance(rc, dict):
            raise ValueError('missing resource build configuration')
        if pc.get('diagnostic_profile') is not True or rc.get('diagnostic_profile') is not False:
            raise ValueError('resource build did not declare its diagnostic role')
        for key in ('build_type', 'generator', 'extensions', 'array_storage', 'multiply_split_point',
                    'preparsed_library_image_sha256'):
            if pc.get(key) != rc.get(key):
                raise ValueError('profile and ordinary resource configurations differ: ' + key)
        for key in ('portable_scalar_policy', 'prepared_divisors_policy', 'native_scalar_policy', 'native_function_policy', 'compact_digit_policy'):
            if mode_identity(pc, key) != mode_identity(rc, key):
                raise ValueError('profile and ordinary resource modes differ: ' + key)
        if mode_identity(pc, 'word_function_policy') != mode_identity(rc, 'word_function_policy'):
            raise ValueError('profile and ordinary resource modes differ: word_function_policy')
        if mode_identity(pc, 'prefix_result_policy') != mode_identity(rc, 'prefix_result_policy'):
            raise ValueError('profile and ordinary resource modes differ: prefix_result_policy')
        for key in ('status', 'schema', 'mode', 'automatic_ipo'):
            if (pc.get('ipo_policy') or {}).get(key) != (rc.get('ipo_policy') or {}).get(key):
                raise ValueError('profile and ordinary resource IPO policies differ')
        for key in ('compiler', 'standard_library'):
            if pm.get(key) != rm.get(key):
                raise ValueError('profile and ordinary resource toolchains differ: ' + key)
        # Compilation databases necessarily differ in paths and counter defines.
        # The explicit CMake compiler/linker flags must still agree.
        pf, rf = pm.get('flags'), rm.get('flags')
        if isinstance(pf, dict) and isinstance(rf, dict):
            if {k: v for k, v in pf.items() if k.startswith('CMAKE_')} != {
                    k: v for k, v in rf.items() if k.startswith('CMAKE_')}:
                raise ValueError('profile and ordinary resource compiler flags differ')
        elif pf != rf:
            raise ValueError('profile and ordinary resource compiler flags differ')


def checked_counters(report):
    if (not isinstance(report, dict) or report.get('instrumented') is not True or
            isinstance(report.get('schema'), bool) or report.get('schema') != 1):
        raise ValueError('resource counters are not declared instrumented')
    allocations = report.get('allocations')
    if not isinstance(allocations, dict) or not allocations:
        raise ValueError('missing allocation resource categories')
    for counters in [report.get('total')] + list(allocations.values()):
        if not isinstance(counters, dict) or not all(key in counters for key in COUNTERS):
            raise ValueError('missing allocation resource counters')
        if any(isinstance(v, bool) or not isinstance(v, int) or v < 0 for v in counters.values()):
            raise ValueError('invalid allocation resource counters')
        if counters['live_bytes'] != 0:
            raise ValueError('unreleased tracked storage in resource report')
    events = report.get('events')
    if not isinstance(events, dict) or any(isinstance(v, bool) or not isinstance(v, int) or v < 0
                                         for v in events.values()):
        raise ValueError('invalid event resource counters')
    return report


def differences(base, candidate):
    result = {}
    for key in sorted(set(base) | set(candidate)):
        a, b = base.get(key), candidate.get(key)
        result[key] = dict(base=a, candidate=b, status='compared' if a is not None and b is not None else 'missing counter')
        if a is not None and b is not None:
            result[key].update(delta=b - a, candidate_over_base=b / a if a else None)
    return result


def allocation_comparison(base, candidate):
    categories = {}
    for key in sorted(set(base['allocations']) | set(candidate['allocations'])):
        categories[key] = differences(base['allocations'].get(key, {}), candidate['allocations'].get(key, {}))
    return dict(total=differences(base['total'], candidate['total']), allocations=categories,
                events=differences(base['events'], candidate['events']))


def rss_comparison(observations, repetitions):
    ratios = []
    for observation in observations:
        a, b = observation['roles']['base'], observation['roles']['candidate']
        if not all(role.get('status') == 'collected' and role.get('checked') is True and
                   isinstance(role.get('peak_rss_native'), int) and not isinstance(role['peak_rss_native'], bool) and
                   role['peak_rss_native'] > 0 for role in (a, b)):
            continue
        if not a.get('native_unit') or a['native_unit'] != b.get('native_unit'):
            continue
        ratios.append(b['peak_rss_native'] / a['peak_rss_native'])
    return dict(status='descriptive' if len(ratios) == repetitions else 'inconclusive',
                complete_pairs=len(ratios), required_pairs=repetitions, paired_ratios=ratios,
                median_paired_ratio=statistics.median(ratios) if ratios else None,
                scope='cold native RSS pairs only; no elapsed inference, confidence claim or merge decision')


def role_configuration(roles, library=False):
    if set(roles) != set(ROLES):
        raise ValueError('resource configuration requires base and candidate')
    entries = []
    for role in ROLES:
        entry = copy.deepcopy(roles[role])
        entry['name'] = role
        if library:
            entry['argv'].append('-l')
        entries.append(entry)
    return dict(implementations=entries)


def run_diagnostics(run, profiles, ordinary, presets, suite, evidence, timeout=60):
    """Invoke resource wrappers after all elapsed children have finished."""
    profile_path, ordinary_path = evidence / 'resource-profiles.json', evidence / 'resource-ordinary.json'
    write(profile_path, role_configuration(profiles, library=suite == 'math'))
    write(ordinary_path, role_configuration(ordinary, library=suite == 'math'))
    for preset in presets:
        run([sys.executable, str(Path(__file__).resolve()), '--profiles', str(profile_path),
             '--ordinary', str(ordinary_path), '--suite', suite, '--preset', preset,
             '--output', str(evidence / (preset + '-resource-comparison')),
             '--repetitions', '5', '--seed', '661197', '--child-timeout-seconds', str(timeout)])


def preserve_profiles(profiles, directory):
    """Retain actual instrumented images separately from ordinary timing roles."""
    if not profiles or not set(profiles) <= set(ROLES):
        raise ValueError('profile preservation needs known completed roles')
    bench = load('bench-bc')
    directory.mkdir(exist_ok=False)
    report = dict(schema=1, scope='instrumented resource images; never elapsed timing',
                  observations=[])
    try:
        for role, tool in profiles.items():
            if tool.get('metadata', {}).get('instrumented') is not True or tool.get('input_artifacts'):
                raise ValueError('resource profile must declare instrumentation without external inputs')
            check_binary(bench, tool)
            source = Path(tool['argv'][0])
            target = directory / role
            target.mkdir()
            image = target / ('binary' + source.suffix)
            raw = source.read_bytes()
            image.write_bytes(raw)
            expected = tool['metadata']['binary_sha256']
            if (hashlib.sha256(raw).hexdigest() != expected or
                    bench.digest_file(source) != expected or bench.digest_file(image) != expected):
                raise ValueError('resource profile image changed during preservation')
            report['observations'].append(dict(role=role, implementation=tool,
                binary=str(image.relative_to(directory)), binary_sha256=expected, file_bytes=len(raw)))
    finally:
        write(directory / 'manifest.json', report)


def verify_native_path(configuration, counters, case):
    """A compiled option does not prove completion in a resource input."""
    events = counters['events']
    for key, prefix, group in (('native_scalar_policy', 'native_scalar', 'loop'),
                                ('native_function_policy', 'native_function', 'calls')):
        policy = configuration.get(key) or {}
        if policy.get('status') != 'recorded':
            continue
        attempts = events.get(prefix + '_attempts', 0)
        completions = events.get(prefix + '_completions', 0)
        if not policy.get('enabled'):
            if attempts or completions:
                raise ValueError('native events contradict disabled resource compilation: ' + key)
            continue
        expected = case['parameters'].get('native_function_outcome') if prefix == 'native_function' else None
        if case['group'] == group or expected == 'complete':
            minimum = case['parameters'].get('native_min_completions', 1)
            if attempts < minimum or completions < minimum or completions > attempts:
                raise ValueError('resource case did not complete the compiled ' + prefix + ' path')
        elif expected == 'decline' and (attempts != 1 or completions != 0):
            raise ValueError('resource overflow case did not decline native function execution')
    policy = configuration.get('word_function_policy') or {}
    if policy.get('status') != 'recorded':
        return
    attempts = events.get('word_function_attempts', 0)
    completions = events.get('word_function_completions', 0)
    if completions > attempts:
        raise ValueError('word completions exceed attempts')
    if not policy.get('enabled'):
        if attempts or completions:
            raise ValueError('word events contradict disabled resource compilation')
        return
    expected = case['parameters'].get('word_outcome')
    if case['group'] in ('calls', 'recursion') or expected == 'complete':
        minimum = case['parameters'].get('word_min_completions', 1)
        if completions < minimum:
            raise ValueError('resource case did not complete the compiled word path')
        if events.get('word_function_promotions', 0) < case['parameters'].get('word_min_promotions', 0):
            raise ValueError('word resource case did not promote exact arithmetic')
        if events.get('word_function_demotions', 0) < case['parameters'].get('word_min_demotions', 0):
            raise ValueError('word resource case did not demote exact arithmetic')
    elif expected in ('reject', 'cold'):
        if attempts or completions:
            raise ValueError('word cold/rejected resource case unexpectedly executed')
        if expected == 'reject' and events.get('word_function_rejections', 0) < 1:
            raise ValueError('word resource case did not cache static rejection')


def collect(bench, adapter, profiles, ordinary, cases, directory, repetitions=5, seed=661197, timeout=60):
    validate_roles(bench, profiles, ordinary)
    if isinstance(repetitions, bool) or not isinstance(repetitions, int) or repetitions < 3:
        raise ValueError('resource diagnostics require at least three declared RSS pairs')
    directory.mkdir(parents=True, exist_ok=False)
    report = dict(schema=1, status='incomplete', merge_eligible=False, repetitions=repetitions, seed=seed,
                  driver_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  platform=dict(system=platform.system(), machine=platform.machine(), python=platform.python_version()),
                  scope='selected-container software counts and cold native RSS; not process malloc, steady-state retention or elapsed performance; release/profile execution-path identity not established',
                  profiles=profiles, ordinary=ordinary, results=[])
    previous = os.environ.get('GD_BC_PROFILE_OUTPUT')
    try:
        for case in cases:
            name = case['name']
            if not re.fullmatch('[A-Za-z0-9_.-]+', name) or name in ('.', '..'):
                raise ValueError('invalid resource case name')
            target = directory / name
            target.mkdir()
            program, oracle = case['program'].encode('utf-8'), bench.oracle_payload(case)
            (target / 'input.bc').write_bytes(program)
            (target / 'expected.txt').write_bytes(oracle)
            row = dict(name=name, parameters=case['parameters'], input_mode=case.get('input_mode', 'stdin'),
                       program_sha256=hashlib.sha256(program).hexdigest(),
                       oracle_sha256=hashlib.sha256(oracle).hexdigest(), rss_observations=[])
            report['results'].append(row)
            counters = {}
            for role in ROLES:
                check_binary(bench, profiles[role])
                path = target / (role + '-counters.json')
                os.environ['GD_BC_PROFILE_OUTPUT'] = str(path)
                observed = bench.run_checked(profiles[role], case, timeout, program_path=target / 'input.bc')
                # Keep checked output identities, never instrumented durations.
                observed = {key: value for key, value in observed.items()
                            if key in ('checked', 'output_sha256', 'output_bytes')}
                if observed.get('checked') is not True:
                    raise ValueError('unchecked resource counter output')
                write(target / (role + '-profile.json'), observed)
                counters[role] = checked_counters(json.loads(path.read_text(encoding='utf-8')))
                verify_native_path(profiles[role]['metadata']['configuration'], counters[role], case)
            row['allocation_comparison'] = allocation_comparison(counters['base'], counters['candidate'])
            randomizer = random.Random(str(seed) + ':' + name)
            first = randomizer.randrange(2)
            for round_number in range(repetitions):
                order = list(ROLES)
                if (first + round_number) % 2:
                    order.reverse()
                observed = dict(round=round_number, order=order, roles={})
                row['rss_observations'].append(observed)
                for role in order:
                    check_binary(bench, ordinary[role])
                    destination = target / ('rss-' + str(round_number) + '-' + role)
                    destination.mkdir()
                    observed['roles'][role] = adapter.rss_profile(bench, ordinary[role], case, destination, timeout)
            row['rss_comparison'] = rss_comparison(row['rss_observations'], repetitions)
        report['status'] = 'collected'
        return report
    finally:
        if previous is None:
            os.environ.pop('GD_BC_PROFILE_OUTPUT', None)
        else:
            os.environ['GD_BC_PROFILE_OUTPUT'] = previous
        write(directory / 'manifest.json', report)


def main():
    bench = load('bench-bc')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--profiles', type=Path, required=True)
    parser.add_argument('--ordinary', type=Path, required=True)
    parser.add_argument('--suite', choices=('arithmetic', 'math'), required=True)
    parser.add_argument('--preset', choices=('full', 'holdout'), required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--repetitions', type=bench.positive_integer, default=5)
    parser.add_argument('--seed', type=int, default=661197)
    parser.add_argument('--child-timeout-seconds', type=bench.positive_timeout, default=60)
    args = parser.parse_args()
    try:
        profiles = json.loads(args.profiles.read_text(encoding='utf-8'))['implementations']
        ordinary = bench.implementations(args.ordinary, require_provenance=True)
        if len(profiles) != 2 or len(ordinary) != 2:
            raise ValueError('resource configuration requires two roles')
        profiles = {entry['name']: entry for entry in profiles}
        ordinary = {entry['name']: entry for entry in ordinary}
        collect(bench, load('profile-bc'), profiles, ordinary,
                representatives(bench, args.preset, args.suite), args.output,
                args.repetitions, args.seed, args.child_timeout_seconds)
        return 0
    except (ValueError, OSError, KeyError, TypeError, bench.BenchmarkError) as error:
        print('resource comparison failed: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
