#!/usr/bin/env python3
"""Build an immutable GD revision with fixed dispatch and actual provenance.

Release is the default. Explicit diagnostic counter builds are always marked
instrumented, so the timing harness rejects their generated configurations.

Use the sibling comparator builder for shared bounded command/log handling.
Python 3.6+; SPDX-License-Identifier: Apache-2.0
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import sys


def helpers():
    path = Path(__file__).with_name('build-bc-comparators.py')
    spec = importlib.util.spec_from_file_location('bc_build_helpers', str(path))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def compiler_identity(helper, builder, requested):
    metadata = helper.compiler_metadata(builder, requested)
    # clang++ and compiler wrappers can select behavior through argv[0]. Keep
    # the invocation alias while recording the target file identity separately.
    metadata['realpath'] = metadata['argv0']
    metadata['argv0'] = str(Path(shutil.which(requested)).absolute())
    return metadata


def standard_library_info(helper, builder, compiler, root):
    macros = ('_LIBCPP_VERSION', '__GLIBCXX__', '_GLIBCXX_RELEASE',
              '_MSVC_STL_VERSION', '_MSVC_STL_UPDATE', '__GLIBC__', '__GLIBC_MINOR__')
    probe = root / 'standard-library-probe.cc'
    probe.write_text('#include <version>\n' + ''.join(
        '#ifdef {0}\nGD_BC_LIBRARY_{0} {0}\n#endif\n'.format(name) for name in macros),
        encoding='utf-8')
    msvc = Path(compiler['argv0']).stem.lower() in ('cl', 'clang-cl')
    command = ([compiler['argv0'], '/nologo', '/std:c++20', '/EP', str(probe)] if msvc else
               [compiler['argv0'], '-std=c++20', '-E', '-P', str(probe)])
    try:
        output = builder.run(command)
    except helper.BuildError:
        return {'status': 'unavailable', 'reason': 'header-version probe failed; see command log'}
    observed = dict(re.findall(r'^GD_BC_LIBRARY_([A-Z0-9_]+)\s+([0-9]+[uUlL]*)\s*$',
                               output, re.MULTILINE))
    return {'status': 'recorded' if observed else 'unavailable', 'header_macros': observed,
            'scope': 'standard-library header identity; provision runtime and SDK separately'}


def cache_values(path):
    result = {}
    for line in path.read_text(encoding='utf-8', errors='replace').splitlines():
        if not line or line.startswith(('#', '//')) or '=' not in line:
            continue
        key, value = line.split('=', 1)
        result[key.split(':', 1)[0]] = value
    return result


def build_environment():
    return {key: value for key, value in os.environ.items()
            if not key.startswith(('BC_', 'DC_')) and
            key not in ('CFLAGS', 'CXXFLAGS', 'CPPFLAGS', 'LDFLAGS', 'CL', '_CL_')}


def profiling_execution_policy(helper, builder, root, source, build):
    """Record one compile-time eligibility gate, not dynamic path identity.

    Counter instrumentation can intentionally select a fallback. Probe the
    known VM guard with its own compile definitions/target; do not infer an
    inactive feature from missing source or unsupported compiler evidence.
    """
    forwarding = dict(status='unavailable', enabled=None)
    policy = dict(schema=1, path_identity='not established',
                  scalar_argument_forwarding=forwarding)
    vm = source / 'src/bc/vm.cc'
    if not vm.is_file():
        forwarding['reason'] = 'VM source unavailable'
        return policy
    source_bytes = vm.read_bytes()
    forwarding['source_sha256'] = helper.digest(vm)
    guard = ('#if defined(__APPLE__) && defined(__aarch64__) && !GD_BC_PROFILE\n'
             '#  define GD_BC_FORWARD_SCALAR_ARGUMENT 1\n#else\n'
             '#  define GD_BC_FORWARD_SCALAR_ARGUMENT 0\n#endif')
    text = source_bytes.decode('utf-8', errors='replace').replace('\r\n', '\n')
    definitions = re.findall(r'^\s*#\s*define\s+GD_BC_FORWARD_SCALAR_ARGUMENT\b', text, re.M)
    if text.count(guard) != 1 or len(definitions) != 2 or re.search(
            r'^\s*#\s*undef\s+GD_BC_FORWARD_SCALAR_ARGUMENT\b', text, re.M):
        forwarding.update(status='unknown', reason='unrecognized or absent VM eligibility guard')
        return policy
    commands = build / 'compile_commands.json'
    if builder is None or os.name != 'posix' or not commands.is_file():
        forwarding['reason'] = 'GNU-style VM compilation context/probe runner unavailable'
        return policy
    try:
        entries = json.loads(commands.read_text(encoding='utf-8'))
        if not isinstance(entries, list) or any(not isinstance(entry, dict) for entry in entries):
            raise ValueError('unsupported VM compilation database')
        matches = [entry for entry in entries if Path(entry.get('file', '')).resolve() == vm.resolve()]
        if len(matches) != 1:
            raise ValueError('VM compilation command is not unique')
        entry = matches[0]
        argv = entry.get('arguments') or shlex.split(entry['command'])
        if not isinstance(argv, list) or not argv or any(
                not isinstance(arg, str) or arg.startswith('@') for arg in argv):
            raise ValueError('unsupported VM compiler arguments')
        probe = root / 'profile-forwarding-probe.cc'
        if any(char in vm.as_posix() for char in ('"', '\n', '\r')):
            raise ValueError('unsupported VM include path')
        probe.write_text('#include "' + vm.as_posix() + '"\n'
                         'GD_BC_PROFILE_FORWARDING GD_BC_FORWARD_SCALAR_ARGUMENT\n', encoding='utf-8')
        # Keep target, language, include and definition flags. Suppress object,
        # dependency and LTO outputs, which have no role in this known guard.
        filtered, skip, replaced = [], False, 0
        for arg in argv:
            if skip:
                skip = False
                continue
            if arg in ('-o', '-MF', '-MT', '-MQ', '-MJ', '-serialize-diagnostics'):
                skip = True
            elif arg in ('-c', '-MD', '-MMD', '-MP') or arg.startswith(
                    ('-flto', '-fno-lto', '-MF', '-MT', '-MQ', '-MJ', '--serialize-diagnostics=')):
                continue
            elif arg == entry['file']:
                filtered.append(str(probe))
                replaced += 1
            elif arg.startswith('-o') and arg != argv[0]:
                raise ValueError('unsupported attached output/driver option')
            else:
                filtered.append(arg)
        if skip or replaced != 1:
            raise ValueError('unsupported VM source/output arguments')
        output = builder.run(filtered + ['-E', '-P'], cwd=entry.get('directory', build),
                             environment=build_environment())
        state = re.findall(r'^GD_BC_PROFILE_FORWARDING ([01])$', output, re.M)
        if len(state) != 1 or output.splitlines()[-1] != 'GD_BC_PROFILE_FORWARDING ' + state[0]:
            raise ValueError('VM probe did not provide one final boolean eligibility state')
        if vm.read_bytes() != source_bytes:
            raise ValueError('VM source changed during the probe')
        log = builder.commands[-1]['log']
        forwarding.update(status='recorded', enabled=state[0] == '1',
                          verification=dict(kind='vm-preprocessor', log=log,
                                            log_sha256=helper.digest(root / log),
                                            probe=probe.name,
                                            probe_sha256=helper.digest(probe),
                                            compile_commands_sha256=helper.digest(commands)),
                          scope='Compile-time eligibility only; not evidence that a call forwards or that all release/profile paths match')
    except helper.BuildError:
        forwarding.update(status='unavailable',
                          reason='VM preprocessor probe failed; see build command logs')
    except (OSError, ValueError, KeyError, TypeError) as error:
        forwarding.update(status='unavailable', reason=str(error))
    return policy


def build_metadata(helper, root, source, build, revision, split, compiler_c, compiler_cxx,
                   ipo_mode='AUTO', prepared_mode='AUTO', diagnostic_profile=False, builder=None):
    cache = cache_values(build / 'CMakeCache.txt')
    for flag in ('ENABLE_BC_PROFILE', 'ENABLE_BC_COVERAGE'):
        allowed = ('ON', 'TRUE', 'YES', '1') if diagnostic_profile and flag == 'ENABLE_BC_PROFILE' else ('OFF', 'FALSE', 'NO', '0')
        if cache.get(flag, '').upper() not in allowed:
            raise helper.BuildError('configuration disagrees with requested diagnostic mode: ' + flag)
    header = (build / 'src/bc/find-multiply-split-point.hh').read_text(encoding='utf-8')
    match = re.search(r'^#define BC_MULTIPLY_SPLIT_POINT ([0-9]+)$', header, re.MULTILINE)
    if not match or int(match.group(1)) != split or 'explicit BC_MULTIPLY_SPLIT_POINT' not in header:
        raise helper.BuildError('revision did not produce the requested fixed-threshold header')
    flag_names = ('CMAKE_C_FLAGS', 'CMAKE_C_FLAGS_RELEASE', 'CMAKE_CXX_FLAGS',
                  'CMAKE_CXX_FLAGS_RELEASE', 'CMAKE_EXE_LINKER_FLAGS',
                  'CMAKE_EXE_LINKER_FLAGS_RELEASE')
    flags = {name: cache.get(name, '') for name in flag_names}
    commands = build / 'compile_commands.json'
    if commands.is_file():
        shutil.copyfile(str(commands), str(root / 'compile_commands.json'))
        flags['compile_commands_sha256'] = helper.digest(commands)
        checked_flags = json.dumps(flags) + commands.read_text(encoding='utf-8')
    else:
        # Native VS generators do not emit compile_commands; verbose build logs
        # remain required evidence, and this absence stays explicit.
        flags['compile_commands'] = 'unavailable for this generator; see verbose build log'
        checked_flags = json.dumps(flags)
    if re.search(r'-fprofile(?:-instr)?-generate|-fprofile-arcs|-ftest-coverage|-fcoverage|--coverage|-fsanitize|/fsanitize|-finstrument-functions',
                 checked_flags, re.IGNORECASE):
        raise helper.BuildError('timing build contains instrumentation flags')
    compiled_profile = bool(re.search(r'GD_BC_PROFILE=1', checked_flags, re.IGNORECASE))
    if compiled_profile and not diagnostic_profile:
        raise helper.BuildError('timing build contains diagnostic counter instrumentation')
    if commands.is_file() and diagnostic_profile and not compiled_profile:
        raise helper.BuildError('diagnostic counters are absent from compiled commands')
    shutil.copyfile(str(build / 'src/bc/find-multiply-split-point.hh'),
                    str(root / 'multiply-threshold.hh'))
    ipo = verified_ipo_policy(helper, build, ipo_mode)
    if ipo['status'] == 'recorded':
        shutil.copyfile(str(build / 'gd-ipo-policy.json'), str(root / 'gd-ipo-policy.json'))
        ipo['report_sha256'] = helper.digest(root / 'gd-ipo-policy.json')
    prepared = verified_prepared_divisors(helper, build, prepared_mode)
    native = verified_native_scalars(helper, build)
    native_functions = verified_native_functions(helper, build, source)
    if native_functions.get('verification', {}).get('kind') == 'msbuild-projects':
        for relative in native_functions['verification']['files']:
            target = root / 'native-function-policy' / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(str(build / relative), str(target))
    if native_functions['status'] == 'recorded':
        directory = root / 'native-function-policy'
        directory.mkdir(exist_ok=True)
        shutil.copyfile(str(build / 'CMakeCache.txt'), str(directory / 'CMakeCache.txt'))
        for item in native_functions.get('source_inputs', []):
            target = directory / item['retained_path']
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(str(source / item['path']), str(target))
    if native.get('verification', {}).get('kind') == 'msbuild-projects':
        for relative in native['verification']['files']:
            target = root / 'native-policy' / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(str(build / relative), str(target))
    if native['status'] == 'recorded':
        (root / 'native-policy').mkdir(exist_ok=True)
        shutil.copyfile(str(build / 'CMakeCache.txt'), str(root / 'native-policy/CMakeCache.txt'))
    if prepared.get('verification', {}).get('kind') == 'msbuild-projects':
        for relative in prepared['verification']['files']:
            target = root / 'prepared-policy' / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(str(build / relative), str(target))
    return {'source_revision': revision,
            'compiler': {'C': compiler_c, 'CXX': compiler_cxx}, 'flags': flags,
            'version': 'gd-posix-apps source ' + revision, 'instrumented': diagnostic_profile,
            'configuration': {'build_type': cache.get('CMAKE_BUILD_TYPE'),
                              'generator': cache.get('CMAKE_GENERATOR'),
                              'extensions': cache.get('ENABLE_NON_POSIX_EXTENSIONS'),
                              'ipo_policy': ipo,
                              'prepared_divisors_policy': prepared,
                              'native_scalar_policy': native,
                              'native_function_policy': native_functions,
                              'multiply_split_point': split,
                              'diagnostic_profile': diagnostic_profile,
                              'profiling_execution_policy': profiling_execution_policy(
                                  helper, builder, root, source, build),
                              'threshold_header_sha256': helper.digest(root / 'multiply-threshold.hh')},
            'limitations': ['Provision compiler, C/C++ runtime and SDK separately; this is not an OS image',
                            'Compiler defaults and actual commands are recorded, not claimed identical across hosts']}


def verified_ipo_policy(helper, build, requested):
    """Require the source policy report, not an ignored command-line cache key."""
    path = build / 'gd-ipo-policy.json'
    if not path.is_file():
        if requested != 'AUTO':
            raise helper.BuildError('source revision cannot verify the requested IPO policy')
        return dict(status='legacy-auto', mode='AUTO', automatic_ipo=None,
                    scope='source predates explicit policy; inspect recorded compiler commands')
    try:
        value = json.loads(path.read_text(encoding='utf-8'))
    except (ValueError, OSError) as error:
        raise helper.BuildError('invalid IPO policy report: ' + str(error))
    if (not isinstance(value, dict) or type(value.get('schema')) is not int or
            value.get('schema') != 1 or
            value.get('mode') != requested or type(value.get('automatic_ipo')) is not bool or
            cache_values(build / 'CMakeCache.txt').get('IPO_MODE') != requested):
        raise helper.BuildError('source did not record the requested IPO policy')
    if ((requested == 'OFF' and value['automatic_ipo']) or
            (requested == 'REQUIRED' and not value['automatic_ipo'])):
        raise helper.BuildError('source did not apply the requested IPO policy')
    return dict(status='recorded', schema=value['schema'], mode=value['mode'],
                automatic_ipo=value['automatic_ipo'])


def verified_prepared_divisors(helper, build, requested):
    """Verify the configured mode in compiled VM/entrypoint definitions."""
    if requested not in ('AUTO', 'ON', 'OFF'):
        raise helper.BuildError('invalid prepared-divisor mode')
    observed = cache_values(build / 'CMakeCache.txt').get('ENABLE_BC_PREPARED_DIVISORS')
    if observed is None:
        if requested != 'AUTO':
            raise helper.BuildError('source revision cannot verify the requested divisor mode')
        return dict(status='legacy-auto', mode='AUTO', enabled=None,
                    scope='source predates explicit VM reuse policy; inspect its immutable source')
    normalized = observed.upper()
    if normalized not in ('ON', 'TRUE', 'YES', '1', 'OFF', 'FALSE', 'NO', '0'):
        raise helper.BuildError('invalid configured prepared-divisor mode')
    enabled = normalized in ('ON', 'TRUE', 'YES', '1')
    if requested != 'AUTO' and enabled != (requested == 'ON'):
        raise helper.BuildError('source did not configure the requested divisor mode')
    expected = '1' if enabled else '0'
    define = re.compile(r'''(?:^|[\s"'])[-/]D\s*["']?GD_BC_PREPARED_DIVISORS\b(?:=([^\s"';]+))?''')
    undefine = re.compile(r'''(?:^|[\s"'])[-/]U\s*["']?GD_BC_PREPARED_DIVISORS\b''')
    commands = build / 'compile_commands.json'
    if commands.is_file():
        try:
            entries = json.loads(commands.read_text(encoding='utf-8'))
        except (ValueError, OSError) as error:
            raise helper.BuildError('invalid compiled divisor-mode evidence: ' + str(error))
        if not isinstance(entries, list) or any(not isinstance(row, dict) for row in entries):
            raise helper.BuildError('invalid compiled divisor-mode command inventory')
        selected = []
        for row in entries:
            filename = '/' + str(row.get('file', '')).replace('\\', '/').lstrip('/')
            if not filename.endswith(('/src/bc/vm.cc', '/src/bc/bc.cc')):
                continue
            if isinstance(row.get('arguments'), list):
                if not all(isinstance(arg, str) for arg in row['arguments']):
                    raise helper.BuildError('invalid compiler argument inventory')
                command = ' '.join(row['arguments'])
            else:
                command = row.get('command', '')
            if not isinstance(command, str):
                raise helper.BuildError('invalid compiler command')
            definitions = define.findall(command)
            if not definitions or any(value != expected for value in definitions) or undefine.search(command):
                raise helper.BuildError('compiled VM/entrypoint did not select the configured divisor mode')
            selected.append(filename.rsplit('/', 1)[-1])
        if set(selected) != {'vm.cc', 'bc.cc'}:
            raise helper.BuildError('missing compiled VM/entrypoint divisor-mode definitions')
        evidence = dict(kind='compile-commands', sha256=helper.digest(commands),
                        translation_units=len(selected))
    else:
        # Native VS generators have no compilation database. Inspect and retain
        # both Release target projects rather than accepting an ignored -D key.
        import xml.etree.ElementTree as ET
        generator = cache_values(build / 'CMakeCache.txt').get('CMAKE_GENERATOR', '')
        if not generator.startswith('Visual Studio'):
            raise helper.BuildError('compiled divisor mode needs compile commands or native VS projects')
        files = {}
        for name in ('libbc', 'bc'):
            relative = 'src/bc/' + name + '.vcxproj'
            path = build / relative
            try:
                root = ET.parse(str(path)).getroot()
            except (ET.ParseError, OSError) as error:
                raise helper.BuildError('cannot verify compiled divisor mode: ' + str(error))
            release = False
            for node in root.iter():
                tag = node.tag.rsplit('}', 1)[-1]
                if tag == 'UndefinePreprocessorDefinitions' and 'GD_BC_PREPARED_DIVISORS' in (node.text or ''):
                    raise helper.BuildError('project undefines the divisor mode')
                if tag == 'AdditionalOptions':
                    values = define.findall(node.text or '')
                    if any(value != expected for value in values) or undefine.search(node.text or ''):
                        raise helper.BuildError('project options contradict the divisor mode')
                if tag == 'PreprocessorDefinitions':
                    values = re.findall(r'(?:^|;)GD_BC_PREPARED_DIVISORS\b(?:=([^;]+))?', node.text or '')
                    if any(value != expected for value in values):
                        raise helper.BuildError('project contradicts the configured divisor mode')
                if tag == 'ItemDefinitionGroup':
                    condition = node.get('Condition', '')
                    if condition and not re.search(r'Release\|', condition, re.IGNORECASE):
                        continue
                    for definition in node.iter():
                        if definition.tag.rsplit('}', 1)[-1] == 'PreprocessorDefinitions':
                            values = re.findall(r'(?:^|;)GD_BC_PREPARED_DIVISORS\b(?:=([^;]+))?', definition.text or '')
                            release = release or values == [expected]
            if not release:
                raise helper.BuildError('Release project did not select the divisor mode')
            files[relative] = helper.digest(path)
        evidence = dict(kind='msbuild-projects', files=files, retained_root='prepared-policy')
    return dict(status='recorded', mode=requested, enabled=enabled, verification=evidence)


def require_prepared_candidate(helper, entry):
    """An experiment must not silently time a default/ignored activation flag."""
    metadata = entry.get('metadata') if isinstance(entry, dict) else None
    configuration = metadata.get('configuration') if isinstance(metadata, dict) else None
    policy = configuration.get('prepared_divisors_policy') if isinstance(configuration, dict) else None
    if (not isinstance(policy, dict) or policy.get('status') != 'recorded' or
            policy.get('mode') != 'ON' or policy.get('enabled') is not True):
        raise helper.BuildError('candidate lacks verified explicit prepared-divisor activation')


def verified_native_backend(helper, build, key, macro, backend_names, shared_compilation):
    """Verify a native presence definition against the actual build inventory."""
    cache_path = build / 'CMakeCache.txt'
    cache = cache_values(cache_path)
    configured = cache.get(key)
    if configured is None:
        return dict(status='legacy-auto', mode='AUTO', configured=None, enabled=None, backend=None,
                    scope='source predates this native option; activation unknown')
    if (not re.search(r'^' + key + r':BOOL=', cache_path.read_text(), re.MULTILINE) or
            configured.upper() not in ('ON', 'TRUE', 'YES', '1', 'OFF', 'FALSE', 'NO', '0')):
        raise helper.BuildError('source did not declare a boolean native option')
    configured = configured.upper() in ('ON', 'TRUE', 'YES', '1')
    define = re.compile(r'''(?:^|[\s"'])[-/]D\s*["']?''' + re.escape(macro) + r'''\b(?:=([^\s"';]+))?''')
    undefine = re.compile(r'''(?:^|[\s"'])[-/]U\s*["']?''' + re.escape(macro) + r'''\b''')
    database = build / 'compile_commands.json'
    if database.is_file():
        try:
            rows = json.loads(database.read_text())
        except (ValueError, OSError) as error:
            raise helper.BuildError('invalid native compilation database: ' + str(error))
        if not isinstance(rows, list) or any(not isinstance(r, dict) for r in rows):
            raise helper.BuildError('invalid native compilation inventory')
        vm = [r for r in rows if str(r.get('file', '')).replace('\\', '/').endswith('/src/bc/vm.cc')]
        if len(vm) != 1:
            raise helper.BuildError('missing or ambiguous native VM compilation')
        arguments = vm[0].get('arguments')
        if arguments is not None:
            if not isinstance(arguments, list) or not all(isinstance(x, str) for x in arguments):
                raise helper.BuildError('invalid native VM compiler arguments')
            command = ' '.join(arguments)
        else:
            command = vm[0].get('command')
        if not isinstance(command, str) or undefine.search(command):
            raise helper.BuildError('invalid or overridden native VM definition')
        values = define.findall(command)
        if values not in ([], ['1']):
            raise helper.BuildError('native presence-style definition must be absent or exactly one')
        names = {str(r.get('file', '')).replace('\\', '/').rsplit('/', 1)[-1] for r in rows}
        evidence = dict(kind='compile-commands', sha256=helper.digest(database))
    else:
        import xml.etree.ElementTree as ET
        if not cache.get('CMAKE_GENERATOR', '').startswith('Visual Studio'):
            raise helper.BuildError('native selection needs compile commands or native VS projects')
        relative = 'src/bc/libbc.vcxproj'
        project = build / relative
        try:
            tree = ET.parse(str(project)).getroot()
        except (ET.ParseError, OSError) as error:
            raise helper.BuildError('invalid native Release project: ' + str(error))
        names = set()
        definitions = []
        vm_items = []
        for node in tree.iter():
            tag = node.tag.rsplit('}', 1)[-1]
            if tag == 'ClCompile' and node.get('Include'):
                name = node.get('Include').replace('\\', '/').rsplit('/', 1)[-1]
                names.add(name)
                if name == 'vm.cc':
                    vm_items.append(node)
            if tag == 'AdditionalOptions' and (define.findall(node.text or '') or undefine.search(node.text or '')):
                raise helper.BuildError('native project options override its definition')
            if tag == 'UndefinePreprocessorDefinitions' and macro in (node.text or ''):
                raise helper.BuildError('native project undefines its selection')
            if tag != 'ItemDefinitionGroup':
                continue
            condition = node.get('Condition', '')
            if condition and not re.search(r'Release\|', condition, re.IGNORECASE):
                continue
            for child in node.iter():
                if child.tag.rsplit('}', 1)[-1] == 'PreprocessorDefinitions':
                    definitions.append(re.findall(r'(?:^|;)' + re.escape(macro) + r'\b(?:=([^;]+))?', child.text or ''))
        if len(vm_items) != 1 or not definitions or any(d != definitions[0] for d in definitions):
            raise helper.BuildError('missing or ambiguous native Release definitions')
        values = definitions[0]
        if values not in ([], ['1']):
            raise helper.BuildError('invalid native Release definition')
        for child in vm_items[0]:
            if child.tag.rsplit('}', 1)[-1] == 'PreprocessorDefinitions':
                text = child.text or ''
                own = re.findall(r'(?:^|;)' + re.escape(macro) + r'\b(?:=([^;]+))?', text)
                if (own and own != values) or (own != values and '%(PreprocessorDefinitions)' not in text):
                    raise helper.BuildError('native VM item overrides Release definitions')
        evidence = dict(kind='msbuild-projects', files={relative: helper.digest(project)},
                        retained_root='native-policy')
    backends = names & set(backend_names)
    enabled = values == ['1']
    if ((enabled and (not configured or len(backends) != 1)) or
            (not enabled and (backends or shared_compilation in names))):
        raise helper.BuildError('native option, VM and compiled backend disagree')
    return dict(status='recorded', mode='AUTO', configured=configured, enabled=enabled,
                backend=next(iter(backends)) if enabled else None,
                shared_plan=shared_compilation in names,
                verification=evidence, cache_sha256=helper.digest(cache_path),
                scope='Compile-time eligibility only; no assertion that any input executes native code')



def verified_native_scalars(helper, build):
    """A requested cache option alone does not establish compiled eligibility."""
    return verified_native_backend(helper, build, 'ENABLE_BC_NATIVE_SCALAR',
        'GD_BC_NATIVE_SCALAR', ('native-scalar.cc', 'native-scalar-x64.cc'),
        'native-scalar-plan.cc')


def verified_native_functions(helper, build, source):
    """Record actual leaf compilation and its same-directory shared planner.

    Unsupported targets can legitimately request the option and remain generic.
    An activation cohort must separately require enabled compilation and actual
    completion in matched diagnostics. Source hashes do not prove a dynamic path.
    """
    policy = verified_native_backend(helper, build, 'ENABLE_BC_NATIVE_FUNCTIONS',
        'GD_BC_NATIVE_FUNCTIONS', ('native-expression-x64.cc', 'native-expression-arm64.cc'), None)
    if policy.get('verification', {}).get('kind') == 'msbuild-projects':
        policy['verification']['retained_root'] = 'native-function-policy'
    if policy['status'] == 'recorded' and policy['enabled']:
        header = source / 'src/bc/native-expression-plan.hh'
        backend = source / 'src/bc' / policy['backend']
        if (not header.is_file() or not backend.is_file() or
                '#include "native-expression-plan.hh"' not in backend.read_text()):
            raise helper.BuildError('native function backend lacks its shared planner include')
        policy['shared_plan'] = True
        policy['source_inputs'] = [dict(path=path.relative_to(source).as_posix(),
            sha256=helper.digest(path), retained_path='source/' + path.relative_to(source).as_posix())
            for path in (header, backend)]
    policy['scope'] = ('Compiled selection and source-bound planner include; '
                       'no claim that any input executes native code')
    return policy


def require_native_function_candidate(helper, entry):
    metadata = entry.get('metadata') if isinstance(entry, dict) else None
    config = metadata.get('configuration') if isinstance(metadata, dict) else None
    policy = config.get('native_function_policy') if isinstance(config, dict) else None
    inputs = policy.get('source_inputs') if isinstance(policy, dict) else None
    if (not isinstance(policy, dict) or policy.get('status') != 'recorded' or
            policy.get('configured') is not True or policy.get('enabled') is not True or
            policy.get('shared_plan') is not True or policy.get('backend') not in
            ('native-expression-x64.cc', 'native-expression-arm64.cc') or
            not isinstance(inputs, list) or len(inputs) != 2 or
            any(not isinstance(item, dict) or not isinstance(item.get('path'), str) or
                not isinstance(item.get('sha256'), str) or
                not re.fullmatch('[0-9a-f]{64}', item['sha256']) for item in inputs) or
            {item.get('path') for item in inputs} !=
            {'src/bc/native-expression-plan.hh', 'src/bc/' + policy['backend']}):
        raise helper.BuildError('candidate lacks verified native function backend and planner')


def require_native_candidate(helper, entry):
    metadata = entry.get('metadata') if isinstance(entry, dict) else None
    config = metadata.get('configuration') if isinstance(metadata, dict) else None
    policy = config.get('native_scalar_policy') if isinstance(config, dict) else None
    if (not isinstance(policy, dict) or policy.get('status') != 'recorded' or
            policy.get('configured') is not True or policy.get('enabled') is not True or
            policy.get('shared_plan') is not True):
        raise helper.BuildError('candidate lacks verified native backend and shared eligibility proof')


def verified_array_layout(helper, build, requested):
    """Do not accept a cache variable ignored by an older source revision."""
    observed = cache_values(build / 'CMakeCache.txt').get('BC_ARRAY_STORAGE', 'hybrid')
    if observed != requested:
        raise helper.BuildError('revision did not build the requested array storage layout')
    if requested != 'hybrid':
        commands = build / 'compile_commands.json'
        number = ('hybrid', 'paged', 'flat', 'hash', 'ordered').index(requested)
        if not commands.is_file() or not re.search(
                r'GD_BC_ARRAY_STORAGE=' + str(number) + r'(?![0-9])',
                commands.read_text(encoding='utf-8')):
            raise helper.BuildError('cannot verify the experimental array layout compilation')
    return observed


def positive_point(text):
    if not re.fullmatch(r'[1-9][0-9]*', text) or len(text) > 10 or int(text) > 1000000000:
        raise argparse.ArgumentTypeError('must be an integer from 1 through 1000000000')
    return int(text)


def revision_value(text):
    if not re.fullmatch(r'[0-9a-fA-F]{40}', text):
        raise argparse.ArgumentTypeError('use the full immutable 40-digit commit SHA')
    return text.lower()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', default=str(Path(__file__).resolve().parents[3]),
                        help='local Git repository or public GD URL; local edits are excluded')
    parser.add_argument('--revision', type=revision_value, required=True)
    parser.add_argument('--split-point', type=positive_point, required=True)
    parser.add_argument('--output', type=Path, required=True, help='fresh source/build/artifact directory')
    parser.add_argument('--name', default='gd')
    parser.add_argument('--c-compiler', default='cl' if os.name == 'nt' else 'cc')
    parser.add_argument('--cxx-compiler', default='cl' if os.name == 'nt' else 'c++')
    parser.add_argument('--generator', help='default Ninja when available, otherwise CMake platform default')
    parser.add_argument('--extensions', choices=('ON', 'OFF'), default='ON')
    parser.add_argument('--ipo-mode', choices=('AUTO', 'OFF', 'REQUIRED'), default='AUTO',
                        help='automatic IPO policy; explicit modes require source support')
    parser.add_argument('--prepared-divisors', choices=('AUTO', 'ON', 'OFF'), default='AUTO',
                        help='VM divisor reuse policy; explicit modes require compiled source support')
    parser.add_argument('--preparsed-library', action='store_true',
                        help='enable and retain the generated math-library image')
    parser.add_argument('--array-storage', choices=('hybrid', 'paged', 'flat', 'hash', 'ordered'),
                        default='hybrid', help='explicit experimental array layout')
    parser.add_argument('--diagnostic-profile', action='store_true',
                        help='build counters separately; resulting config is forbidden for timing')
    parser.add_argument('--jobs', type=int, default=2)
    parser.add_argument('--command-timeout', type=int, default=900)
    args = parser.parse_args()
    if args.jobs <= 0 or args.command_timeout <= 0:
        parser.error('--jobs and --command-timeout must be positive')
    if args.source not in ('https://github.com/matt-gretton-dann/gd-posix-apps.git',
                            'https://github.com/matt-gretton-dann/gd-posix-apps') and not Path(args.source).is_dir():
        parser.error('--source must be a local repository or the public GD repository URL')
    root = args.output.resolve()
    try:
        root.mkdir(parents=True, exist_ok=False)
    except OSError as error:
        parser.error('cannot create fresh build directory: ' + str(error))
    helper = helpers()
    builder = helper.Builder(root, args.command_timeout)
    manifest = {'schema': 1, 'builder_sha256': helper.digest(__file__),
                'helper_sha256': helper.digest(Path(__file__).with_name('build-bc-comparators.py')),
                'commands': builder.commands, 'status': 'incomplete'}
    try:
        source = root / 'source'
        builder.run(['git', 'clone', '--no-hardlinks', '--no-checkout', args.source, str(source)])
        builder.run(['git', 'checkout', '--detach', args.revision], source)
        actual = builder.run(['git', 'rev-parse', 'HEAD'], source)
        if actual != args.revision:
            raise helper.BuildError('source revision differs from requested immutable commit')
        if not (source / 'cmake/BcMultiplyConfig.cmake').is_file():
            raise helper.BuildError('source revision predates the fixed-threshold build API')
        compiler_c = compiler_identity(helper, builder, args.c_compiler)
        compiler_cxx = compiler_identity(helper, builder, args.cxx_compiler)
        build = root / 'build'
        configure = ['cmake', '-S', str(source), '-B', str(build), '-DCMAKE_BUILD_TYPE=Release',
                     '-DBUILD_TESTING=OFF', '-DENABLE_BC_PROFILE=' + ('ON' if args.diagnostic_profile else 'OFF'),
                     '-DENABLE_BC_COVERAGE=OFF',
                     '-DENABLE_NON_POSIX_EXTENSIONS=' + args.extensions,
                     '-DBC_MULTIPLY_SPLIT_POINT=' + str(args.split_point),
                     '-DCMAKE_EXPORT_COMPILE_COMMANDS=ON',
                     '-DCMAKE_C_COMPILER:FILEPATH=' + compiler_c['argv0'],
                     '-DCMAKE_CXX_COMPILER:FILEPATH=' + compiler_cxx['argv0']]
        if args.array_storage != 'hybrid':
            configure += ['-DBC_ARRAY_STORAGE=' + args.array_storage]
        if args.prepared_divisors != 'AUTO':
            configure += ['-DENABLE_BC_PREPARED_DIVISORS=' + args.prepared_divisors]
        if args.preparsed_library:
            configure += ['-DENABLE_BC_PREPARSED_LIBRARY=ON']
        if args.ipo_mode != 'AUTO':
            configure += ['-DIPO_MODE=' + args.ipo_mode]
        generator = args.generator or ('Ninja' if shutil.which('ninja') else None)
        if generator:
            configure += ['-G', generator]
        environment = build_environment()
        builder.run(configure, environment=environment)
        builder.run(['cmake', '--build', str(build), '--config', 'Release', '--target', 'bc',
                     '--parallel', str(args.jobs), '--verbose'], environment=environment)
        executable = build / ('src/bc/bc.exe' if os.name == 'nt' else 'src/bc/bc')
        if not executable.is_file() and os.name == 'nt':
            executable = build / 'src/bc/Release/bc.exe'
        if not executable.is_file():
            raise helper.BuildError('build did not produce the expected bc executable')
        metadata = build_metadata(helper, root, source, build, actual, args.split_point,
                                  compiler_c, compiler_cxx, args.ipo_mode, args.prepared_divisors,
                                  diagnostic_profile=args.diagnostic_profile, builder=builder)
        observed_layout = verified_array_layout(helper, build, args.array_storage)
        metadata['configuration']['array_storage'] = observed_layout
        image = build / 'src/bc/bc-math-library-image.inc'
        if args.preparsed_library and not image.is_file():
            raise helper.BuildError('requested preparsed math-library image was not built')
        if image.is_file():
            retained = root / image.name
            shutil.copyfile(image, retained)
            metadata['configuration']['preparsed_library_image_sha256'] = helper.digest(retained)
        metadata['binary_sha256'] = helper.digest(executable)
        metadata['standard_library'] = standard_library_info(helper, builder, compiler_cxx, root)
        helper.write_json(root / 'implementations.json', {'implementations': [
            {'name': args.name, 'argv': [str(executable)], 'metadata': metadata}]})
        manifest['status'] = ('built diagnostic counters; forbidden for elapsed timing' if args.diagnostic_profile
                              else 'built; run checked corpus before timing')
        print(root / 'implementations.json')
        return 0
    except (helper.BuildError, OSError, ValueError) as error:
        manifest['failure'] = str(error)
        print('GD build failed: ' + str(error), file=sys.stderr)
        return 1
    finally:
        helper.write_json(root / 'build-manifest.json', manifest)


if __name__ == '__main__':
    sys.exit(main())
