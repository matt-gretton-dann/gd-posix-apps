#!/usr/bin/env python3
"""Verify actual portable backend objects, shared ABI and opaque IPO boundary.

Python 3.6+; SPDX-License-Identifier: Apache-2.0
"""
import json
from pathlib import Path
import posixpath
import re
import shlex
import xml.etree.ElementTree as ET


BACKEND = 'bc-portable-scalar'
SOURCES = {'portable-scalar.cc', 'portable-scalar-vm.cc'}


def verify(helper, build, cache, enabled):
    def fail(message):
        raise helper.BuildError('portable object boundary: ' + message)

    def normal(path):
        path = posixpath.normpath(str(path).replace('\\', '/'))
        return path.casefold() if re.match(r'^[A-Za-z]:/', path) else path

    def boolean(key):
        value = cache.get(key, '').upper()
        if value not in ('ON', 'TRUE', 'YES', '1', 'OFF', 'FALSE', 'NO', '0'):
            fail('missing/invalid configured ' + key)
        return '1' if value in ('ON', 'TRUE', 'YES', '1') else '0'

    report = build / 'src/bc/portable-scalar-boundary-Release.txt'
    try:
        lines = report.read_text(encoding='utf-8').splitlines()
        split = lines.index('[objects]')
        library = lines.index('[library-sources]')
        header = {}
        for line in lines[:split]:
            key, sep, value = line.partition('=')
            if not sep or key in header:
                fail('invalid or duplicate contract field')
            header[key] = value
        objects = [normal(line) for line in lines[split + 1:library] if line]
        library_sources = [normal(line) for line in lines[library + 1:] if line]
    except (ValueError, OSError) as error:
        fail('unavailable target-source/IPO contract: ' + str(error))
    if (set(header) != {'schema', 'mode', 'configuration', 'core', 'backend', 'ipo', 'config-ipo'} or
            header['schema'] != '1' or header['configuration'] != 'Release' or
            header['core'] != 'libbc' or header['mode'] != ('ON' if enabled else 'OFF')):
        fail('contract does not describe the requested Release mode')
    if len(objects) != len(set(objects)):
        fail('duplicate object in target contract')
    if sum(Path(p).name == 'vm.cc' for p in library_sources) != 1:
        fail('core target lost its VM source')
    if enabled:
        if header['backend'] != BACKEND or any(header[k].upper() not in ('FALSE', 'OFF', 'NO', '0')
                                               for k in ('ipo', 'config-ipo')):
            fail('backend target did not disable IPO')
        if len(objects) != 2 or not set(objects).issubset(library_sources):
            fail('backend objects are not included in the core library')
        if not all('/' + BACKEND + '.dir/' in '/' + p for p in objects):
            fail('contract objects are not owned by the production backend')
    elif (header['backend'] != 'disabled' or objects or
          header['ipo'] != 'not-applicable' or header['config-ipo'] != 'not-applicable' or
          any('/' + BACKEND + '.dir/' in '/' + p for p in library_sources)):
        fail('disabled mode still includes a backend')

    expected = {'GD_BC_PORTABLE_SCALAR': '1' if enabled else '0',
                'GD_BC_PROFILE': boolean('ENABLE_BC_PROFILE'),
                'GD_BC_PREPARED_DIVISORS': boolean('ENABLE_BC_PREPARED_DIVISORS'),
                'ENABLE_EXTENSIONS': boolean('ENABLE_NON_POSIX_EXTENSIONS'),
                'GD_BC_EXPERIMENT_NTT': boolean('ENABLE_BC_EXACT_SQUARE')}
    layouts = ('hybrid', 'paged', 'flat', 'hash', 'ordered')
    if cache.get('BC_ARRAY_STORAGE') not in layouts:
        fail('unknown configured array layout')
    expected['GD_BC_ARRAY_STORAGE'] = str(layouts.index(cache['BC_ARRAY_STORAGE']))

    def arguments(text):
        try:
            lexer = shlex.shlex(text, posix=True)
            lexer.whitespace_split = True
            lexer.commenters = ''
            lexer.escape = ''
            return list(lexer)
        except (ValueError, TypeError) as error:
            fail('unreadable compiler options: ' + str(error))

    def options(argv, backend):
        if not isinstance(argv, list) or not all(isinstance(x, str) for x in argv):
            fail('invalid compiler arguments')
        definitions = {}
        index = 0
        while index < len(argv):
            arg = argv[index]
            if arg.startswith('@'):
                fail('unexpanded response file')
            if backend and (arg.startswith('-flto') or arg.lower() == '/gl' or
                            re.match(r'^[-/]gl[:=]', arg, re.IGNORECASE)):
                fail('backend compiler options enable IPO')
            option = arg[:2]
            if option in ('-D', '/D', '-U', '/U'):
                value = arg[2:]
                if not value:
                    index += 1
                    if index == len(argv):
                        fail('missing compiler definition argument')
                    value = argv[index]
                key, _, setting = value.partition('=')
                if key == 'GD_BC_NATIVE_SCALAR':
                    fail('separate native emitter definition is present')
                if key in expected:
                    if option in ('-U', '/U'):
                        fail('compiler undefines a shared ABI/mode definition')
                    definitions.setdefault(key, set()).add(setting)
            index += 1
        for key, value in expected.items():
            actual = definitions.get(key, set())
            if not actual and key in ('ENABLE_EXTENSIONS', 'GD_BC_EXPERIMENT_NTT'):
                actual = {'0'}
            if actual != {value}:
                fail('inconsistent compiled ABI/mode definition: ' + key)

    commands = build / 'compile_commands.json'
    selected, linked = [], []
    if commands.is_file():
        try:
            rows = json.loads(commands.read_text(encoding='utf-8'))
        except (ValueError, OSError) as error:
            fail('invalid compilation database: ' + str(error))
        if not isinstance(rows, list) or any(not isinstance(r, dict) for r in rows):
            fail('invalid compiler inventory')
        for row in rows:
            name = Path(str(row.get('file', '')).replace('\\', '/')).name
            if name not in SOURCES | {'vm.cc'}:
                continue
            argv = row['arguments'] if 'arguments' in row else arguments(row.get('command', ''))
            if not isinstance(argv, list) or not all(isinstance(x, str) for x in argv):
                fail('invalid compiler arguments')
            outputs = []
            for index, arg in enumerate(argv):
                if arg == '-o' and index + 1 < len(argv):
                    outputs.append(argv[index + 1])
                elif arg.startswith(('/Fo', '-Fo')):
                    outputs.append(arg[3:] or (argv[index + 1] if index + 1 < len(argv) else ''))
            if 'output' in row:
                outputs.append(row['output'])
            owner = 'libbc' if name == 'vm.cc' else BACKEND
            if not outputs or not all(isinstance(p, str) for p in outputs):
                continue
            if name in SOURCES and any('/libbc.dir/' in '/' + normal(p) for p in outputs):
                fail('backend source was compiled inside the core target')
            if not all('/' + owner + '.dir/' in '/' + normal(p) for p in outputs):
                continue  # Standalone unit/test components are not production ownership.
            if name in SOURCES and not enabled:
                fail('disabled backend was compiled as a production object')
            options(argv, name in SOURCES)
            selected.append(name)
            if name in SOURCES:
                absolute = set()
                for output in outputs:
                    output = normal(output)
                    if not output.startswith('/') and not re.match(r'^[A-Za-z]:/', output):
                        output = normal(posixpath.join(str(row.get('directory', build)).replace('\\', '/'), output))
                    absolute.add(output)
                if len(absolute) != 1:
                    fail('compiler outputs disagree')
                linked.extend(absolute)
        if set(linked) != set(objects):
            fail('compiled backend outputs do not match linked target objects')
        evidence = dict(kind='compile-commands', sha256=helper.digest(commands),
                        translation_units=len(selected))
    else:
        if not cache.get('CMAKE_GENERATOR', '').startswith('Visual Studio'):
            fail('need compilation database or native MSBuild projects')

        def tag(node):
            return node.tag.rsplit('}', 1)[-1]

        def release(node):
            condition = node.get('Condition', '').strip()
            if not condition:
                return True
            match = re.fullmatch(r'''['"]\$\(Configuration\)(?:\|\$\(Platform\))?['"]\s*==\s*['"]([^'"]+)['"]''', condition)
            if not match:
                fail('unsupported conditional project evidence')
            return match.group(1).split('|')[0] == 'Release'

        files = {}
        for target, wanted in [('libbc', {'vm.cc'})] + ([(BACKEND, SOURCES)] if enabled else []):
            relative = 'src/bc/' + target + '.vcxproj'
            path = build / relative
            try:
                project = ET.parse(str(path)).getroot()
            except (ET.ParseError, OSError) as error:
                fail('cannot inspect native project: ' + str(error))
            settings = []
            references = []
            for group in project:
                if not release(group):
                    continue
                if tag(group) == 'ItemDefinitionGroup':
                    settings.extend(c for c in group if tag(c) == 'ClCompile' and release(c))
                if tag(group) == 'ItemGroup':
                    references.extend(n for n in group if tag(n) == 'ProjectReference' and release(n))
            if not settings:
                fail('missing Release compiler settings')

            def project_options(node, inherited=None):
                argv = list(inherited or [])
                for child in node:
                    if not release(child):
                        continue
                    text = child.text or ''
                    key = tag(child)
                    if key == 'PreprocessorDefinitions':
                        parts = [part.strip() for part in text.split(';')]
                        if '%(PreprocessorDefinitions)' not in parts:
                            argv = [arg for arg in argv if not arg.startswith(('-D', '/D'))]
                        argv += ['-D' + p for p in parts if p and p != '%(PreprocessorDefinitions)']
                    elif key == 'AdditionalOptions':
                        argv += arguments(text)
                    elif key == 'UndefinePreprocessorDefinitions':
                        argv += ['-U' + p.strip() for p in text.split(';') if p.strip()]
                    elif key == 'ExcludedFromBuild' and text.strip().lower() != 'false':
                        fail('production source excluded from Release')
                    elif target == BACKEND and key == 'WholeProgramOptimization' and text.strip().lower() != 'false':
                        fail('native backend enables whole-program optimization')
                return argv

            for group in project:
                if target == BACKEND and release(group) and tag(group) == 'PropertyGroup':
                    for child in group:
                        if tag(child) == 'WholeProgramOptimization' and release(child) and (child.text or '').strip().lower() != 'false':
                            fail('native backend property enables IPO')
            inherited = [project_options(setting) for setting in settings]
            for args in inherited:
                options(args, target == BACKEND)
            for group in project:
                if tag(group) != 'ItemGroup' or not release(group):
                    continue
                for item in group:
                    name = Path(item.get('Include', '').replace('\\', '/')).name
                    if tag(item) != 'ClCompile' or not release(item):
                        continue
                    if target == 'libbc' and name in SOURCES:
                        fail('native core project still compiles a backend source')
                    if name not in wanted:
                        continue
                    for args in inherited:
                        options(project_options(item, args), target == BACKEND)
                    selected.append(name)
            if target == 'libbc' and enabled:
                references = [n for n in references if Path(n.get('Include', '').replace('\\', '/')).name == BACKEND + '.vcxproj']
                if len(references) != 1 or any(tag(c) == 'BuildReference' and (c.text or '').strip().lower() == 'false' for c in references[0]):
                    fail('core project does not reference the production backend')
            files[relative] = helper.digest(path)
        if enabled:
            stems = {Path(p).name.replace('.cc.obj', '.obj') for p in objects}
            if stems != {'portable-scalar.obj', 'portable-scalar-vm.obj'}:
                fail('native linked object inventory disagrees with backend sources')
        evidence = dict(kind='msbuild-projects', files=files, retained_root='portable-scalar-policy')
    wanted = SOURCES | {'vm.cc'} if enabled else {'vm.cc'}
    if set(selected) != wanted or len(selected) != len(wanted):
        fail('missing/duplicate production source inventory')
    relative = str(report.relative_to(build)).replace('\\', '/')
    evidence['boundary_report'] = dict(path=relative, sha256=helper.digest(report))
    boundary = dict(kind='opaque-object' if enabled else 'disabled-object',
                    target=BACKEND if enabled else None, ipo=False if enabled else None,
                    configuration='Release', linked_object_count=len(objects))
    return boundary, evidence
