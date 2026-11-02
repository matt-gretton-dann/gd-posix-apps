#!/usr/bin/env python3
"""Build an immutable GD revision with fixed dispatch and actual provenance.

Use the sibling comparator builder for shared bounded command/log handling.
Python 3.6+; SPDX-License-Identifier: Apache-2.0
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
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


def build_metadata(helper, root, source, build, revision, split, compiler_c, compiler_cxx):
    cache = cache_values(build / 'CMakeCache.txt')
    for flag in ('ENABLE_BC_PROFILE', 'ENABLE_BC_COVERAGE'):
        if cache.get(flag, '').upper() not in ('OFF', 'FALSE', 'NO', '0'):
            raise helper.BuildError('release timing configuration must disable ' + flag)
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
    if re.search(r'-fprofile(?:-instr)?-generate|-fprofile-arcs|-ftest-coverage|-fcoverage|--coverage|-fsanitize|/fsanitize|-finstrument-functions|GD_BC_PROFILE=1',
                 checked_flags, re.IGNORECASE):
        raise helper.BuildError('timing build contains instrumentation flags')
    shutil.copyfile(str(build / 'src/bc/find-multiply-split-point.hh'),
                    str(root / 'multiply-threshold.hh'))
    return {'source_revision': revision,
            'compiler': {'C': compiler_c, 'CXX': compiler_cxx}, 'flags': flags,
            'version': 'gd-posix-apps source ' + revision, 'instrumented': False,
            'configuration': {'build_type': cache.get('CMAKE_BUILD_TYPE'),
                              'generator': cache.get('CMAKE_GENERATOR'),
                              'extensions': cache.get('ENABLE_NON_POSIX_EXTENSIONS'),
                              'multiply_split_point': split,
                              'threshold_header_sha256': helper.digest(root / 'multiply-threshold.hh')},
            'limitations': ['Provision compiler, C/C++ runtime and SDK separately; this is not an OS image',
                            'Compiler defaults and actual commands are recorded, not claimed identical across hosts']}


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
                     '-DBUILD_TESTING=OFF', '-DENABLE_BC_PROFILE=OFF', '-DENABLE_BC_COVERAGE=OFF',
                     '-DENABLE_NON_POSIX_EXTENSIONS=' + args.extensions,
                     '-DBC_MULTIPLY_SPLIT_POINT=' + str(args.split_point),
                     '-DCMAKE_EXPORT_COMPILE_COMMANDS=ON',
                     '-DCMAKE_C_COMPILER:FILEPATH=' + compiler_c['argv0'],
                     '-DCMAKE_CXX_COMPILER:FILEPATH=' + compiler_cxx['argv0']]
        generator = args.generator or ('Ninja' if shutil.which('ninja') else None)
        if generator:
            configure += ['-G', generator]
        environment = {key: value for key, value in os.environ.items()
                       if not key.startswith(('BC_', 'DC_')) and
                       key not in ('CFLAGS', 'CXXFLAGS', 'CPPFLAGS', 'LDFLAGS', 'CL', '_CL_')}
        builder.run(configure, environment=environment)
        builder.run(['cmake', '--build', str(build), '--config', 'Release', '--target', 'bc',
                     '--parallel', str(args.jobs), '--verbose'], environment=environment)
        executable = build / ('src/bc/bc.exe' if os.name == 'nt' else 'src/bc/bc')
        if not executable.is_file() and os.name == 'nt':
            executable = build / 'src/bc/Release/bc.exe'
        if not executable.is_file():
            raise helper.BuildError('build did not produce the expected bc executable')
        metadata = build_metadata(helper, root, source, build, actual, args.split_point,
                                  compiler_c, compiler_cxx)
        metadata['binary_sha256'] = helper.digest(executable)
        metadata['standard_library'] = standard_library_info(helper, builder, compiler_cxx, root)
        helper.write_json(root / 'implementations.json', {'implementations': [
            {'name': args.name, 'argv': [str(executable)], 'metadata': metadata}]})
        manifest['status'] = 'built; run checked corpus before timing'
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
