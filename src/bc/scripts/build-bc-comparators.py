#!/usr/bin/env python3
"""Build pinned Howard/GNU comparators in a fresh directory (Python 3.6+).

Commands use argv lists and bounded subprocesses. Logs and metadata describe
actual local builds, including source and compiler checksums; no credentials or
entire environment dumps are recorded. SPDX-License-Identifier: Apache-2.0
"""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
import sys
import tarfile
import urllib.request

HOWARD_REVISION = 'c8dabac65d39fd527a9264a5e505a99b1d8df3d8'
HOWARD_REPOSITORY = 'https://github.com/gavinhoward/bc.git'
GNU_URL = 'https://ftp.gnu.org/gnu/bc/bc-1.08.2.tar.gz'
GNU_SHA256 = 'ae470fec429775653e042015edc928d07c8c3b2fc59765172a330d3d87785f86'


class BuildError(RuntimeError):
    pass


def digest(path):
    value = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            value.update(block)
    return value.hexdigest()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n', encoding='utf-8')


class Builder:
    def __init__(self, root, timeout):
        self.root = root
        self.timeout = timeout
        self.commands = []

    def run(self, argv, cwd=None, environment=None, allowed=(0,)):
        number = len(self.commands) + 1
        log = self.root / ('command-{:03d}.log'.format(number))
        row = {'argv': list(map(str, argv)), 'cwd': str(cwd or self.root), 'log': log.name}
        self.commands.append(row)
        try:
            process = subprocess.run(row['argv'], cwd=str(cwd or self.root),
                                     env=environment, stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT, timeout=self.timeout)
        except subprocess.TimeoutExpired as error:
            log.write_bytes(error.output or b'')
            row['status'] = 'timeout'
            raise BuildError('command {} exceeded its deadline; see {}'.format(number, log))
        log.write_bytes(process.stdout)
        row['returncode'] = process.returncode
        if process.returncode not in allowed:
            detail = '\n'.join(process.stdout.decode('utf-8', errors='replace').splitlines()[-25:])
            raise BuildError('command {} failed with {}; see {}\n{}'.format(
                number, process.returncode, log, detail))
        return process.stdout.decode('utf-8', errors='replace').strip()


def safe_extract(archive, destination):
    """The pinned checksum is checked before extraction; reject unsafe members too."""
    destination = destination.resolve()
    with tarfile.open(str(archive), 'r:gz') as source:
        for member in source.getmembers():
            target = (destination / member.name).resolve()
            if destination not in target.parents or member.issym() or member.islnk():
                raise BuildError('archive contains an unsupported path or link: ' + member.name)
            if not (member.isfile() or member.isdir()):
                raise BuildError('archive contains a special file: ' + member.name)
        source.extractall(str(destination))


def compiler_metadata(builder, compiler):
    resolved = shutil.which(compiler)
    if not resolved:
        raise BuildError('compiler not found: ' + compiler)
    version = builder.run([resolved] if Path(resolved).stem.lower() == 'cl'
                          else [resolved, '--version'], allowed=(0, 2))
    return {'argv0': str(Path(resolved).resolve()), 'version': version,
            'sha256': digest(resolved)}


def make_variables(builder, source, make):
    # Ask make to expand its actual compiler flags; raw $(CPPFLAGS) references
    # are insufficient provenance. This target performs no compilation.
    recipe = source / 'gd-provenance.mk'
    names = ('CC', 'CFLAGS', 'CPPFLAGS', 'LDFLAGS')
    recipe.write_text('.PHONY: gd-provenance\ngd-provenance:\n' + ''.join(
        '\t@$(info GD_FLAGS_{}=$({}))\n'.format(name, name) for name in names),
        encoding='utf-8')
    output = builder.run([make, '-s', '-f', 'Makefile', '-f', recipe.name, 'gd-provenance'], source)
    return dict(line[len('GD_FLAGS_'):].split('=', 1) for line in output.splitlines()
                if line.startswith('GD_FLAGS_'))


def build_environment(compiler):
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith(('BC_', 'DC_')) and key not in ('CL', '_CL_')}
    environment.update(CC=compiler['argv0'], HOSTCC=compiler['argv0'],
                       CFLAGS='-O3 -DNDEBUG', HOSTCFLAGS='-O3 -DNDEBUG',
                       CPPFLAGS='', LDFLAGS='', CONFIG_SITE=os.devnull)
    return environment


def msvc_toolset(requested, installation):
    if requested:
        return requested
    if not installation:
        raise BuildError('use a Visual Studio developer prompt or specify --msvc-toolset')
    root = Path(installation) / 'MSBuild/Microsoft/VC'
    candidates = set()
    for directory in root.rglob('PlatformToolsets'):
        if directory.parent.name.lower() == 'x64':
            candidates.update(path.name for path in directory.iterdir()
                              if path.is_dir() and re.fullmatch(r'v[0-9]+', path.name))
    if not candidates:
        raise BuildError('cannot identify installed x64 MSVC toolset; use --msvc-toolset')
    return max(candidates, key=lambda name: int(name[1:]))


def build_howard(builder, args):
    source = builder.root / 'howard-source'
    builder.run(['git', 'clone', '--no-hardlinks', '--depth', '1', '--branch', '7.0.3',
                 args.howard_repository, str(source)])
    revision = builder.run(['git', 'rev-parse', 'HEAD'], source)
    if revision != HOWARD_REVISION:
        raise BuildError('Howard 7.0.3 did not resolve to its pinned commit')
    if os.name == 'nt':
        compiler = compiler_metadata(builder, args.cc or 'cl')
        toolset = msvc_toolset(args.msvc_toolset, os.environ.get('VSINSTALLDIR'))
        builder.run([args.msbuild, 'vs/bc.sln', '/m:{}'.format(args.jobs),
                     '/p:Configuration=Release', '/p:Platform=x64', '/p:PlatformToolset=' + toolset,
                     '/p:CLToolPath=' + str(Path(compiler['argv0']).parent) + os.sep,
                     '/p:CLToolExe=' + Path(compiler['argv0']).name], source,
                    build_environment(compiler))
        executable = source / 'vs/bin/x64/Release/bc.exe'
        flags = {'configuration': 'Release', 'platform': 'x64', 'toolset': toolset,
                 'project_sha256': digest(source / 'vs/bc.vcxproj')}
        command_logs = list((source / 'vs/bin/x64/Release').rglob('CL.command.1.tlog'))
        if not command_logs:
            raise BuildError('MSBuild did not preserve actual compiler command tracking logs')
        flags['native_compile_commands'] = [path.read_text(encoding='utf-16')
                                            for path in command_logs]
        options = 'native MSBuild Release/x64 with recorded installed toolset'
    else:
        compiler = compiler_metadata(builder, args.cc or 'cc')
        environment = build_environment(compiler)
        configure = ['sh', 'configure.sh', '--opt=3', '--disable-nls', '--disable-history',
                     '--disable-dc', '--disable-man-pages', '--disable-generated-tests',
                     '--disable-strip']
        builder.run(configure, source, environment)
        builder.run([args.make, '-j{}'.format(args.jobs)], source, environment)
        executable = source / 'bin/bc'
        flags = make_variables(builder, source, args.make)
        options = configure[2:]
    if not executable.is_file():
        raise BuildError('Howard build did not produce its expected executable')
    version = builder.run([str(executable), '--version'])
    return {'name': 'howard-7.0.3', 'role': 'reference', 'argv': [str(executable)],
            'metadata': {'source_revision': revision, 'source_url': HOWARD_REPOSITORY,
                         'compiler': compiler, 'flags': flags, 'configuration': options,
                         'version': version, 'instrumented': False,
                         'binary_sha256': digest(executable),
                         'tuning': {'BC_NUM_KARATSUBA_LEN': 32}}}


def build_gnu(builder, args):
    if os.name == 'nt':
        raise BuildError('GNU POSIX configure recipe is unavailable on native Windows; select Howard')
    archive = builder.root / 'bc-1.08.2.tar.gz'
    if args.gnu_archive:
        shutil.copyfile(str(args.gnu_archive), str(archive))
    else:
        with urllib.request.urlopen(GNU_URL, timeout=30) as remote, archive.open('wb') as local:
            shutil.copyfileobj(remote, local)
    if digest(archive) != GNU_SHA256:
        raise BuildError('GNU archive checksum does not match the pinned 1.08.2 source')
    safe_extract(archive, builder.root)
    source = builder.root / 'bc-1.08.2'
    compiler = compiler_metadata(builder, args.cc or 'cc')
    environment = build_environment(compiler)
    builder.run(['sh', './configure', '--disable-nls', '--without-readline'], source, environment)
    builder.run([args.make, '-j{}'.format(args.jobs), '-C', 'lib'], source, environment)
    builder.run([args.make, '-j{}'.format(args.jobs), '-C', 'bc'], source, environment)
    executable = source / 'bc/bc'
    version = builder.run([str(executable), '--version'])
    return {'name': 'gnu-1.08.2', 'role': 'reference', 'argv': [str(executable)],
            'metadata': {'source_revision': 'bc-1.08.2 sha256:' + GNU_SHA256,
                         'source_url': GNU_URL, 'compiler': compiler,
                         'flags': make_variables(builder, source / 'bc', args.make),
                         'configuration': ['--disable-nls', '--without-readline'],
                         'version': version, 'instrumented': False,
                         'binary_sha256': digest(executable), 'tuning': 'upstream 1.08.2 defaults'}}


def positive(value):
    result = int(value)
    if result <= 0:
        raise argparse.ArgumentTypeError('must be positive')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True, help='fresh build/artifact directory')
    parser.add_argument('--implementation', choices=('howard', 'gnu', 'both'), default='howard')
    parser.add_argument('--cc', help='explicit C compiler; record its version and checksum')
    parser.add_argument('--make', default='make')
    parser.add_argument('--msbuild', default='MSBuild.exe', help='native Windows MSBuild command')
    parser.add_argument('--msvc-toolset', help='explicit native toolset, otherwise use installed x64 default')
    parser.add_argument('--jobs', type=positive, default=2)
    parser.add_argument('--command-timeout', type=positive, default=900)
    parser.add_argument('--howard-repository', default=HOWARD_REPOSITORY,
                        help='upstream or a local git cache; pinned commit is always verified')
    parser.add_argument('--gnu-archive', type=Path, help='offline archive; checksum is always verified')
    args = parser.parse_args()
    root = args.output.resolve()
    try:
        root.mkdir(parents=True, exist_ok=False)
    except OSError as error:
        parser.error('cannot create a fresh directory: ' + str(error))
    builder = Builder(root, args.command_timeout)
    manifest = {'schema': 1, 'started_utc': datetime.now(timezone.utc).isoformat(),
                'platform': {'system': platform.system(), 'release': platform.release(),
                             'machine': platform.machine()}, 'commands': builder.commands,
                'builder_sha256': digest(__file__), 'status': 'incomplete'}
    try:
        entries = []
        if args.implementation in ('howard', 'both'):
            entries.append(build_howard(builder, args))
        if args.implementation in ('gnu', 'both'):
            entries.append(build_gnu(builder, args))
        write_json(root / 'implementations.json', {'implementations': entries})
        manifest['status'] = 'built; correctness must pass the checked corpus before timing'
        print(root / 'implementations.json')
        return 0
    except (BuildError, OSError, ValueError, tarfile.TarError) as error:
        manifest['failure'] = str(error)
        print('Comparator build failed: ' + str(error), file=sys.stderr)
        return 1
    finally:
        write_json(root / 'build-manifest.json', manifest)


if __name__ == '__main__':
    sys.exit(main())
