#!/usr/bin/env python3
"""Attribute frozen ordinary arithmetic images; never collect paired timings."""
import argparse
import copy
import hashlib
import importlib.util
import json
import os
from pathlib import Path, PurePosixPath
import platform
import random
import shutil
import stat
import subprocess
import sys
import time
import zipfile

HERE = Path(__file__).resolve().parent
MAX_ARCHIVE = 64 * 1024 * 1024
MAX_EXPANDED = 256 * 1024 * 1024


def digest(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()


def write(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n')


def load(scripts, name):
    spec = importlib.util.spec_from_file_location('frozen_cpu_' + name, str(scripts / (name + '.py')))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def check_metadata(meta, pin):
    run = meta.get('workflow_run', {})
    expected = (pin['artifact_id'], pin['artifact_name'], 'sha256:' + pin['artifact_sha256'])
    if (meta.get('id'), meta.get('name'), meta.get('digest')) != expected:
        raise ValueError('artifact metadata does not match the frozen provider identity')
    if (run.get('id'), run.get('head_sha'), run.get('repository_id'),
            run.get('head_repository_id')) != (pin['run_id'], pin['source'],
                                               pin['repository_id'], pin['repository_id']):
        raise ValueError('artifact metadata has another source, run or repository')
    if meta.get('expired') is not False:
        raise ValueError('artifact has expired or lacks an explicit availability state')


def check_members(archive):
    members = archive.infolist()
    names = [m.filename for m in members]
    if len(set(names)) != len(names) or sum(m.file_size for m in members) > MAX_EXPANDED:
        raise ValueError('duplicate archive members or expanded-size limit exceeded')
    for member in members:
        name = member.filename
        path = PurePosixPath(name)
        mode = member.external_attr >> 16
        if (not name or '\\' in name or path.is_absolute() or '..' in path.parts or
                ':' in name or (mode and not (stat.S_ISREG(mode) or stat.S_ISDIR(mode)))):
            raise ValueError('unsafe archive member: ' + name)
    if archive.testzip() is not None:
        raise ValueError('archive CRC verification failed')


def prepare(archive_path, metadata, output, scripts, pin):
    """Verify and relocate inputs only. This function never executes a child."""
    check_metadata(metadata, pin)
    if archive_path.stat().st_size > MAX_ARCHIVE or digest(archive_path) != pin['artifact_sha256']:
        raise ValueError('frozen archive digest or size mismatch')
    for name, expected in pin['scripts'].items():
        if digest(scripts / name) != expected:
            raise ValueError('profiling/replay tool differs from the declared version')
    if output.exists():
        raise ValueError('output must be a fresh directory')
    # Validate every selected byte before creating executables or a configuration.
    payloads = {}
    with zipfile.ZipFile(str(archive_path)) as archive:
        check_members(archive)
        for name, expected in pin['files'].items():
            raw = archive.read(name)
            if hashlib.sha256(raw).hexdigest() != expected:
                raise ValueError('frozen input changed: ' + name)
            payloads[name] = raw
    codegen = json.loads(payloads[pin['codegen']].decode('utf-8-sig'))
    if codegen['status'] != 'preserved':
        raise ValueError('ordinary code-generation evidence is incomplete')
    tools = []
    for role in ('base', 'candidate'):
        observations = [r for r in codegen['observations'] if r['name'] == role]
        if len(observations) != 1:
            raise ValueError('missing or ambiguous ordinary role')
        row = observations[0]
        tool = copy.deepcopy(row['implementation'])
        image = pin['images'][role]
        meta = tool['metadata']
        policy = meta['configuration']['prefix_result_policy']
        if (tool['name'] != role or len(tool['argv']) != 1 or tool.get('input_artifacts') or
                meta['source_revision'] != image['source_revision'] or
                meta['instrumented'] is not False or meta['configuration']['diagnostic_profile'] is not False or
                row['binary_sha256'] != image['sha256'] or meta['binary_sha256'] != image['sha256'] or
                row['file_bytes'] != image['bytes'] or policy['enabled'] is not (role == 'candidate') or
                policy['implementation_present'] is not (role == 'candidate')):
            raise ValueError('ordinary image source, mode, inputs or identity mismatch')
        raw = payloads[image['path']]
        # Both retained images are native little-endian x86-64 ELF, not profiles.
        if (len(raw) != image['bytes'] or raw[:6] != b'\x7fELF\x02\x01' or
                int.from_bytes(raw[18:20], 'little') != 62):
            raise ValueError('retained image is not the declared x86-64 ELF')
        tools.append((role, tool, raw))
    output.mkdir(parents=True)
    write(output / 'original-artifact-metadata.json', metadata)
    write(output / 'source-pin.json', pin)
    (output / 'original-codegen.json').write_bytes(payloads[pin['codegen']])
    configs = {}
    for role, tool, raw in tools:
        image = output / 'images' / role / 'bc'
        image.parent.mkdir(parents=True)
        image.write_bytes(raw)
        image.chmod(0o755)
        original_argv = tool['argv']
        tool['argv'] = [str(image.resolve())]
        config = output / (role + '-implementations.json')
        write(config, dict(implementations=[tool]))
        configs[role] = str(config)
        write(output / (role + '-relocation.json'), dict(
            original_argv=original_argv, diagnostic_argv=tool['argv'],
            binary_sha256=digest(image), source_revision=tool['metadata']['source_revision'],
            scope='Same retained ordinary executable bytes; original build provenance, not a new build'))
    replay = {}
    bench = load(scripts, 'bench-bc')
    for preset, group in pin['groups'].items():
        folder = output / 'replay' / preset
        prefix = group['path']
        original_manifest = json.loads(payloads[prefix + 'manifest.json'].decode('utf-8-sig'))
        if original_manifest['harness_sha256'] != pin['scripts']['bench-bc.py']:
            raise ValueError('original corpus used another replay harness')
        for name, raw in payloads.items():
            if name.startswith(prefix):
                target = folder / name[len(prefix):]
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(raw)
        cases, evidence = bench.replay_corpus(folder, 'arithmetic', group['workloads'])
        if {c['name'] for c in cases} != set(group['workloads']):
            raise ValueError('frozen selection is incomplete')
        replay[preset] = dict(directory=str(folder.resolve()), workloads=group['workloads'], evidence=evidence,
                              summary_sha256=pin['files'][prefix + 'summary.json'])
    report = dict(schema=1, status='prepared; no native child executed', configs=configs, replay=replay,
                  source=pin['source'], base=pin['base'], artifact_sha256=pin['artifact_sha256'],
                  images=pin['images'], performance_acceptance=False, scope=pin['scope'])
    write(output / 'preparation.json', report)
    return report


def runtime_libraries(image, destination, runner, timeout):
    result = runner([shutil.which('ldd') or '/usr/bin/ldd', str(image)],
                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=timeout)
    text = result.stdout.decode('utf-8', errors='replace')
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(text)
    if result.returncode or 'not found' in text:
        raise ValueError('retained image has unavailable runtime libraries')
    import re
    paths = sorted(set(re.findall(r'(?:=>\s+|^\s*)(/\S+)\s+\(', text, re.M)))
    if not paths:
        raise ValueError('no actual runtime-library paths were decoded')
    return [dict(path=p, realpath=str(Path(p).resolve()), sha256=digest(p)) for p in paths]


def collect(prepared, output, scripts, perf, timeout=90, total=1800, runner=subprocess.run):
    """Use existing bounded diagnostic children; never calibrate or time pairs."""
    if platform.system() != 'Linux' or platform.machine().lower() not in ('x86_64', 'amd64'):
        raise ValueError('native sampling requires a Linux x86-64 host')
    deadline = time.monotonic() + total
    manifest = dict(status='incomplete', host=dict(system=platform.system(), release=platform.release(),
                    machine=platform.machine(), python=platform.python_version()), commands=[],
                    performance_acceptance=False, limits=[
                        'Different profiling host from the original timing host; no elapsed or causal acceptance',
                        'Userspace leaf samples, not callgraphs or hardware branch/cache counters',
                        'Program repetition includes its parsing/definitions; startup and declared argv inputs occur once',
                        'Original held-out labels are historical; these exposed diagnostic cases are not untouched future holdouts',
                        'Historical medians only estimate repetitions; RSS is a separate cold observation'])
    try:
        for role in ('base', 'candidate'):
            image = output / 'images' / role / 'bc'
            if digest(image) != prepared['images'][role]['sha256']:
                raise ValueError('retained ordinary image changed before sampling')
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise ValueError('shared diagnostic deadline exceeded')
            libraries = runtime_libraries(image, output / 'runtime' / (role + '-ldd.txt'),
                                          runner, min(60, remaining))
            manifest.setdefault('runtime_libraries', {})[role] = libraries
        cpuinfo = Path('/proc/cpuinfo')
        if cpuinfo.is_file():
            first = cpuinfo.read_text().split('\n\n')[0]
            allowed = {'vendor_id', 'cpu family', 'model', 'model name', 'stepping',
                       'microcode', 'cache size', 'cpu cores', 'siblings', 'flags'}
            manifest['host']['cpu'] = {a.strip(): b.strip() for a, b in
                                       (line.split(':', 1) for line in first.splitlines() if ':' in line)
                                       if a.strip() in allowed}
        manifest['host']['logical_cpus'] = os.cpu_count()
        order = []
        for preset in sorted(prepared['replay']):
            for name in prepared['replay'][preset]['workloads']:
                roles = ['base', 'candidate']
                random.Random('200-frozen-cpu-' + preset + name).shuffle(roles)
                order.extend((preset, name, role) for role in roles)
        for index, (preset, name, role) in enumerate(order):
            for library in manifest['runtime_libraries'][role]:
                path = Path(library['path'])
                if str(path.resolve()) != library['realpath'] or digest(path) != library['sha256']:
                    raise ValueError('runtime library changed before diagnostic')
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise ValueError('shared diagnostic deadline exceeded')
            dest = output / 'diagnostics' / preset / name / role
            folder = Path(prepared['replay'][preset]['directory'])
            argv = [sys.executable, str(scripts / 'profile-bc.py'), '--config', prepared['configs'][role],
                    '--implementation', role, '--summary', str(folder / 'summary.json'),
                    '--replay-corpus', str(folder), '--workload', name, '--output', str(dest),
                    '--perf', str(perf), '--timeout', str(min(timeout, remaining))]
            log = output / 'logs' / ('{:02d}-{}-{}.log'.format(index, name, role))
            log.parent.mkdir(exist_ok=True)
            command = dict(argv=argv, returncode=None, log=str(log.relative_to(output)),
                           preset=preset, name=name, role=role)
            manifest['commands'].append(command)
            write(output / 'collection.json', manifest)
            with log.open('w') as stream:
                result = runner(argv, stdout=stream, stderr=subprocess.STDOUT, timeout=remaining)
            command['returncode'] = result.returncode
            write(output / 'collection.json', manifest)
            if result.returncode:
                raise ValueError('checked diagnostic failed; retain partial results: ' + name + '/' + role)
            data = json.loads((dest / 'manifest.json').read_text())
            tool = data['implementation']
            if (tool['binary_sha256'] != prepared['images'][role]['sha256'] or
                    tool['metadata']['source_revision'] != prepared['images'][role]['source_revision'] or
                    tool['metadata']['instrumented'] is not False or
                    data['repeat_estimate_summary_sha256'] != prepared['replay'][preset]['summary_sha256'] or
                    data['replay']['source_manifest_sha256'] !=
                    prepared['replay'][preset]['evidence']['source_manifest_sha256']):
                raise ValueError('diagnostic image, source or replay identity differs from preparation')
            rows = data['workloads']
            if len(rows) != 1 or rows[0]['name'] != name or not rows[0]['checked']:
                raise ValueError('diagnostic did not check the selected output')
            cpu = rows[0]['cpu']
            trace = (dest / (name + '.perf.txt')).read_text()
            if not cpu.get('checked') or cpu.get('event') != 'cpu-clock:u' or 'cpu-clock:u:' not in trace or '/bc)' not in trace:
                raise ValueError('requested userspace trace lacks checked interpreter samples')
        manifest['status'] = 'all declared diagnostic children checked; no performance acceptance'
    except (OSError, ValueError, KeyError, subprocess.TimeoutExpired) as error:
        manifest['failure'] = str(error)
        raise
    finally:
        write(output / 'collection.json', manifest)
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--archive', type=Path, required=True)
    parser.add_argument('--artifact-metadata', type=Path, required=True)
    parser.add_argument('--scripts', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--perf', type=Path)
    parser.add_argument('--prepare-only', action='store_true')
    args = parser.parse_args()
    pin = json.loads((HERE / 'provenance.json').read_text())
    try:
        prepared = prepare(args.archive.resolve(), json.loads(args.artifact_metadata.read_text()),
                           args.output.resolve(), args.scripts.resolve(), pin)
        if not args.prepare_only:
            if args.perf is None or not args.perf.is_file() or not os.access(str(args.perf), os.X_OK):
                raise ValueError('native collection requires an executable perf adapter')
            collect(prepared, args.output.resolve(), args.scripts.resolve(), args.perf.resolve())
    except (OSError, ValueError, KeyError, zipfile.BadZipFile, subprocess.TimeoutExpired) as error:
        print('Frozen diagnostics failed: ' + str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
