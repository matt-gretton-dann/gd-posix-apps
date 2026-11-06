#!/usr/bin/env python3
"""Preserve actual timing executables and inspect code after sampling finishes.

No compiler/linker flags change. PE code identities exclude non-code sections
and the COFF timestamp field, but include encoded addresses and raw padding.
Identical code bytes alone do not establish identical runtime behavior.
SPDX-License-Identifier: Apache-2.0
"""
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import platform
import re
import shutil
import struct


class CodegenError(RuntimeError):
    pass


def digest(path):
    result = hashlib.sha256()
    with Path(path).open('rb') as source:
        for block in iter(lambda: source.read(1024 * 1024), b''):
            result.update(block)
    return result.hexdigest()


def pe_code(data):
    """Read bounded PE section payloads, never load or execute the image.

    Layout: https://learn.microsoft.com/windows/win32/debug/pe-format
    Other formats retain the full-file identity and native-tool output.
    """
    unavailable = dict(status='unavailable', reason='not a supported PE image')
    if not data.startswith(b'MZ'):
        return unavailable
    try:
        if len(data) < 64:
            raise ValueError('truncated DOS header')
        offset = struct.unpack_from('<I', data, 60)[0]
        if offset > len(data) - 24 or data[offset:offset + 4] != b'PE\0\0':
            raise ValueError('missing PE signature/header')
        machine, count, timestamp, _, _, optional_size, _ = struct.unpack_from('<HHIIIHH', data, offset + 4)
        optional = offset + 24
        table = optional + optional_size
        if optional_size < 2 or table > len(data) or count == 0 or count > (len(data) - table) // 40:
            raise ValueError('truncated optional header/section table')
        magic = struct.unpack_from('<H', data, optional)[0]
        if magic not in (0x10b, 0x20b):
            raise ValueError('unsupported optional-header magic')
        sections = []
        combined = hashlib.sha256()
        for index in range(count):
            header = table + 40 * index
            name = data[header:header + 8].rstrip(b'\0').decode('ascii', errors='backslashreplace')
            virtual_size, rva, raw_size, raw_offset = struct.unpack_from('<IIII', data, header + 8)
            flags = struct.unpack_from('<I', data, header + 36)[0]
            if raw_size and (raw_offset < table + count * 40 or raw_offset > len(data) or raw_size > len(data) - raw_offset):
                raise ValueError('section payload is outside the image')
            if not flags & (0x20 | 0x20000000):
                continue
            payload = data[raw_offset:raw_offset + raw_size] if raw_size else b''
            combined.update(struct.pack('<Q', len(payload)))
            combined.update(payload)
            sections.append(dict(name=name, rva=rva, virtual_size=virtual_size,
                                 raw_size=raw_size, raw_offset=raw_offset, characteristics=flags,
                                 sha256=hashlib.sha256(payload).hexdigest()))
        if not sections:
            raise ValueError('no code/executable sections')
        return dict(status='collected', format='PE', machine=machine,
                    coff_timestamp_field=timestamp, sections=sections,
                    code_file_bytes=sum(s['raw_size'] for s in sections),
                    code_virtual_bytes=sum(s['virtual_size'] for s in sections),
                    code_payload_sha256=combined.hexdigest(),
                    scope='ordered length-framed raw code/executable sections, including file padding; no address/relocation normalization; non-code data and runtime placement excluded')
    except (ValueError, struct.error) as error:
        return dict(status='unavailable', reason='invalid/unsupported PE: ' + str(error))


def native_commands(binary, system):
    if system == 'Windows':
        tool = shutil.which('dumpbin')
        arguments = [('/headers',), ('/disasm',)]
    elif system == 'Darwin':
        tool = shutil.which('otool')
        arguments = [('-l',), ('-tvV',)]
    elif system == 'Linux':
        tool = shutil.which('llvm-objdump') or shutil.which('objdump')
        arguments = [('-h',), ('-d', '-C')]
    else:
        return []
    return [[tool, *args, str(binary)] for args in arguments] if tool else []


def collect(entries, directory, timeout=60):
    """Archive declared, uninstrumented binaries; unavailable tools are explicit.

    Call only after every elapsed child has finished. Hash mismatch/missing
    declared binaries are hard errors; diagnostics cannot legitimize a changed
    executable. Native tools inspect the copy and never run the bc program.
    """
    if not math.isfinite(timeout) or timeout <= 0:
        raise ValueError("inspection timeout must be finite and positive")
    directory = Path(directory)
    directory.mkdir(exist_ok=False)
    report = dict(schema=1, purpose='post-sampling executable/code-generation evidence; not timing or performance acceptance',
                  collector_sha256=digest(__file__), system=platform.system(), observations=[])
    names = set()
    failed = False
    try:
        for entry in entries:
            name = entry['name']
            if not re.fullmatch(r'[A-Za-z0-9_-]+', name) or name in names:
                raise CodegenError('invalid/duplicate role name')
            names.add(name)
            row = dict(name=name, source_revision=entry['metadata'].get('source_revision'),
                       implementation=entry, input_artifacts=[], status='incomplete')
            report['observations'].append(row)
            target = directory / name
            target.mkdir()
            source = Path(entry['argv'][0])
            expected = entry['metadata'].get('binary_sha256')
            if entry['metadata'].get('instrumented') is not False or not isinstance(expected, str) or not re.fullmatch('[0-9a-f]{64}', expected):
                raise CodegenError('role lacks an uninstrumented executable identity')
            try:
                if digest(source) != expected:
                    raise CodegenError('declared executable changed before archival: ' + name)
                binary = target / ('binary.exe' if source.suffix.lower() == '.exe' else 'binary')
                shutil.copyfile(str(source), str(binary))
                if digest(binary) != expected:
                    raise CodegenError('archived executable identity mismatch: ' + name)
            except OSError as error:
                raise CodegenError('cannot preserve declared executable ' + name + ': ' + str(error))
            for index, artifact in enumerate(entry.get('input_artifacts', [])):
                identity = artifact.get('sha256')
                if not isinstance(identity, str) or not re.fullmatch('[0-9a-f]{64}', identity):
                    raise CodegenError('input artifact lacks a SHA-256: ' + name)
                original = Path(artifact['path'])
                copied = target / ('input-' + str(index))
                if digest(original) != identity:
                    raise CodegenError('declared input changed before archival: ' + name)
                shutil.copyfile(str(original), str(copied))
                if digest(copied) != identity:
                    raise CodegenError('archived input identity mismatch: ' + name)
                row['input_artifacts'].append(dict(artifact,
                    archive_path=str(copied.relative_to(directory))))
            row.update(status='preserved', binary=str(binary.relative_to(directory)),
                       binary_sha256=expected, file_bytes=binary.stat().st_size,
                       pe_code=pe_code(binary.read_bytes()))
            commands = native_commands(binary.resolve(), report['system'])
            row['disassembly'] = dict(status='unavailable', reason='no supported native inspection tool')
            if commands:
                # Reuse the native harness's bounded process-tree cleanup.
                spec = importlib.util.spec_from_file_location('bc_codegen_commands', Path(__file__).with_name('benchmark-math-ci.py'))
                module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(module)
                runner = module.Commands(target, 2 * timeout + 5)
                native = dict(status='incomplete', tool=commands[0][0], tool_sha256=digest(commands[0][0]), commands=runner.commands)
                row['disassembly'] = native
                try:
                    for command in commands:
                        runner.run(command, timeout=timeout)
                    native['status'] = 'collected'
                except (OSError, module.CommandFailure) as error:
                    native.update(status='unavailable', reason=str(error))
                for command in runner.commands:
                    log = target / command['log']
                    if log.is_file():
                        command['log_sha256'] = digest(log)
                        command['log_bytes'] = log.stat().st_size
            for artifact in row['input_artifacts']:
                if (digest(artifact['path']) != artifact['sha256'] or
                        digest(directory / artifact['archive_path']) != artifact['sha256']):
                    raise CodegenError('input changed during inspection: ' + name)
            if digest(source) != expected or digest(binary) != expected:
                raise CodegenError('executable changed during inspection: ' + name)
        report['status'] = 'preserved' if entries else 'unavailable: no completed build roles'
    except (CodegenError, KeyError, TypeError, OSError) as error:
        report.update(status='error', error=str(error))
        failed = True
    finally:
        (directory / 'manifest.json').write_text(json.dumps(report, indent=2, sort_keys=True) + '\n', encoding='utf-8')
    if failed:
        raise CodegenError(report['error'])
    return report
