#!/usr/bin/env python3
"""Check post-sampling executable identity and bounded native inspection."""
import hashlib
import copy
import importlib.util
import json
from pathlib import Path
import struct
import sys
import tempfile
import types
import unittest
from unittest import mock

SPEC = importlib.util.spec_from_file_location('bc_codegen', Path(__file__).with_name('scripts') / 'collect-bc-codegen.py')
CG = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CG)


def image(code=b'\x90' * 16, data=b'A' * 16, timestamp=1):
    result = bytearray(1024)
    result[:2] = b'MZ'
    struct.pack_into('<I', result, 60, 128)
    result[128:132] = b'PE\0\0'
    struct.pack_into('<HHIIIHH', result, 132, 0x8664, 2, timestamp, 0, 0, 240, 2)
    struct.pack_into('<H', result, 152, 0x20b)
    for index, (name, offset, payload, flags) in enumerate(((b'.text', 512, code, 0x60000020), (b'.data', 768, data, 0xc0000040))):
        header = 392 + 40 * index
        result[header:header + 8] = name.ljust(8, b'\0')
        struct.pack_into('<IIII', result, header + 8, len(payload), 4096 * (index + 1), len(payload), offset)
        struct.pack_into('<I', result, header + 36, flags)
        result[offset:offset + len(payload)] = payload
    return bytes(result)


class CodegenEvidence(unittest.TestCase):
    def test_pe_code_identity_distinguishes_code_from_timestamp_and_data(self):
        first = image()
        changed_data = image(data=b'B' * 16, timestamp=456)
        changed_code = image(code=b'\xcc' * 16)
        left, right = CG.pe_code(first), CG.pe_code(changed_data)
        self.assertEqual(left['status'], 'collected')
        self.assertNotEqual(hashlib.sha256(first).hexdigest(), hashlib.sha256(changed_data).hexdigest())
        self.assertEqual(left['code_payload_sha256'], right['code_payload_sha256'])
        self.assertNotEqual(left['coff_timestamp_field'], right['coff_timestamp_field'])
        self.assertNotEqual(left['code_payload_sha256'], CG.pe_code(changed_code)['code_payload_sha256'])
        self.assertEqual(left['code_file_bytes'], 16)
        self.assertEqual(left['sections'][0]['rva'], 4096)
        self.assertEqual(left['sections'][0]['sha256'], hashlib.sha256(b'\x90' * 16).hexdigest())

    def test_malformed_pe_never_yields_a_partial_code_identity(self):
        invalid = [b'', b'MZ', image()[:400]]
        for header_offset, value in ((60, 0xffffffff), (392 + 16, 0xffffffff), (392 + 20, 0xffffffff)):
            payload = bytearray(image())
            struct.pack_into('<I', payload, header_offset, value)
            invalid.append(bytes(payload))
        for payload in invalid:
            with self.subTest(size=len(payload)):
                result = CG.pe_code(payload)
                self.assertEqual(result['status'], 'unavailable')
                self.assertNotIn('code_payload_sha256', result)

    def entry(self, path, name='candidate'):
        return dict(name=name, argv=[str(path)], metadata=dict(instrumented=False,
                    binary_sha256=CG.digest(path), source_revision='fixture'))

    def test_generic_fallback_preserves_exact_binary_without_running_it(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'not executable'
            source.write_bytes(b'fixture: must never be executed')
            with mock.patch.object(CG, 'native_commands', return_value=[]):
                report = CG.collect([self.entry(source)], root / 'evidence')
            row = report['observations'][0]
            self.assertEqual((root / 'evidence' / row['binary']).read_bytes(), source.read_bytes())
            self.assertEqual(row['binary_sha256'], CG.digest(source))
            self.assertEqual(row['disassembly']['status'], 'unavailable')
            self.assertEqual(row['pe_code']['status'], 'unavailable')
            self.assertEqual(row['file_bytes'], source.stat().st_size)

    def test_pre_sampling_snapshot_preserves_inputs_without_native_inspection(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source, library = root / 'binary', root / 'library.bc'
            source.write_bytes(image())
            library.write_bytes(b'scale=20\n')
            entry = self.entry(source)
            entry['input_artifacts'] = [dict(path=str(library), sha256=CG.digest(library))]
            with mock.patch.object(CG, 'native_commands') as inspector:
                report = CG.collect([entry], root / 'snapshot', inspect=False)
            inspector.assert_not_called()
            self.assertEqual(report['status'], 'preserved')
            self.assertFalse(report['inspection_requested'])
            row = report['observations'][0]
            self.assertEqual((root / 'snapshot' / row['binary']).read_bytes(), source.read_bytes())
            copied = row['input_artifacts'][0]
            self.assertEqual((root / 'snapshot' / copied['archive_path']).read_bytes(), library.read_bytes())

    def test_versioned_comparators_preserve_both_exact_executables(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            entries = []
            for index, name in enumerate(('howard-7.0.3', 'gnu-1.08.2')):
                source = root / ('fixture-' + str(index))
                source.write_bytes(image(timestamp=index + 1))
                entries.append(self.entry(source, name))
            with mock.patch.object(CG, 'native_commands', return_value=[]):
                report = CG.collect(entries, root / 'evidence')
            self.assertEqual(report['status'], 'preserved')
            self.assertEqual([r['name'] for r in report['observations']], [e['name'] for e in entries])
            for entry, row in zip(entries, report['observations']):
                self.assertEqual(row['status'], 'preserved')
                self.assertEqual((root / 'evidence' / row['binary']).read_bytes(), Path(entry['argv'][0]).read_bytes())
                self.assertEqual(row['binary_sha256'], entry['metadata']['binary_sha256'])
                self.assertEqual(row['disassembly']['status'], 'unavailable')

    def test_recipes_and_declared_external_inputs_are_preserved_exactly(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source, library = root / 'binary', root / 'library.bc'
            source.write_bytes(image())
            library.write_bytes(b'scale=20\n')
            entry = self.entry(source)
            entry['argv'].append(str(library))
            entry['input_artifacts'] = [dict(path=str(library), sha256=CG.digest(library),
                                             purpose='ordinary library')]
            original = copy.deepcopy(entry)
            with mock.patch.object(CG, 'native_commands', return_value=[]):
                report = CG.collect([entry], root / 'evidence')
            row = report['observations'][0]
            self.assertEqual(entry, original)
            self.assertEqual(row['implementation'], original)
            self.assertEqual(len(row['input_artifacts']), 1)
            retained = row['input_artifacts'][0]
            self.assertEqual((root / 'evidence' / retained['archive_path']).read_bytes(), library.read_bytes())
            self.assertEqual(retained['sha256'], CG.digest(library))

    def test_all_assets_are_copied_before_native_inspection(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            entries = []
            for name in ('first', 'second'):
                source = root / name
                source.write_bytes(image())
                entries.append(self.entry(source, name))
            def inspect(binary, system):
                self.assertTrue((root / 'evidence/second/binary').is_file())
                return []
            with mock.patch.object(CG, 'native_commands', side_effect=inspect):
                self.assertEqual(CG.collect(entries, root / 'evidence')['status'], 'preserved')

    def test_expired_collection_deadline_keeps_assets_without_starting_tools(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'binary'
            source.write_bytes(image())
            with mock.patch.object(CG, 'native_commands') as native:
                report = CG.collect([self.entry(source)], root / 'evidence', deadline=0)
            native.assert_not_called()
            self.assertEqual(report['status'], 'preserved')
            row = report['observations'][0]
            self.assertEqual((root / 'evidence' / row['binary']).read_bytes(), source.read_bytes())
            self.assertEqual(row['disassembly']['status'], 'unavailable')

    def test_missing_or_changed_declared_inputs_are_hard_errors(self):
        for mode in ('missing', 'changed', 'copy'):
            with self.subTest(mode=mode), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                source, library = root / 'binary', root / 'library.bc'
                source.write_bytes(image())
                library.write_bytes(b'scale=20\n')
                entry = self.entry(source)
                entry['input_artifacts'] = [dict(path=str(library), sha256=CG.digest(library))]
                if mode == 'missing': library.unlink()
                elif mode == 'changed': library.write_bytes(b'changed')
                original_copy = CG.shutil.copyfile
                def copy_file(src, dst):
                    original_copy(src, dst)
                    if mode == 'copy' and Path(src) == library:
                        Path(dst).write_bytes(b'changed copy')
                with mock.patch.object(CG.shutil, 'copyfile', side_effect=copy_file), \
                        self.assertRaises(CG.CodegenError):
                    CG.collect([entry], root / 'evidence')
                self.assertEqual(json.loads((root / 'evidence/manifest.json').read_text())['status'], 'error')

    def test_deadline_expiry_between_inspection_commands_stops_further_tools(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'binary'
            source.write_bytes(image())
            marker = root / 'second-command-ran'
            commands = [[sys.executable, '-c', 'print("first fixture")'],
                        [sys.executable, '-c', 'from pathlib import Path;import sys;Path(sys.argv[1]).touch()',
                         str(marker)]]
            clock = types.SimpleNamespace(monotonic=mock.Mock(side_effect=[0, 1, 300]))
            with mock.patch.object(CG, 'time', clock), \
                    mock.patch.object(CG, 'native_commands', return_value=commands):
                report = CG.collect([self.entry(source)], root / 'evidence', deadline=300)
            self.assertFalse(marker.exists())
            self.assertEqual(report['status'], 'preserved')
            native = report['observations'][0]['disassembly']
            self.assertEqual(native['status'], 'unavailable')
            self.assertEqual(len(native['commands']), 1)
            self.assertIn('deadline', native['reason'])

    def test_inspection_cannot_change_original_or_copied_external_inputs(self):
        for archived in (False, True):
            with self.subTest(archived=archived), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                source, library = root / 'binary', root / 'library.bc'
                source.write_bytes(image())
                library.write_bytes(b'scale=20\n')
                entry = self.entry(source)
                entry['input_artifacts'] = [dict(path=str(library), sha256=CG.digest(library))]
                changed = root / 'evidence/candidate/input-0' if archived else library
                commands = [[sys.executable, '-c',
                             'from pathlib import Path;import sys;Path(sys.argv[1]).write_bytes(b"changed")',
                             str(changed)]]
                with mock.patch.object(CG, 'native_commands', return_value=commands), \
                        self.assertRaises(CG.CodegenError):
                    CG.collect([entry], root / 'evidence')
                self.assertEqual(json.loads((root / 'evidence/manifest.json').read_text())['status'], 'error')

    def test_duplicate_versioned_role_cannot_overwrite_its_first_binary(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'fixture'
            source.write_bytes(image())
            entry = self.entry(source, 'howard-7.0.3')
            with mock.patch.object(CG, 'native_commands', return_value=[]), self.assertRaises(CG.CodegenError):
                CG.collect([entry, entry], root / 'evidence')
            report = json.loads((root / 'evidence/manifest.json').read_text())
            self.assertEqual(report['status'], 'error')
            self.assertEqual(len(report['observations']), 1)
            self.assertEqual((root / 'evidence' / report['observations'][0]['binary']).read_bytes(), source.read_bytes())

    def test_missing_changed_or_instrumented_binaries_are_hard_errors(self):
        for mode in ('missing', 'changed', 'instrumented'):
            with self.subTest(mode=mode), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                source = root / 'binary'
                source.write_bytes(image())
                entry = self.entry(source)
                if mode == 'missing':
                    source.unlink()
                elif mode == 'changed':
                    source.write_bytes(b'changed')
                else:
                    entry['metadata']['instrumented'] = True
                with self.assertRaises(CG.CodegenError):
                    CG.collect([entry], root / 'evidence')
                report = json.loads((root / 'evidence/manifest.json').read_text())
                self.assertEqual(report['status'], 'error')

    def test_invalid_role_names_cannot_escape_archive(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'binary'
            source.write_bytes(image())
            for index, name in enumerate(('../outside', '/absolute', 'a/b', '.', '..',
                                          '.candidate', 'candidate.', 'candidate..other')):
                with self.subTest(name=name), self.assertRaises(CG.CodegenError):
                    CG.collect([self.entry(source, name)], root / ('evidence' + str(index)))
            self.assertFalse((root / 'outside').exists())

    def test_native_output_is_hashed_and_binary_remains_unchanged(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'fixture.exe'
            source.write_bytes(image())
            commands = [[sys.executable, '-c', 'print("known disassembly fixture")']]
            with mock.patch.object(CG, 'native_commands', return_value=commands):
                report = CG.collect([self.entry(source)], root / 'evidence')
            row = report['observations'][0]
            self.assertEqual(row['disassembly']['status'], 'collected')
            command = row['disassembly']['commands'][0]
            log = root / 'evidence/candidate' / command['log']
            self.assertIn('known disassembly fixture', log.read_text())
            self.assertEqual(command['log_sha256'], CG.digest(log))
            self.assertEqual(CG.digest(source), row['binary_sha256'])

    def test_timed_out_and_failing_tools_are_explicitly_unavailable(self):
        for command in ([sys.executable, '-c', 'import time;time.sleep(10)'], [sys.executable, '-c', 'raise SystemExit(7)']):
            with self.subTest(command=command), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                source = root / 'binary'
                source.write_bytes(image())
                with mock.patch.object(CG, 'native_commands', return_value=[command]):
                    report = CG.collect([self.entry(source)], root / 'evidence', timeout=.2)
                native = report['observations'][0]['disassembly']
                self.assertEqual(native['status'], 'unavailable')
                self.assertTrue(native['reason'])
                self.assertEqual(CG.digest(source), report['observations'][0]['binary_sha256'])

    def test_tool_mutating_archive_is_detected(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'binary'
            source.write_bytes(image())
            def mutate(binary, system):
                return [[sys.executable, '-c', 'from pathlib import Path;import sys;Path(sys.argv[1]).write_bytes(b"changed")', str(binary)]]
            with mock.patch.object(CG, 'native_commands', side_effect=mutate), self.assertRaises(CG.CodegenError):
                CG.collect([self.entry(source)], root / 'evidence')
            self.assertEqual(json.loads((root / 'evidence/manifest.json').read_text())['status'], 'error')

    def test_native_tool_selection_has_portable_fallback(self):
        with mock.patch.object(CG.shutil, 'which', side_effect=lambda name: '/tools/' + name):
            self.assertEqual(CG.native_commands(Path('/binary'), 'Windows')[1][1], '/disasm')
            self.assertEqual(CG.native_commands(Path('/binary'), 'Darwin')[1][1], '-tvV')
            self.assertEqual(CG.native_commands(Path('/binary'), 'Linux')[1][1:3], ['-d', '-C'])
            self.assertEqual(CG.native_commands(Path('/binary'), 'unknown'), [])
        with mock.patch.object(CG.shutil, 'which', return_value=None):
            for system in ('Windows', 'Darwin', 'Linux'):
                self.assertEqual(CG.native_commands(Path('/binary'), system), [])

    def test_invalid_deadlines_are_rejected_before_creating_output(self):
        with tempfile.TemporaryDirectory() as temporary:
            target = Path(temporary) / 'evidence'
            for timeout in (0, -1, float('nan'), float('inf')):
                with self.subTest(timeout=timeout), self.assertRaises(ValueError):
                    CG.collect([], target, timeout=timeout)
                self.assertFalse(target.exists())

    def test_real_native_inspector_can_read_this_host_executable(self):
        if not CG.native_commands(Path(sys.executable), CG.platform.system()):
            self.skipTest('supported native inspection tool unavailable')
        with tempfile.TemporaryDirectory() as temporary:
            report = CG.collect([self.entry(Path(sys.executable))], Path(temporary) / 'evidence')
            row = report['observations'][0]
            self.assertEqual(row['disassembly']['status'], 'collected', row['disassembly'])
            self.assertEqual(row['binary_sha256'], CG.digest(sys.executable))


if __name__ == '__main__':
    unittest.main()
