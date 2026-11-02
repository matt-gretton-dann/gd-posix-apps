#!/usr/bin/env python3
"""Check retained evidence integrity and rejection of material corruptions.
SPDX-License-Identifier: Apache-2.0
"""
import json
import contextlib
import io
from pathlib import Path
import runpy
import shutil
import tempfile
import unittest
from unittest import mock
import zipfile

ROOT = Path(__file__).resolve().parent
VERIFY = runpy.run_path(str(ROOT / 'verify-further-local.py'))['verify']


def replace_member(path, name, change):
    replacement = path.with_suffix('.replacement.zip')
    with zipfile.ZipFile(path) as original, zipfile.ZipFile(
            replacement, 'w', compression=zipfile.ZIP_DEFLATED) as output:
        for member in original.namelist():
            data = original.read(member)
            output.writestr(member, change(data) if member == name else data)
    replacement.replace(path)


class IntegrityTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        shutil.copytree(ROOT / 'further-local', self.root / 'further-local')

    def test_windows_report_preserves_recorded_bytes(self):
        # Model Windows text newline translation on every host. The report is
        # hashed evidence, so replay must not depend on the writer's host OS.
        shutil.copytree(ROOT / 'sources', self.root / 'sources')
        script = self.root / 'verify-windows-controls.py'
        shutil.copyfile(ROOT / script.name, script)

        def windows_text(path, data, encoding=None, errors=None, **kwargs):
            with path.open('w', encoding=encoding, errors=errors,
                           newline=kwargs.get('newline', '\r\n')) as output:
                return output.write(data)

        with mock.patch.object(Path, 'write_text', windows_text):
            with contextlib.redirect_stdout(io.StringIO()):
                runpy.run_path(str(script))
        self.assertEqual((self.root / 'windows-controls-audit.json').read_bytes(),
                         (ROOT / 'windows-controls-audit.json').read_bytes())

    def test_checked_sample_omission_is_rejected(self):
        archive = self.root / 'further-local/typed-paired.zip'
        with zipfile.ZipFile(archive) as z:
            name = next(n for n in z.namelist() if n.endswith('/samples.jsonl'))
        replace_member(archive, name, lambda data: b'\n'.join(data.splitlines()[1:]) + b'\n')
        with self.assertRaises(AssertionError):
            VERIFY(self.root)

    def test_changed_timing_executable_is_rejected(self):
        archive = self.root / 'further-local/typed-codegen.zip'
        with zipfile.ZipFile(archive) as z:
            manifest = json.loads(z.read('manifest.json'))
            name = manifest['observations'][0]['binary']
        replace_member(archive, name, lambda data: bytes([data[0] ^ 1]) + data[1:])
        with self.assertRaises(AssertionError):
            VERIFY(self.root)


class RootStorageIntegrityTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        shutil.copytree(ROOT / 'root-storage', self.root / 'root-storage')
        native = self.root / 'native/root-535-405'
        native.mkdir(parents=True)
        shutil.copyfile(ROOT / 'native/root-535-405/macos.zip', native / 'macos.zip')
        self.verify = runpy.run_path(str(ROOT / 'verify-root-storage.py'))['verify']

    def test_original_failed_attempt_and_successful_retry_are_retained(self):
        result = self.verify(self.root)
        self.assertEqual((result['root_logical'], result['root_rss'],
                          result['numeric_logical']), (56, 126, 12))
        self.assertFalse(result['performance_acceptance'])

    def test_missing_logical_observation_is_rejected(self):
        def omit(data):
            value = json.loads(data)
            value['logical'].pop()
            return json.dumps(value).encode('utf-8')
        replace_member(self.root / 'root-storage/root-logical.zip', 'manifest.json', omit)
        with self.assertRaises(AssertionError):
            self.verify(self.root)

    def test_changed_resource_report_is_rejected(self):
        archive = self.root / 'root-storage/root-rss.zip'
        with zipfile.ZipFile(archive) as z:
            name = json.loads(z.read('manifest.json'))['samples'][0]['report']
        replace_member(archive, name, lambda data: data.replace(b' resident ', b' altered '))
        with self.assertRaises(AssertionError):
            self.verify(self.root)

    def test_changed_profile_executable_is_rejected(self):
        replace_member(self.root / 'root-storage/numeric-sparse-provenance.zip',
                       'candidate/binary', lambda data: bytes([data[0] ^ 1]) + data[1:])
        with self.assertRaises(AssertionError):
            self.verify(self.root)


if __name__ == '__main__':
    unittest.main()
