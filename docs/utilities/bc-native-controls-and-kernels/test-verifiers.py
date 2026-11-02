#!/usr/bin/env python3
"""Check that valid archived data passes and two material corruptions fail.
SPDX-License-Identifier: Apache-2.0
"""
import json
from pathlib import Path
import runpy
import shutil
import tempfile
import unittest
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


if __name__ == '__main__':
    unittest.main()
