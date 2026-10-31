#!/usr/bin/env python3
"""Reject invalid comparator builds before publishing usable provenance."""
import argparse
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import types
import unittest


class ComparatorBuilds(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        spec = importlib.util.spec_from_file_location('comparator_builder', BUILDER)
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)

    def test_failed_child_keeps_log(self):
        builder = self.module.Builder(self.root, 3)
        with self.assertRaises(self.module.BuildError):
            builder.run([sys.executable, '-c', "print('build failed'); raise SystemExit(23)"])
        self.assertEqual(builder.commands[0]['returncode'], 23)
        self.assertIn('build failed', (self.root / 'command-001.log').read_text())

    def test_timeout_keeps_partial_log(self):
        builder = self.module.Builder(self.root, 0.1)
        with self.assertRaises(self.module.BuildError):
            builder.run([sys.executable, '-c', 'import time; time.sleep(5)'])
        self.assertEqual(builder.commands[0]['status'], 'timeout')
        self.assertTrue((self.root / 'command-001.log').is_file())

    def test_argv_boundaries(self):
        builder = self.module.Builder(self.root, 3)
        argument = "directory with spaces and 'quote"
        result = builder.run([sys.executable, '-c', 'import sys; print(sys.argv[1])', argument])
        self.assertEqual(result, argument)

    def test_wrong_howard_revision(self):
        class WrongSource:
            root = self.root
            def run(self, argv, *args, **kwargs):
                return 'wrong-commit' if argv[1] == 'rev-parse' else ''
        with self.assertRaisesRegex(self.module.BuildError, 'pinned commit'):
            self.module.build_howard(WrongSource(), types.SimpleNamespace(howard_repository='cache'))

    def test_bad_gnu_archive_never_publishes_implementation(self):
        if sys.platform == 'win32':
            self.skipTest('GNU configure recipe is POSIX-only')
        archive = self.root / 'bad.tar.gz'
        archive.write_bytes(b'not the pinned archive')
        output = self.root / 'result'
        process = subprocess.run([sys.executable, BUILDER, '--output', str(output),
                                  '--implementation', 'gnu', '--gnu-archive', str(archive)],
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 universal_newlines=True, timeout=5)
        self.assertEqual(process.returncode, 1)
        manifest = json.loads((output / 'build-manifest.json').read_text())
        self.assertIn('checksum', manifest['failure'])
        self.assertFalse((output / 'implementations.json').exists())

    def test_archive_path_cannot_escape(self):
        archive = self.root / 'unsafe.tar.gz'
        with tarfile.open(str(archive), 'w:gz') as output:
            member = tarfile.TarInfo('../escape')
            member.size = 1
            output.addfile(member, io.BytesIO(b'x'))
        with self.assertRaises(self.module.BuildError):
            self.module.safe_extract(archive, self.root / 'extract')
        self.assertFalse((self.root / 'escape').exists())

    def test_existing_output_is_preserved(self):
        output = self.root / 'existing'
        output.mkdir()
        marker = output / 'marker'
        marker.write_text('preserve')
        process = subprocess.run([sys.executable, BUILDER, '--output', str(output)],
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=5)
        self.assertEqual(process.returncode, 2)
        self.assertEqual(marker.read_text(), 'preserve')
        self.assertEqual(list(output.iterdir()), [marker])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('builder')
    args, remaining = parser.parse_known_args()
    BUILDER = str(Path(args.builder).resolve())
    unittest.main(argv=[__file__, *remaining])
