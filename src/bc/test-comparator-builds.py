#!/usr/bin/env python3
"""Reject invalid comparator builds before publishing usable provenance."""
import argparse
import hashlib
import importlib.util
import io
import json
import shlex
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import types
import unittest
from unittest import mock


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

    @unittest.skipIf(sys.platform == 'win32', 'POSIX configure recipe')
    def test_howard_target_and_host_use_an_explicit_compatible_dialect(self):
        calls = []
        module = self.module
        root = self.root

        class Source:
            def __init__(self):
                self.root = root

            def run(self, argv, cwd=None, env=None):
                calls.append((argv, env))
                if argv[:2] == ['git', 'rev-parse']:
                    return module.HOWARD_REVISION
                if argv[0] == 'make':
                    binary = root / 'howard-source/bin/bc'
                    binary.parent.mkdir(parents=True)
                    binary.write_bytes(b'checked build fixture')
                return 'bc 7.0.3' if argv[-1] == '--version' else ''

        args = types.SimpleNamespace(howard_repository='fixture', cc='cc', make='make', jobs=2)
        with mock.patch.object(module, 'compiler_metadata', return_value={'argv0': 'cc'}), \
             mock.patch.object(module, 'make_variables', return_value={'CFLAGS': 'recorded'}):
            module.build_howard(Source(), args)
        environments = [env for argv, env in calls if argv[0] in ('sh', 'make')]
        self.assertEqual(len(environments), 2)
        for env in environments:
            for name in ('CFLAGS', 'HOSTCFLAGS'):
                standards = [flag for flag in shlex.split(env[name]) if flag.startswith('-std=')]
                self.assertEqual(standards, ['-std=gnu17'], (name, env[name]))

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

    def test_gnu_download_retries_transient_failure(self):
        if sys.platform == 'win32':
            self.skipTest('GNU configure recipe is POSIX-only')
        payload = b'pinned test archive'

        class InterruptedDownload(io.BytesIO):
            def read(self, size=-1):
                if self.tell():
                    raise TimeoutError('transient download failure')
                return super().read(4)

        with mock.patch.object(self.module.urllib.request, 'urlopen',
                               side_effect=[InterruptedDownload(payload), io.BytesIO(payload)]) as download, \
             mock.patch.object(self.module, 'GNU_SHA256', hashlib.sha256(payload).hexdigest()), \
             mock.patch.object(self.module, 'safe_extract',
                               side_effect=self.module.BuildError('verified archive')) as extract:
            with self.assertRaisesRegex(self.module.BuildError, 'verified archive'):
                self.module.build_gnu(self.module.Builder(self.root, 3),
                                      types.SimpleNamespace(gnu_archive=None))
        self.assertEqual(download.call_count, 2)
        self.assertEqual(extract.call_args[0][0].read_bytes(), payload)

    def test_gnu_download_retry_limit(self):
        error = self.module.urllib.error.URLError('network unavailable')
        with mock.patch.object(self.module.urllib.request, 'urlopen', side_effect=error) as download, \
             mock.patch.object(self.module.time, 'sleep') as delay:
            with self.assertRaises(self.module.urllib.error.URLError):
                self.module.download_gnu_archive(self.root / 'archive')
        self.assertEqual(download.call_count, 3)
        self.assertEqual(delay.call_args_list, [mock.call(1), mock.call(2)])

    def test_archive_path_cannot_escape(self):
        archive = self.root / 'unsafe.tar.gz'
        with tarfile.open(str(archive), 'w:gz') as output:
            member = tarfile.TarInfo('../escape')
            member.size = 1
            output.addfile(member, io.BytesIO(b'x'))
        with self.assertRaises(self.module.BuildError):
            self.module.safe_extract(archive, self.root / 'extract')
        self.assertFalse((self.root / 'escape').exists())

    def test_native_toolset_discovery(self):
        root = self.root / 'MSBuild/Microsoft/VC/v180/Platforms/x64/PlatformToolsets'
        for toolset in ('v143', 'v145', 'ClangCL', 'v143_xp'):
            (root / toolset).mkdir(parents=True)
        self.assertEqual(self.module.msvc_toolset(None, str(self.root)), 'v145')
        self.assertEqual(self.module.msvc_toolset('v143', None), 'v143')
        with self.assertRaises(self.module.BuildError):
            self.module.msvc_toolset(None, None)

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
