#!/usr/bin/env python3
"""Check immutable source requests and reject misleading GD build provenance."""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class GdBuild(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        spec = importlib.util.spec_from_file_location('gd_builder', BUILDER)
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        self.helper = self.module.helpers()
        self.build = self.root / 'build'
        (self.build / 'src/bc').mkdir(parents=True)
        self.cache = self.build / 'CMakeCache.txt'
        self.cache.write_text('ENABLE_BC_PROFILE:BOOL=OFF\nENABLE_BC_COVERAGE:BOOL=OFF\n'
                              'CMAKE_BUILD_TYPE:STRING=Release\nCMAKE_CXX_FLAGS:STRING=\n'
                              'CMAKE_CXX_FLAGS_RELEASE:STRING=-O3 -DNDEBUG\n')
        self.header = self.build / 'src/bc/find-multiply-split-point.hh'
        self.header.write_text('// explicit BC_MULTIPLY_SPLIT_POINT\n'
                               '#define BC_MULTIPLY_SPLIT_POINT 126\n')
        (self.build / 'compile_commands.json').write_text(json.dumps([
            {'command': 'c++ -O3 -DNDEBUG -std=c++20 -c source.cc'}]))

    def metadata(self):
        return self.module.build_metadata(self.helper, self.root, self.root / 'source',
                                           self.build, 'a' * 40, 126,
                                           {'version': 'C'}, {'version': 'C++'})

    def test_records_compiled_threshold_and_commands(self):
        value = self.metadata()
        self.assertEqual(value['configuration']['multiply_split_point'], 126)
        self.assertEqual(value['configuration']['threshold_header_sha256'],
                         hashlib.sha256(self.header.read_bytes()).hexdigest())
        self.assertEqual((self.root / 'compile_commands.json').read_bytes(),
                         (self.build / 'compile_commands.json').read_bytes())
        self.assertFalse(value['instrumented'])

    def test_instrumented_configuration_is_rejected(self):
        for variable in ('ENABLE_BC_PROFILE', 'ENABLE_BC_COVERAGE'):
            previous = self.cache.read_text()
            self.cache.write_text(previous.replace(variable + ':BOOL=OFF', variable + ':BOOL=ON'))
            with self.assertRaises(self.helper.BuildError):
                self.metadata()
            self.cache.write_text(previous)
        for flag in ('-fsanitize=address', '-fprofile-instr-generate', '-fprofile-arcs', '--coverage'):
            (self.build / 'compile_commands.json').write_text(json.dumps([{'command': 'c++ ' + flag}]))
            with self.assertRaises(self.helper.BuildError):
                self.metadata()

    def test_diagnostic_mode_is_explicit_and_forbidden_for_release_metadata(self):
        self.cache.write_text(self.cache.read_text().replace('ENABLE_BC_PROFILE:BOOL=OFF',
                                                           'ENABLE_BC_PROFILE:BOOL=ON'))
        commands = self.build / 'compile_commands.json'
        commands.write_text(json.dumps([{'command': 'c++ -O3 -DGD_BC_PROFILE=1 -c source.cc'}]))
        with self.assertRaises(self.helper.BuildError):
            self.metadata()
        value = self.module.build_metadata(self.helper, self.root, self.root / 'source',
                                           self.build, 'a' * 40, 126,
                                           {'version': 'C'}, {'version': 'C++'}, True)
        self.assertTrue(value['instrumented'])
        self.assertTrue(value['configuration']['diagnostic_profile'])
        commands.write_text(json.dumps([{'command': 'c++ -O3 -c source.cc'}]))
        with self.assertRaises(self.helper.BuildError):
            self.module.build_metadata(self.helper, self.root, self.root / 'source', self.build,
                                       'a' * 40, 126, {'version': 'C'}, {'version': 'C++'}, True)
        self.cache.write_text(self.cache.read_text().replace('ENABLE_BC_COVERAGE:BOOL=OFF',
                                                           'ENABLE_BC_COVERAGE:BOOL=ON'))
        with self.assertRaises(self.helper.BuildError):
            self.module.build_metadata(self.helper, self.root, self.root / 'source', self.build,
                                       'a' * 40, 126, {'version': 'C'}, {'version': 'C++'}, True)

    def test_wrong_or_automatic_threshold_is_rejected(self):
        for content in ('#define BC_MULTIPLY_SPLIT_POINT 127\n',
                        '#define BC_MULTIPLY_SPLIT_POINT 126\n',
                        '#include "find-multiply-split-point-auto.hh"\n'):
            self.header.write_text(content)
            with self.assertRaises(self.helper.BuildError):
                self.metadata()

    def test_only_immutable_revisions_and_valid_points(self):
        self.assertEqual(self.module.revision_value('A' * 40), 'a' * 40)
        for value in ('main', 'HEAD', 'a' * 39, '--revision'):
            with self.assertRaises(argparse.ArgumentTypeError):
                self.module.revision_value(value)
        for value in ('0', '-1', '1e3', '01', '1000000001'):
            with self.assertRaises(argparse.ArgumentTypeError):
                self.module.positive_point(value)

    @unittest.skipUnless(os.name == 'posix', 'Unix compiler invocation aliases')
    def test_compiler_alias_preserves_driver_behavior(self):
        driver = self.root / 'compiler-driver'
        driver.write_text('#!/bin/sh\nprintf "%s\\n" "${0##*/}"\n')
        driver.chmod(0o755)
        alias = self.root / 'clang++'
        alias.symlink_to(driver)
        builder = self.helper.Builder(self.root, 3)
        value = self.module.compiler_identity(self.helper, builder, str(alias))
        self.assertEqual(value['realpath'], str(driver.resolve()))
        process = subprocess.run([value['argv0'], '--version'], stdout=subprocess.PIPE,
                                 universal_newlines=True, timeout=3)
        self.assertEqual(process.stdout.strip(), 'clang++')

    def test_failed_source_checkout_never_publishes_config(self):
        source = self.root / 'not-a-repository'
        source.mkdir()
        output = self.root / 'failed-build'
        child = subprocess.run([sys.executable, BUILDER, '--source', str(source),
                                '--revision', 'a' * 40, '--split-point', '126',
                                '--output', str(output)], stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, timeout=5)
        self.assertEqual(child.returncode, 1)
        self.assertFalse((output / 'implementations.json').exists())
        manifest = json.loads((output / 'build-manifest.json').read_text())
        self.assertEqual(manifest['status'], 'incomplete')
        self.assertIn('failure', manifest)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('builder')
    args, remaining = parser.parse_known_args()
    BUILDER = str(Path(args.builder).resolve())
    unittest.main(argv=[__file__, *remaining])
