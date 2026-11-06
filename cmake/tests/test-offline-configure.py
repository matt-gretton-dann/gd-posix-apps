#!/usr/bin/env python3
"""Check downloads-disabled configuration with and without installed Catch2."""

import argparse
import json
from pathlib import Path
import subprocess
import tempfile
import unittest


class OfflineConfigure(unittest.TestCase):
    def command(self, argv, cwd=None):
        run = subprocess.run(argv, cwd=cwd, stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT, universal_newlines=True,
                             timeout=90)
        self.assertEqual(run.returncode, 0, run.stdout)
        return run.stdout

    def configure(self, root, extra):
        build = root / 'project-build'
        self.command([ARGS.cmake, '-S', ARGS.source, '-B', str(build),
                      '-G', ARGS.generator, '-DBUILD_TESTING=ON',
                      '-DALLOW_THIRD_PARTY_DOWNLOADS=OFF', *extra])
        # No fetched source can silently replace the installed/missing case.
        self.assertFalse(list((build / '_deps').glob('*-src')))
        return build

    def test_missing_catch_keeps_integration_tests_available(self):
        with tempfile.TemporaryDirectory(prefix='gd-offline-missing-') as temporary:
            build = self.configure(Path(temporary),
                                   ['-DCMAKE_DISABLE_FIND_PACKAGE_Catch2=TRUE'])
            listing = self.command([ARGS.ctest, '-C', 'Release',
                                    '--show-only=json-v1'], cwd=str(build))
            names = {test['name'] for test in json.loads(listing)['tests']}
            self.assertIn('bc/cli', names)
            self.assertIn('bc/vm-cli', names)
            targets = (build / 'CMakeFiles/TargetDirectories.txt').read_text()
            self.assertNotIn('test-libbc.dir', targets)

    def test_installed_catch_enables_unit_targets_without_downloads(self):
        if not ARGS.catch_source:
            self.skipTest('Catch2 source unavailable for the installed-package check')
        with tempfile.TemporaryDirectory(prefix='gd-offline-installed-') as temporary:
            root = Path(temporary)
            dependency = root / 'catch-build'
            prefix = root / 'installed'
            # Install the actual header-only dependency and its CMake helpers;
            # no test executable, static implementation or network is needed.
            self.command([ARGS.cmake, '-S', ARGS.catch_source, '-B', str(dependency),
                          '-G', ARGS.generator, '-DBUILD_TESTING=OFF',
                          '-DCATCH_BUILD_TESTING=OFF', '-DCATCH_BUILD_EXAMPLES=OFF',
                          '-DCATCH_BUILD_EXTRA_TESTS=OFF',
                          '-DCATCH_BUILD_STATIC_LIBRARY=OFF',
                          '-DCATCH_INSTALL_DOCS=OFF', '-DCATCH_INSTALL_HELPERS=ON',
                          '-DCMAKE_INSTALL_PREFIX=' + str(prefix)])
            self.command([ARGS.cmake, '--install', str(dependency),
                          '--config', 'Release'])
            configs = list(prefix.rglob('Catch2Config.cmake'))
            self.assertEqual(len(configs), 1)
            build = self.configure(root, ['-DCatch2_DIR=' + str(configs[0].parent)])
            targets = (build / 'CMakeFiles/TargetDirectories.txt').read_text()
            self.assertIn('test-libbc.dir', targets)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cmake', required=True)
    parser.add_argument('--ctest', required=True)
    parser.add_argument('--source', required=True)
    parser.add_argument('--generator', required=True)
    parser.add_argument('--catch-source', default='')
    ARGS, remaining = parser.parse_known_args()
    unittest.main(argv=[__file__, *remaining])
