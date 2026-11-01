#!/usr/bin/env python3
"""Validate threshold configuration before unsafe constants reach generated C++."""
import argparse
from pathlib import Path
import subprocess
import tempfile
import unittest


class MultiplyConfig(unittest.TestCase):
    def invoke(self, value, cross=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            probe = root / 'probe.cmake'
            result = root / 'choice.txt'
            probe.write_text('include("${MODULE}")\n'
                             'gd_bc_multiply_choice("${REQUESTED}" "${CROSS}" mode point)\n'
                             'file(WRITE "${OUTPUT}" "${mode}|${point}")\n')
            child = subprocess.run([CMAKE, '-DREQUESTED=' + value,
                                    '-DCROSS=' + ('ON' if cross else 'OFF'),
                                    '-DMODULE=' + MODULE, '-DOUTPUT=' + str(result),
                                    '-P', str(probe)], stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE, universal_newlines=True, timeout=3)
            return child, result.read_text() if result.exists() else None

    def test_portable_default_never_runs_target_calibration(self):
        for cross in (False, True):
            with self.subTest(cross=cross):
                child, choice = self.invoke('portable', cross)
                self.assertEqual(child.returncode, 0, child.stderr)
                self.assertEqual(choice, 'fixed|126')

    def test_native_auto_remains_calibrated(self):
        child, choice = self.invoke('auto')
        self.assertEqual(child.returncode, 0, child.stderr)
        self.assertEqual(choice, 'auto|')

    def test_cross_auto_has_portable_fixed_fallback(self):
        child, choice = self.invoke('auto', cross=True)
        self.assertEqual(child.returncode, 0, child.stderr)
        self.assertEqual(choice, 'fixed|128')

    def test_explicit_choice_is_independent_of_execution_host(self):
        for cross in (False, True):
            for point in ('1', '126', '1000000000'):
                child, choice = self.invoke(point, cross)
                self.assertEqual(child.returncode, 0, child.stderr)
                self.assertEqual(choice, 'fixed|' + point)

    def test_invalid_or_injectable_constants_are_rejected(self):
        for value in ('', '0', '-1', '+1', '01', '1.5', '1e3', '1000000001',
                      '9' * 100, '1;2', 'auto;1', '1\n#error invalid'):
            with self.subTest(value=value):
                child, choice = self.invoke(value)
                self.assertNotEqual(child.returncode, 0)
                self.assertIn('BC_MULTIPLY_SPLIT_POINT', child.stderr)
                self.assertIsNone(choice)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('cmake')
    parser.add_argument('module')
    args, remaining = parser.parse_known_args()
    CMAKE = args.cmake
    MODULE = str(Path(args.module).resolve())
    unittest.main(argv=[__file__, *remaining])
