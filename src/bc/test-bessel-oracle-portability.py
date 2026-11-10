#!/usr/bin/env python3
"""Check enclosure provenance across LF and native Windows CRLF checkouts."""
import argparse
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parent


class GeneratorPortability(unittest.TestCase):
    def test_native_newlines_keep_the_same_certified_fixture(self):
        generator = ROOT/'scripts/generate-bessel-classification-oracles.py'
        fixture = ROOT/'fixtures/math-bessel-classification-oracles.json'
        canonical = generator.read_bytes().replace(b'\r\n', b'\n')
        fixture_bytes = fixture.read_bytes().replace(b'\r\n', b'\n')
        for generator_newline in (b'\n', b'\r\n'):
            for fixture_newline in (b'\n', b'\r\n'):
                with self.subTest(generator=repr(generator_newline), fixture=repr(fixture_newline)), tempfile.TemporaryDirectory() as directory:
                    root = Path(directory)/'bc'
                    (root/'scripts').mkdir(parents=True)
                    (root/'fixtures').mkdir()
                    target = root/'scripts'/generator.name
                    target.write_bytes(canonical.replace(b'\n', generator_newline))
                    (root/'fixtures'/fixture.name).write_bytes(fixture_bytes.replace(b'\n', fixture_newline))
                    result = subprocess.run([sys.executable, str(target), '--check'],
                                            capture_output=True, text=True, timeout=10)
                    self.assertEqual(result.returncode, 0, result.stderr)



if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    args, rest = parser.parse_known_args()
    ROOT = args.root.resolve()
    unittest.main(argv=[__file__, *rest])
