#!/usr/bin/env python3
"""Check canonical declarations on CRLF checkouts without waiving content.
SPDX-License-Identifier: Apache-2.0
"""
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
GENERATOR = ROOT / 'src/bc/scripts/generate-reconstruction-declaration.py'


class DeclarationText(unittest.TestCase):
    def test_crlf_checkout_preserves_the_canonical_declaration(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'declaration.json'
            command = [sys.executable, str(GENERATOR), '--output', str(path)]
            generated = subprocess.run(command, capture_output=True)
            self.assertEqual(generated.returncode, 0, generated.stderr)
            raw = path.read_bytes().replace(b'\r\n', b'\n')
            path.write_bytes(raw.replace(b'\n', b'\r\n'))
            checked = subprocess.run(command + ['--check'], capture_output=True)
            self.assertEqual(checked.returncode, 0, checked.stderr)

            # Line-ending normalization must not accept changed source identity.
            base = b'3beebb1461ac7549832707f9b84e547127e3c14a'
            self.assertIn(base, raw)
            path.write_bytes(path.read_bytes().replace(base, b'0' * 40))
            corrupted = subprocess.run(command + ['--check'], capture_output=True)
            self.assertNotEqual(corrupted.returncode, 0)


if __name__ == '__main__':
    unittest.main()
