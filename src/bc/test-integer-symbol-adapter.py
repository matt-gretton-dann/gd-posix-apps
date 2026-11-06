#!/usr/bin/env python3
"""Keep missing or incompatible adapters from hiding a native boundary failure."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ENGINE = b'00000000 T plan_integer_function(Instructions const&)\n00000010 T run_integer_core(Core&)\n'
ROUTER = (b'00000020 T GD::Bc::Details::dispatch_integer_call('
          b'GD::Bc::BasicNumber<T> (*)(void*), void*)\n')
OWNER = (b'00000000 T public: __cdecl GD::Bc::BasicNumber<T>::~BasicNumber<T>(void)\n'
         b'00000000 T public: __cdecl GD::Bc::Details::BasicDigits<T>::~BasicDigits<T>(void)\n')


class SymbolAdapter(unittest.TestCase):
    def collect(self, missing=False, incompatible=False, owner=False, available=True):
        spec = importlib.util.spec_from_file_location(
            'integer_symbols', Path(__file__).with_name('test-integer-symbols.py'))
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            preferred, llvm = root / 'preferred', root / 'llvm-nm'
            for path in (preferred, llvm, root / 'integer-function.obj', root / 'integer-call.obj'):
                path.write_bytes(b'test object or adapter')

            def which(name):
                if not available:
                    return None
                if name == str(preferred) and not missing:
                    return str(preferred)
                return str(llvm) if name in ('llvm-nm', 'llvm-nm.exe') else None

            def run(command, **unused):
                if incompatible and command[0] == str(preferred):
                    return subprocess.CompletedProcess(command, 1, b'', b'unsupported options')
                data = ENGINE if 'integer-function' in command[-1] else ROUTER + (OWNER if owner else b'')
                return subprocess.CompletedProcess(command, 0, data, b'')

            args = ['collector', '--nm', str(preferred), '--object',
                    str(root / 'integer-function.obj'), str(root / 'integer-call.obj'),
                    '--output', str(root / 'evidence')]
            with mock.patch.object(module.shutil, 'which', side_effect=which), \
                    mock.patch.object(module.subprocess, 'run', side_effect=run), \
                    mock.patch.object(sys, 'argv', args), contextlib.redirect_stdout(io.StringIO()):
                result = module.main()
            report = json.loads(next((root / 'evidence').glob('*/report.json')).read_text())
            return result, report

    def test_missing_preferred_uses_available_llvm(self):
        status, report = self.collect(missing=True)
        self.assertEqual((status, report['status']), (0, 'pass'))
        self.assertEqual(Path(report['tool']).name, 'llvm-nm')

    def test_incompatible_preferred_uses_available_llvm(self):
        status, report = self.collect(incompatible=True)
        self.assertEqual((status, report['status']), (0, 'pass'))
        self.assertEqual(Path(report['tool']).name, 'llvm-nm')

    def test_fallback_adapter_exposes_coff_ownership_definitions(self):
        status, report = self.collect(missing=True, owner=True)
        self.assertEqual((status, report['status']), (1, 'failure'))
        self.assertEqual(len(report['number_definitions']), 2)

    def test_borrowed_number_signature_is_accepted(self):
        status, report = self.collect()
        self.assertEqual((status, report['status'], report['number_definitions']), (0, 'pass', []))

    def test_real_absence_remains_explicitly_unavailable(self):
        status, report = self.collect(available=False)
        self.assertEqual((status, report['status']), (77, 'unavailable'))


if __name__ == '__main__':
    unittest.main()
