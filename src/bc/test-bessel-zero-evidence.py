#!/usr/bin/env python3
"""Exercise the native byte-retention contract with synthetic bytes only."""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location(
    'retention', Path(__file__).parent / 'scripts/verify-bessel-zero-evidence.py')
retention = importlib.util.module_from_spec(spec)
spec.loader.exec_module(retention)
SOURCE = 'a' * 40


def fixture(image=True, windows=False, ipo=False):
    files = {'revision.txt': (SOURCE + '\n').encode()}
    inputs = {name: hashlib.sha256(b'synthetic input').hexdigest() for name in (
        '.github/workflows/bc-bessel-zero-correctness.yml',
        'src/bc/vm.cc', 'src/bc/test-bessel-zero-proof.py',
        'src/bc/test-bessel-precision-proof.py',
        'src/bc/function-slot.hh','src/bc/integer-recurrence.cc',
        'src/bc/test-integer-recurrence.cc','src/bc/test-math-budget-proof.py', 'src/bc/fixtures/math-bessel-classification-oracles.json', 'src/bc/math-library.bc',
        'build-native/src/bc/bc-math-library.hh')}
    if image:
        inputs['build-native/src/bc/bc-math-library-image.inc'] = next(iter(inputs.values()))
    files.update({'inputs/' + name: b'synthetic input' for name in inputs})
    names = ['bc', 'test-libbc', 'test-bc-signal', 'test-divisor-cache-lifetime',
             'test-bc-vm-lifetime', 'test-bc-consumer-mode', 'test-bc-function-slot']
    if image:
        names.append('test-bc-library-lifetime')
    if ipo:
        names.append('test-bc-vm-lifetime-ipo')
    images = {name + ('.exe' if windows else ''):
              hashlib.sha256(b'synthetic executable').hexdigest() for name in names}
    files.update({'images/' + name: b'synthetic executable' for name in images})
    metadata = dict(source=SOURCE, inputs=inputs, images=images,
                    system='Windows' if windows else 'Linux',
                    options={'ENABLE_BC_PREPARSED_LIBRARY': 'ON' if image else 'OFF'})
    files['bessel-zero-build.json'] = json.dumps(metadata).encode()
    files['available-inputs.json'] = json.dumps(dict(
        complete_build_metadata=True, files={})).encode()
    return files


class RetentionTests(unittest.TestCase):
    def validate(self, files):
        return retention.validate(files.__getitem__, SOURCE)

    def test_complete_modes(self):
        for image in (False, True):
            for windows in (False, True):
                for ipo in (False, True):
                    with self.subTest(image=image, windows=windows, ipo=ipo):
                        self.assertEqual(self.validate(fixture(image, windows, ipo))['images'],
                                         7 + int(image) + int(ipo))

    def test_missing_or_corrupt_bytes(self):
        original = fixture()
        for name in original:
            if not name.startswith(('inputs/', 'images/')):
                continue
            for corrupt in (False, True):
                files = copy.deepcopy(original)
                if corrupt:
                    files[name] += b'changed'
                else:
                    del files[name]
                with self.subTest(name=name, corrupt=corrupt):
                    with self.assertRaises(ValueError):
                        self.validate(files)

    def test_incomplete_metadata(self):
        for section, name in (('inputs', 'src/bc/test-bessel-zero-proof.py'),
                              ('inputs', 'src/bc/test-bessel-precision-proof.py'),
                              ('inputs', 'src/bc/fixtures/math-bessel-classification-oracles.json'),
                              ('inputs', 'build-native/src/bc/bc-math-library-image.inc'),
                              ('images', 'test-bc-vm-lifetime')):
            files = fixture()
            metadata = json.loads(files['bessel-zero-build.json'])
            del metadata[section][name]
            files['bessel-zero-build.json'] = json.dumps(metadata).encode()
            with self.subTest(name=name), self.assertRaises(ValueError):
                self.validate(files)

    def test_wrong_source_and_digest(self):
        for field, value in (('source', 'b' * 40), ('inputs', {'src/bc/vm.cc': 'invalid'})):
            files = fixture()
            metadata = json.loads(files['bessel-zero-build.json'])
            if field == 'inputs':
                metadata[field].update(value)
            else:
                metadata[field] = value
            files['bessel-zero-build.json'] = json.dumps(metadata).encode()
            with self.subTest(field=field), self.assertRaises(ValueError):
                self.validate(files)

    def test_partial_build_and_available_bytes(self):
        files = fixture()
        for available in (dict(complete_build_metadata=False, files={}),
                          dict(complete_build_metadata=True, files={
                              'missing': hashlib.sha256(b'absent').hexdigest()})):
            files['available-inputs.json'] = json.dumps(available).encode()
            with self.assertRaises(ValueError):
                self.validate(files)

    def test_unsafe_or_aliased_paths(self):
        for name in ('', '/absolute', '../outside', 'a/../b', './a', 'a//b', 'C:/a'):
            with self.subTest(name=name), self.assertRaises(ValueError):
                retention.canonical(name)
        files = fixture()
        metadata = json.loads(files['bessel-zero-build.json'])
        metadata['inputs']['src\\bc\\vm.cc'] = metadata['inputs']['src/bc/vm.cc']
        files['bessel-zero-build.json'] = json.dumps(metadata).encode()
        with self.assertRaises(ValueError):
            self.validate(files)


if __name__ == '__main__':
    unittest.main()
