#!/usr/bin/env python3
"""Run the fixed startup study for the standalone logarithm."""
# SPDX-License-Identifier: Apache-2.0
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile

COLLECTOR = '4e749f37cffcd565f8d411af01dc6f51204b8697'
PATH = 'docs/utilities/bc-bessel-turning-qualification/collect.py'
CANDIDATE = '3711f6b05e073637ef85636a0fb1cf746061cd2f'
BASE = '4479b0efdfeab9ada458a343d0a37895034663c9'
SUITES = ('startup',)


def main():
    raw = subprocess.check_output(['git', 'show', COLLECTOR + ':' + PATH])
    receipt = dict(collector=COLLECTOR, collector_file=PATH,
                   collector_sha256=hashlib.sha256(raw).hexdigest(),
                   candidate=CANDIDATE, base=BASE, suites=SUITES)
    Path('qualification-collector.json').write_text(
        json.dumps(receipt, indent=2) + '\n', encoding='utf-8')
    with tempfile.TemporaryDirectory(prefix='bc-log-startup-driver-') as temporary:
        path = Path(temporary) / 'collect.py'
        path.write_bytes(raw)
        spec = importlib.util.spec_from_file_location('sealed_qualification', path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        if SUITES[0] not in module.SUITES:
            raise ValueError('the pinned collection protocol does not match the declared study')
        module.BASE = BASE
        module.CANDIDATE = CANDIDATE
        module.SUITES = SUITES
        module.main()


if __name__ == '__main__':
    main()
