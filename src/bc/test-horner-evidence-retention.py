#!/usr/bin/env python3
"""Check actual downloaded native evidence against its recorded byte hashes."""
import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath
import re
import zipfile


def canonical(name):
    name = name.replace('\\', '/')
    path = PurePosixPath(name)
    if path.is_absolute() or '..' in path.parts or ':' in name:
        raise ValueError('nonrelative retained evidence path')
    return name


def validate(read, source):
    metadata = json.loads(read('horner-build.json'))
    revision = read('revision.txt').decode('utf-8-sig').strip()
    if metadata['source'] != source or revision != source:
        raise ValueError('native evidence source identity differs')
    inputs = {canonical(name): digest for name, digest in metadata['inputs'].items()}
    if len(inputs) != len(metadata['inputs']):
        raise ValueError('duplicate canonical input paths')
    required = {'.github/workflows/bc-small-bessel-horner-correctness.yml',
                'src/bc/math-library.bc', 'build-native/src/bc/bc-math-library.hh'}
    if not required <= set(inputs):
        raise ValueError('native metadata omits a required source/data input')
    if metadata['options']['ENABLE_BC_PREPARSED_LIBRARY'] == 'ON':
        if 'build-native/src/bc/bc-math-library-image.inc' not in inputs:
            raise ValueError('native metadata omits its preparsed image')

    def checked(name, digest):
        if not re.fullmatch('[0-9a-f]{64}', digest):
            raise ValueError('invalid native evidence digest')
        try:
            raw = read(name)
        except (KeyError, FileNotFoundError) as error:
            raise ValueError('missing retained evidence: ' + name) from error
        if hashlib.sha256(raw).hexdigest() != digest:
            raise ValueError('retained evidence digest differs: ' + name)

    for name, digest in inputs.items():
        checked('inputs/' + name, digest)
    images = metadata['images']
    if len(images) != 4:
        raise ValueError('native evidence must retain all four tested images')
    for name, digest in images.items():
        checked('images/' + canonical(name), digest)
    available = json.loads(read('available-inputs.json'))
    if available['complete_build_metadata'] is not True:
        raise ValueError('native build metadata is incomplete')
    for name, digest in available['files'].items():
        checked('available/' + canonical(name), digest)
    return dict(source=source, inputs=len(inputs), images=len(images),
                available=len(available['files']),
                scope='Actual retained byte hashes only; no runtime, elapsed or adoption proof.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('evidence', type=Path, help='downloaded directory or original ZIP')
    parser.add_argument('--source', required=True)
    args = parser.parse_args()
    if not re.fullmatch('[0-9a-f]{40}', args.source):
        parser.error('expected a full source commit identity')
    try:
        if args.evidence.is_dir():
            root = args.evidence.resolve()

            def read(name):
                path = root / canonical(name)
                path.resolve().relative_to(root)
                return path.read_bytes()

            result = validate(read, args.source)
        else:
            with zipfile.ZipFile(args.evidence) as archive:
                if archive.testzip() is not None:
                    raise ValueError('native ZIP integrity failure')
                result = validate(archive.read, args.source)
    except (ValueError, KeyError, OSError, zipfile.BadZipFile) as error:
        raise SystemExit(str(error))
    print(json.dumps(result, sort_keys=True))


if __name__ == '__main__':
    main()
