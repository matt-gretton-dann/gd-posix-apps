#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
set -euo pipefail

source_dir=$(cd "${1:?source directory required}" && pwd)
output_dir=${2:?new output directory required}
mkdir "$output_dir"
output_dir=$(cd "$output_dir" && pwd)
evidence="$output_dir/evidence"
mkdir "$evidence"

git -C "$source_dir" rev-parse HEAD > "$evidence/revision.txt"
gcc --version > "$evidence/gcc.txt"
g++ --version > "$evidence/g++.txt"
cmake --version > "$evidence/cmake.txt"
for level in 2 3; do
  for mode in OFF REQUIRED; do
    role="gcc-o$level-${mode,,}"
    build_dir="$output_dir/$role"
    /usr/bin/time -p cmake -S "$source_dir" -B "$build_dir" -G Ninja \
      -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ \
      -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DBUILD_TESTING=OFF -DBC_MULTIPLY_SPLIT_POINT=126 "-DIPO_MODE=$mode" \
      "-DCMAKE_C_FLAGS_RELEASE=-O$level -DNDEBUG" \
      "-DCMAKE_CXX_FLAGS_RELEASE=-O$level -DNDEBUG" \
      2>&1 | tee "$evidence/$role-configure.log"
    /usr/bin/time -p cmake --build "$build_dir" --target bc --parallel 2 \
      2>&1 | tee "$evidence/$role-build.log"
    cp "$build_dir/compile_commands.json" "$evidence/$role-compile.json"
    python3 "$source_dir/src/bc/test-cli.py" "$build_dir/src/bc/bc" --extensions ON \
      2>&1 | tee "$evidence/$role-cli.log"
  done
done
python3 "$source_dir/src/bc/scripts/build-bc-comparators.py" --implementation howard \
  --cc gcc --output "$output_dir/howard"

python3 - "$output_dir" <<'PYTHON'
import hashlib
import json
from pathlib import Path
import shlex
import sys

root = Path(sys.argv[1])
evidence = root / 'evidence'
revision = (evidence / 'revision.txt').read_text().strip()
entries = []
for level in (2, 3):
    for mode in ('off', 'required'):
        role = f'gcc-o{level}-{mode}'
        commands = (evidence / f'{role}-compile.json').read_bytes()
        vm = next(row for row in json.loads(commands) if row['file'].endswith('/src/bc/vm.cc'))
        flags = vm.get('arguments') or shlex.split(vm['command'])
        assert [flag for flag in flags if flag.startswith('-O')] == [f'-O{level}']
        assert any(flag.startswith('-flto') for flag in flags) == (mode == 'required')
        binary = root / role / 'src/bc/bc'
        entries.append({
            'name': role,
            'argv': [str(binary)],
            'metadata': {
                'source_revision': revision,
                'compiler_version': (evidence / 'g++.txt').read_text().strip(),
                'compile_commands_sha256': hashlib.sha256(commands).hexdigest(),
                'binary_sha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
                'binary_bytes': binary.stat().st_size,
                'ipo_mode': mode.upper(),
                'split_point': 126,
            },
        })
entries.extend(json.loads((root / 'howard/implementations.json').read_text())['implementations'])
(evidence / 'implementations.json').write_text(json.dumps({'implementations': entries}, indent=2) + '\n')
PYTHON

for preset in full holdout; do
  python3 "$source_dir/src/bc/scripts/bench-bc.py" --config "$evidence/implementations.json" \
    --output "$output_dir/$preset" --preset "$preset" --reference gcc-o3-required \
    --samples 9 --warmups 1 --seed 197213 --corpus-seed 197213
  mkdir "$evidence/$preset"
  cp "$output_dir/$preset/"{manifest.json,setup.json,samples.jsonl,summary.json} "$evidence/$preset/"
done
