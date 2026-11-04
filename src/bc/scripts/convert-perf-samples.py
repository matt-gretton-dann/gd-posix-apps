#!/usr/bin/env python3
"""Convert PC-only perf samples into an experimental Clang sample profile."""

import argparse
from collections import Counter, defaultdict
from dataclasses import dataclass, field
import json
from pathlib import Path
import re
import subprocess
import sys


# Input from perf script --no-demangle -F ip,sym,symoff,dso.
SAMPLE = re.compile(r'\s*[0-9a-f]+\s+(\S+?)(?:\+0x([0-9a-f]+))?\s+\((.*)\)\s*')


@dataclass
class Profile:
    total: int = 0
    lines: Counter = field(default_factory=Counter)
    calls: dict = field(default_factory=dict)

    def add(self, frames, count):
        node = self
        for (_, caller_location), (callee, _) in zip(frames, frames[1:]):
            node.total += count
            node = node.calls.setdefault((caller_location, callee), Profile())
        node.total += count
        node.lines[frames[-1][1]] += count

    def write(self, depth):
        prefix = ' ' * depth
        for location, count in sorted(self.lines.items()):
            print(f'{prefix}{location}: {count}')
        for (location, name), child in sorted(self.calls.items()):
            print(f'{prefix}{location}: {name}:{child.total}')
            child.write(depth + 1)


def addresses(binary, trace, nm, stats):
    symbols = defaultdict(set)
    for row in subprocess.check_output(
        [nm, '--defined-only', '--numeric-sort', str(binary)], text=True
    ).splitlines():
        address, kind, name = row.split(maxsplit=2)
        if kind in ('t', 'T', 'w', 'W'):
            symbols[name].add(int(address, 16))
    counts = Counter()
    for row in trace:
        if not row.strip():
            continue
        stats['total'] += 1
        sample = SAMPLE.fullmatch(row.rstrip('\n'))
        if sample is None:
            stats['unparsed'] += 1
            continue
        name, offset, module = sample.groups()
        if Path(module) != binary:
            stats['other_module'] += 1
        elif len(symbols.get(name, ())) != 1:
            stats['unmapped_symbol'] += 1
        else:
            counts[next(iter(symbols[name])) + int(offset or '0', 16)] += 1
    return counts


def base_discriminator(raw):
    # LLVM's non-FS DWARF discriminator starts with the prefix-encoded
    # base component. The remaining bits describe duplication and copies.
    # See llvm/Support/Discriminator.h.
    if raw & 1:
        return 0
    value = raw >> 1
    low = value & 0x1f
    return low | ((value >> 1) & 0xfe0) if value & 0x20 else low


def line_frames(symbols):
    frames = []
    # DWARF inline frames arrive from the leaf to the containing function.
    for symbol in reversed(symbols):
        name = symbol['FunctionName']
        line, start = symbol['Line'], symbol['StartLine']
        if not name or not start or line < start:
            return []
        location = str(line - start)
        discriminator = base_discriminator(symbol['Discriminator'])
        if discriminator:
            location += f'.{discriminator}'
        frames.append((name, location))
    return frames


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path, help='exact sampled ELF with line tables')
    parser.add_argument('trace', type=Path, help='decoded perf script trace')
    parser.add_argument('--llvm-suffix', default='', help='LLVM tool suffix, for example -18')
    args = parser.parse_args()
    binary = args.binary.resolve()
    stats = Counter()
    with args.trace.open(encoding='utf-8') as trace:
        counts = addresses(binary, trace, 'llvm-nm' + args.llvm_suffix, stats)
    requests = ''.join(f'{address:#x}\n' for address in counts)
    decoded = subprocess.check_output(
        ['llvm-symbolizer' + args.llvm_suffix, f'--obj={binary}',
         '--no-demangle', '--inlines', '--output-style=JSON'],
        input=requests, text=True,
    )
    profiles = {}
    for row in decoded.splitlines():
        info = json.loads(row)
        count = counts.pop(int(info['Address'], 16))
        frames = line_frames(info['Symbol'])
        if not frames:
            stats['unmapped_line'] += count
            continue
        profiles.setdefault(frames[0][0], Profile()).add(frames, count)
        stats['profiled'] += count
    if counts:
        parser.exit(1, 'Missing symbolizer responses\n')
    print(json.dumps(stats, sort_keys=True), file=sys.stderr)
    if not profiles:
        parser.exit(1, 'No usable samples\n')
    for name, profile in sorted(profiles.items()):
        # A sampled PC gives no direct measurement of function entries.
        print(f'{name}:{profile.total}:0')
        profile.write(1)


if __name__ == '__main__':
    main()
