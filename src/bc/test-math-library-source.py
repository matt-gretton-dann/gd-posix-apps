#!/usr/bin/env python3
"""Require the embedded -l program to match its portable source exactly."""
from pathlib import Path
import re
import sys

source = Path(sys.argv[1]).read_text(encoding='utf-8')
header = Path(sys.argv[2]).read_text(encoding='utf-8')
match = re.search(r'R"GD_BC_LIBRARY\((.*)\)GD_BC_LIBRARY"', header, re.DOTALL)
if match is None or match.group(1) != source:
    raise SystemExit('embedded math-library program differs from its portable source')
print('Portable source and embedded program match (normalized text newlines)')
