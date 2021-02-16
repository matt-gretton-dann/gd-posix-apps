#!/usr/bin/env python3
"""Script to generate the file uncompressible.bin.

The output of this script is a 64KB string that contains every possible pairing
of bytes without repeating any pairs.

The algorithm to do this is derived as follows for the range [0, 3] which can
then be extended for any integers [n, m]:

Start by producing every pair [0, m] for m in the range 1 - 3:
  010203

At this point we can't append '0' as that will leave us no value to append that
we haven't already seen so we prepend '3' to the string:

 3010203

Now on the right-hand side walk down from 3 to 1 as follows:

 30102033231

Again as before we can't go back up to 3 here so prepend 1, and then walk up
from 1 to 2:

 13010203323112

 As at this point we swap over we prepend 2 to the string, and also append to
 get the repeat.

  213010203323112

What is missing here is a repeated 00 pattern - we put that in where the first
0 appears:

  21300102033231122

"""

import numbers
import sys

out1 = b''
out2 = b''
low = 0
high = 255

while low < high:
    for i in range(low + 1, high + 1):
        out1 += low.to_bytes(1, byteorder='little')
        out1 += i.to_bytes(1, byteorder='little')
    for i in range(high - 1, low, -1):
        out1 += high.to_bytes(1, byteorder='little')
        out1 += i.to_bytes(1, byteorder='little')
    out2 += low.to_bytes(1, byteorder='little')
    out2 += high.to_bytes(1, byteorder='little')
    low += 1
    high -= 1

sys.stdout.buffer.write(out2[::-1] + out1 + out1[-1::])
