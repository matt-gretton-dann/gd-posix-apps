# Numeric continuation lines

Number output accounts for its current line position, sign, radix point,
coefficient digits and leading fractional zeros. Continuations use a backslash
and newline. Existing integer and nondecimal chunk boundaries remain compatible;
physical lines, including the continuation marker, stay within 70 columns.
Callers can begin output on a partially occupied line.

A small coefficient at a high decimal scale still requires every fractional
zero. For example, `0.` followed by 79 zeros and `1` has 80 fractional places.
The zeros must pass through continuation handling; a single padded stream field
can otherwise exceed the line bound. Decimal padding uses a fixed buffer of at
most 69 characters and drains only the available capacity before continuing.
It does not allocate a temporary buffer proportional to the requested scale.

The #320 regression fixtures check all three limb widths, both signs, sparse
fractional coefficients, adjacent integer lengths and initial positions around
the boundary. They check exact values after joining continuations separately
from physical line lengths. Extensions ON/OFF retain their established choice
of whether a fractional value prints a zero before the radix point.
