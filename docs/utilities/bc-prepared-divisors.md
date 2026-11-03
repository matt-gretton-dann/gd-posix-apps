# Reusable coefficient divisors

Issue #420 tracks immutable prepared divisors within #197. The coefficient
API is an implementation step; it does not yet cache divisors in the VM or
establish a performance improvement.

`Details::BasicDigits<Traits>::prepare_divisor()` retains the original positive
coefficient, an Algorithm D normalization factor, and normalized coefficient
limbs. Preparation rejects zero with `std::domain_error`. Copies retain the
same immutable value, including after the original coefficient is mutated or
destroyed. Leading zero padding is trimmed in the snapshot without modifying
the source. Already canonical coefficients with factor one need no writable
normalization copy.

The `divide` and `modulo` overloads accept smaller, equal and zero dividends.
Long division reads the prepared normalized limbs directly and scales only
the dividend. Remainders are unnormalized exactly; quotient-only calls retain
the existing result path. Scalar divisors and two-limb dividends use the
existing generic short paths. All limb widths use the same implementation.

Preparation retains up to two coefficient owners and may allocate a normalized
copy. This cost must be measured separately from repeated use, alongside the
ordinary unprepared path and copy/reset controls. This API handles unsigned
integer coefficients only. Decimal scale alignment, signs, input-base parsing
and the language's zero-divisor diagnostic remain responsibilities of the
number and VM layers.

Tests cover independent native integer quotient/remainder oracles, exhaustive
bounded eight-bit cases, directed quotient-estimate correction and add-back
at all three limb widths, normalization that grows inline dividend storage,
padded source coefficients, mutation/destruction, copies/moves, aliases and
frozen large Python `divmod` results. Formatting joins continuation lines only
for comparison with canonical decimal oracle strings.

Before automatic reuse, collect preparation and reuse costs, allocation/work
counters, code size, process memory and checked component timing. Any VM cache
must be bounded and preserve changing operands, scales, input bases, arrays,
recursion and redefinitions. A storage address alone does not prove the value
has not changed. Retaining strong snapshots on each first use or miss can
force avoidable copy-on-write allocations. Both stable and changing divisor
workloads, plus the full native regression matrix, remain adoption gates.

## Component protocol

`bc-components COMPONENT ITERATIONS SPLIT_POINT` accepts two unsigned decimal
integer lines for the following diagnostics:

| Component | Repeated work | Final result |
| --- | --- | --- |
| `coefficient-control` | Dividend copy/reset and limb-count consumer | Dividend |
| `coefficient-divide` | Ordinary integer quotient | Quotient |
| `coefficient-modulo` | Ordinary integer remainder | Remainder |
| `prepared-divide` | Quotient with one prepared divisor | Quotient |
| `prepared-modulo` | Remainder with one prepared divisor | Remainder |
| `prepare-divisor` | Prepare the divisor anew each time | Divisor |

All modes require a nonzero divisor. Signs, fractions, missing operands, extra
nonempty lines and an explicit text-base argument are rejected. Input reading,
decimal coefficient construction and final rendering occur outside the reported
operation interval. Preparation in the two prepared arithmetic modes is outside
that interval and has its own `setup_seconds`. The preparation-only mode includes
preparation and destruction in each iteration. No control duration is subtracted.
The split-point argument is retained for the driver protocol; these coefficient
operations do not use the multiplication threshold.

The checksum consumes stored coefficient limb counts, rather than decimal
lengths. Ordinary quotient buffers may retain leading zero padding, so checksums
need not match between modes; final canonical decimal results must match the
independent oracle. Instrumented builds emit null timing fields and separate
setup/operation allocation calls and requested bytes, plus operation COW clone
counts. These selected allocator counters do not represent all process malloc
traffic or process RSS, and they do not report a phase-local peak.
