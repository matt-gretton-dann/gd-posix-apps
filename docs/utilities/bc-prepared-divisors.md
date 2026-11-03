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


## Initial local observations

The retained [bundle](bc-prepared-divisors/components-v1.zip) and
[summary](bc-prepared-divisors/components-v1.json) contain source revision
`9b1b67e2dd29cd65dfc74c02004c9db6dca433bf`, two exact executable copies,
build recipes, source snapshots, the collector, every raw output and all
predeclared full/held-out fixtures. Apple Clang 21 Release builds use extensions
OFF, fixed split point 126 and the default IPO policy. Profiling and timing use
separate executables. All 72 profiling and 576 timing observations pass Python
integer oracles. The verifier replays retained outputs and provenance; it does
not execute the binaries or reproduce the original timings.

Each fixture runs 2,000 operations. The timing phase has eight trials per mode;
each shuffled six-mode order is followed by its reverse. Owned local builds,
tests and profiling had finished before timing. The host was otherwise
unisolated. Setup and control durations are retained without subtraction.

For 603-digit dividends and 234-digit divisors, reuse reduces quotient allocation
calls from 14,000 to 10,000 and remainder allocation calls from 10,000 to 6,000.
Requested operation storage falls by 304,000 bytes in each case. A divisor
requiring normalization costs two setup allocation calls / 152 requested bytes;
an already normalized divisor costs none. These counts cover the tracked
allocators, including their ownership control blocks, rather than all process
allocations. Retained snapshots still consume memory; the separate process RSS observations
below do not establish an equivalent reduction in physical memory.

The per-case median ratios below divide prepared operation time by ordinary
operation time. They are descriptive local observations, without confidence
intervals or an adoption decision. In particular, both largest held-out quotient
cases are slower; the bundle retains those observations unchanged.

| Case | Quotient ratio | Remainder ratio |
| --- | ---: | ---: |
| full-0-leading-1 | 0.8258 | 0.8073 |
| full-0-leading-9 | 0.8365 | 0.8022 |
| full-1-leading-1 | 0.9847 | 0.9766 |
| full-1-leading-9 | 0.9890 | 0.9937 |
| full-2-leading-1 | 0.9984 | 0.9694 |
| full-2-leading-9 | 0.9458 | 0.9971 |
| held-out-0-leading-1 | 0.8623 | 0.8249 |
| held-out-0-leading-9 | 0.8783 | 0.8275 |
| held-out-1-leading-1 | 0.9795 | 0.9757 |
| held-out-1-leading-9 | 0.9790 | 0.9822 |
| held-out-2-leading-1 | 1.0292 | 0.9763 |
| held-out-2-leading-9 | 1.0370 | 0.9547 |

Run `python3 src/bc/scripts/verify-prepared-components.py` to check the published
ZIP hash, exact source/binary/recipe/output bytes, complete observation grid,
independent integer oracles and reported median ratios. Corruption tests cover
missing samples disguised by duplicates, executable changes, oracle changes,
false adoption and replacement of an adverse ratio.

This establishes a reduction in repeated coefficient allocation work. It does
not yet justify a VM cache, universal speedup or whole-program adoption. Code inspection and the native full/held-out matrix remain required before
integration; changing-divisor and first-use costs remain separate VM concerns.


A separate [RSS bundle](bc-prepared-divisors/rss-v1.zip) and
[RSS summary](bc-prepared-divisors/rss-v1.json) retain 216 checked processes:
all 12 fixtures and six modes, with three predetermined samples each. The
exact uninstrumented executable is bound to the component manifest by SHA-256.
`/usr/bin/time -l` records macOS maximum resident set size, including startup,
input reading and coefficient parsing. Its raw durations are retained separately
and are not added to the component timing sample grid. This run began after the
owned build/test/profile/timing processes had finished; the host was unisolated.

The 603/234-digit full case requiring normalization has median quotient peak RSS
2,998,272 bytes ordinary versus 2,981,888 prepared; remainder peaks are
2,965,504 versus 2,949,120 bytes. The largest held-out case with an already
normalized divisor instead has quotient peak RSS 2,981,888 ordinary versus
2,998,272 prepared. Three samples and page-granularity differences do not prove
a general memory improvement. All modes, resource outputs and adverse results
remain in the retained grid. The verifier binds these observations to the exact
component executable, checks outputs, recomputes RSS medians and rejects duplicate
observations that conceal missing samples.


## Bounded admission prototype

`BasicDigits<Traits>::DivisorCache` is a one-entry coefficient admission
prototype; automatic VM dispatch is still pending. An eligible new owner records
only weak ownership identity and the requested decimal padding. That first
observation needs no limb scan, allocation or strong coefficient snapshot. A
second observation of the same owner/padding records a fingerprint; a third
stable observation prepares the current coefficient. Changing unique-owner
values do not retain snapshots, so observation does not force COW allocations.

The fingerprint only controls admission. It never proves that cached prepared
limbs match the current input. A promoted entry retains an immutable unpadded
source snapshot as well as its prepared aligned coefficient. A hit requires
exact ownership and padding identity; subsequent writes must detach from the
retained source. Keeping the raw snapshot is necessary when padding has already
made the aligned coefficient detach. Weak owner ordering distinguishes control
block lifetimes, including expiry, without using reusable storage addresses.

Inline, zero and oversized inputs use the ordinary fallback. Retained limb
capacity is conservatively charged for all three coefficient views, including
shared views, with a 64 KiB cap. The fixed cache object, ownership control blocks
and external owners are separate from this limb-storage bound. A shrunk vector
can retain a large allocation, so admission checks capacity rather than used
limbs. Promotion checks actual retained capacities before publishing the entry.
An over-budget candidate is not retained; construction can transiently allocate
before that check. Failed allocation unwinds the candidate and permits retry.

The returned prepared pointer remains valid until the next lookup or cache
destruction. Misses release an old retained source before the ordinary fallback.
Tests cover changing unique owners, shared hits, padding changes and subsequent
mutation, destruction of the last external owner, replacement lifetimes,
capacity retained after shrinking, and clearing. An isolated executable replaces
global new/delete only within that test process: first observation plus 100
changing unique-owner requests require zero allocation calls, and each actual
promotion allocation is failed in turn to check cleanup and retry. The production
code contains no fault-injection hooks.

This prototype establishes admission and lifetime behavior. It does not yet
establish VM hit rates, whole-program performance, or a default activation policy.
The existing scalar/short and ordinary coefficient paths remain available.
