# Exact Toom-3 experiment (#204)

The accepted multiplication default remains schoolbook/Karatsuba, with its
existing portable workspace. `BC_TOOM3_THRESHOLD=0` disables the experiment.
An explicit threshold of 6 through 1,000,000,000 limbs permits a balanced
Toom-3 tier above the existing schoolbook split point. There is no automatic
threshold selection or architecture requirement. Narrow accumulator contracts,
radices below eight and asymmetric operands retain the generic coefficient
kernel. A threshold is an experimental input, not evidence of a crossover.

`LimbToom3` splits each coefficient into three radix-power chunks. It evaluates
at 0, 1, -1, 2 and infinity, recursively computes five products, and interpolates
using unsigned magnitudes with separate signs for the -1 evaluations. This uses
the point equations described in the
[GMP algorithm manual](https://gmplib.org/manual/Toom-3_002dWay-Multiplication),
with independent implementation and coefficient storage.

Let `v0`, `vp`, `vm`, `v2` and `v4` be those signed point products. Then
`s=(vp-vm)/2=c1+c3` and `c2=(vp+vm)/2-v0-v4`. Removing `v0+16*v4` from `v2`
and dividing by two gives `c1+2*c2+4*c3`; removing `s+2*c2` gives `3*c3`.
All intermediate magnitudes in this order are nonnegative, and every division
by two or three is exact. Debug builds assert nonnegative subtraction and zero
remainders. Recombination adds the five coefficients at whole-limb offsets;
scale truncation, number sign, COW and aliases remain with the existing wrapper.

For a chunk of `k` limbs at radix `B`, the largest evaluation is below
`7*B^k`; at `B>=8` it occupies at most `k+1` limbs. Inputs of at least six limbs
therefore shrink recursively. Both operands must exceed `2*ceil(n/3)` limbs,
where `n` is the longer length, so no high chunk is empty. The widened type
must represent `16*B`; scalar factors up to sixteen, additions, borrows and
remainders below three then fit without signed arithmetic. Allocation-size
addition is checked before constructing product buffers.

This first prototype owns temporary vectors rather than a reusable Toom
workspace. Their allocator is included in the existing `digits` diagnostic
category, and `toom3_multiply_calls` counts selected recursive nodes. The
diagnostic report also records `toom3_threshold`. Timing
builds compile those counters out. Allocation and peak memory may offset the
reduced product count; do not infer a speed or RSS improvement from the
asymptotic exponent. No default adoption or accepted performance claim follows
from this prototype.

Use `scripts/build-gd-bc.py --toom3-threshold N` with an immutable revision and
the usual explicit `--split-point`. The recipe records the threshold and
requires its actual definition in both the VM and executable compilation
commands. A cache variable ignored by an old revision cannot establish an
experimental build. The zero default permits historical revisions without this
option. Compilation-command evidence is required for opt-in builds; a generator
without that evidence reports an error rather than inventing a selected tier.

The generic kernel tests cover 8/16/32-bit decimal traits, independent decimal
convolution, dense/random/sparse/zero/padded values, all -1 sign combinations,
zero evaluations, recursive carries, squares, asymmetry and output canaries.
The binary small-radix fixture checks the generic fallback. The ordinary number
suite must also pass with a forced low threshold to exercise sign, scale,
truncation and alias behavior through the wrapper. `test-toom-oracle.py` checks
exact integer products and operand snapshots; `--large` extends it to 100,000
and 200,003 decimal digits. Python integer expectations are independent of the
implementation's limb split and interpolation.

Before accepting a tier, measure independent full and held-out balanced,
asymmetric and square workloads, including small values, decimal conversion,
changing scales, startup and peak scratch/RSS. Use fixed per-experiment
thresholds, identical ordinary math source where applicable, and the native
Linux/Windows/macOS comparison policy. Retain losing and inconclusive cases.
Wider decimal/binary limbs, NTT/FFT, GMP and SIMD remain separate #204
experiments; this prototype does not stand in for those comparisons.

## Explicit native checkpoint

`bc-toom3-experiment.yml` runs a separate advisory native experiment on matching
PRs, relevant main pushes and manual dispatch. It records the exact PR base/head
(or the documented fixed accepted main anchor), uses the existing full/held-out
arithmetic corpus and selects threshold 512 only for the candidate. Baseline
and independent A/A builds keep their default tier. There is no scheduled
experimental default and no threshold tuning from the paired outcomes.

The shared controller's `--candidate-toom3-threshold` is zero by default, so the
ordinary native workflow keeps its existing treatment. Opt-in runs first finish
all builds and the candidate's driver, exact rational and 50 large-product/alias
checks, then begin paired elapsed children. Failure in those checks is a hard
failure. The decision report records the requested threshold, each actual recipe
verifies compiled selection, and the existing post-timing executable collector
preserves the measured binaries. Controls, bounded confirmations and per-class
losses retain the ordinary policy; a green advisory job cannot establish a win.
Native elapsed corpus sizes remain the historical arithmetic sizes. Separate
100k/200k checks establish correctness only, not large-size speed or RSS.

Aliased coefficient squares retain the specialized square leaf at every point,
including recursive Toom levels. The original input ranges must share identity
and extent before an evaluated range can serve as both operands. Equal values
in distinct ranges do not trigger that shortcut. The larger shared-square
profiling guard exposed 54 ordinary-product leaves in the first prototype;
reusing the evaluated range for both point operands removes that regression.
The separate number sign and COW snapshots retain their wrapper semantics.
