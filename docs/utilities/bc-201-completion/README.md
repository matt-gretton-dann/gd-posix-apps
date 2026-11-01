# Arithmetic ownership and aligned loops — #201 completion review

The implementation is spread across independently measured, merged stages:

| Decision | Implementation and evidence |
| --- | --- |
| Borrow storage without per-limb ownership changes | [Borrowed-storage report](../bc-201-borrowed-storage/README.md), PR #309. Iterator-owner-copy events become zero; exact COW payload bytes are retained in the diagnostic provenance. |
| Resize once and specialize equal-scale add/subtract | [Aligned-arithmetic report](../bc-201-aligned-arithmetic/README.md), PR #310. Carry/borrow tails stop when no further propagation is needed; differing scales retain the generic path. |
| Compare most-significant unequal limbs first | [Reverse-comparison report](../bc-201-reverse-comparison/README.md), PR #312. Zero extension preserves padded representations; equal/low/high-unequal comparisons remain separate measurements. |
| Evaluate zero/length normalization assumptions | Tests and source comments establish that range copies, fractional splits and `mac(0,0)` can retain padding/nonempty zero storage. `tidy()` is explicit, not universal. Constant-time empty/final-limb shortcuts are therefore not selected. |
| Capture stable views and query COW ownership once | Accepted [inline-storage work](../bc-inline-storage.md), PR #314. Inline values and heap COW retain their independent lifetime rules. |
| Inspect compiler lowering before reciprocal/SIMD work | Actual MAC-template assembly below already uses multiplication/shifts for constant decimal division. No handwritten reciprocal or ISA-specific replacement is justified by this evidence. |

The first ownership stage reduces long Fibonacci to 0.416/0.404 of its parent
on full/held-out dimensions and short Fibonacci to 0.793/0.777. Aligned loops
separately reduce long Fibonacci to 0.826/0.814 of that stage's parent. Reverse
comparison isolates high/low/equal effects without folding in aligned loops.
Those reports preserve their exact parent/candidate identities and all classes.

Related accepted storage work supplies the loop/small-value acceptance that
ownership alone did not achieve: full/held loop ratios 0.649/0.646, short
Fibonacci 0.746/0.752, long Fibonacci 0.846/0.854 and small multiply 0.805/0.856
against the workspace parent. Its [complete report](../bc-202-inline-storage/README.md)
retains the wider Number object and losing/inconclusive observations. These are
separate experiments, not factors to multiply into a synthetic current speedup.
Later literal caching has its own report and attribution.

## Compiler evidence

Reviewed source: `a5c1cb1ad4051616afdce115b02ecc0247ad62f7`. The probe instantiates
actual `BasicDigits::mac` for NumberTraits8/16/32, using the immutable recipe's
Apple Clang 21/libc++ header 220106 context with reviewed source headers first.
The manifest records the full command, compiler/hash, source and assembly hashes.
Thin LTO is removed only to emit readable translation-unit target assembly.
This is **not final LTO-binary disassembly**, a timing experiment, or evidence
about another compiler/architecture.

In the emitted ARM64 loops, the 8-bit trait divides by 100 through multiply/shift
and reconstructs the remainder with `msub`. The 16-bit trait similarly uses a
widening multiply/shift for radix 10000. The native radix 1000000000 loop uses
`umull`, shifts, `umulh` and `msub`; pointer/extent selection occurs before the
limb loop. There are no `udiv` instructions in the emitted probe. COW setup and
carry-draining growth remain separate paths; this does not claim that ownership
or allocation is free everywhere.

The raw 4,043-line assembly, source and replay helper are preserved rather than
replacing compiler division with an unmeasured reciprocal implementation.
Other targets keep the same portable C++ and their compiler's ordinary lowering.
Normal builds compile the new site counters away; numeric behavior and default
dispatch policy are unchanged. Profiling builds expose logical comparison, zero
and length scan positions so future measurements can retain this distinction.

## Matched logical scan counts

Historical parent `2ddf749e8b313f620ad248d680e801c409bbd3f5` and isolated reverse-
comparison candidate `6cf81f0b6bd84a6acccec1b81bc92e04089b4857` are rebuilt with
the same diagnostic-only sites. The archived patches, compiled-file hashes,
compiler/library metadata, commands and profiles identify these as **commit plus
counter patch**, not unmodified commit binaries. No instrumented executable is
timed. Each case repeats 100 comparisons of 1000-digit values, then prints the
result and both decimal lengths. All six runs pass exact oracles and release all
selected storage at EOF.

| Shape | Comparison positions before / after | Zero-scan positions before / after | Length positions before / after |
| --- | --- | --- | --- |
| Most-significant unequal | 11,200 / 100 | 305 / 305 | 2 / 2 |
| Least-significant unequal | 11,200 / 11,200 | 305 / 305 | 2 / 2 |
| Equal values | 11,200 / 11,200 | 204 / 204 | 2 / 2 |

Iterator-owner-copy events stay zero in both sources because this comparison
isolates the later traversal change from the already accepted borrowing step.
Counters describe logical aligned positions or predicate visits, not physical
loads, vector instructions or cycle attribution. Runtime instrumentation can
change optimization, so the separate immutable timing reports remain primary
performance evidence.

Current profile tests verify high-unequal early exit and complete low/equal
traversal. Three exact trait tests additionally verify zero scanning through four
limbs and length scanning of a significant top limb versus four retained zero
limbs. All nine profiling CLI checks and all 24 assertions in these three cases
pass. Ordinary Release still passes 168 bc cases (584,040 assertions) and eight
CLI groups with increments compiled out.

## Correctness and remaining scope

The staged tests cover full carry/borrow chains, cancellation, signs/signed zero,
uneven and partial-limb scales, self-aliases and preserved copies at all three
limb widths. Independent rational/decimal oracles and extensions-disabled
sanitizers accompany the accepted patches; source-specific test counts and raw
performance limitations remain in their reports. Fresh accepted-main validation
is recorded with this completion PR.

This completes #201's bounded ownership/aligned-arithmetic decisions. It does
not close #197's competitive target, #202's remaining lifetime/ownership policy
work, #203's dispatch/tuning experiments, or the other VM/conversion stages.
Canonicalizing every producer and handwritten reciprocal/SIMD kernels are not
silently assumed necessary or safe. No universal performance claim is made.
