# Exact coefficient-prefix square roots

Issue #212 evaluates significant-digit precision growth without changing the
builtin square root's decimal floor contract. The original integer-coefficient
Newton path remains the generic small-root fallback. The initial candidate
selects staging at 128 root-coefficient digits; that crossover is experimental,
not an adopted performance result.

For input coefficient A and scale S, result scale is T=max(requested,S). Keep
the existing working scale W (T plus optional guards, without overflow), and
form the exact integer N=A*10^(2W-S) using separate W-S and W shifts. Internal
coefficient widths and prefix offsets use the storage index domain rather than
the narrower public length result. Oversized shifts are split into representable
chunks. Existing zero, negative-input and resource/error paths remain intact.

For q>=0, let Nq=floor(N/10^(2q)). Then
`floor(sqrt(Nq)) = floor(sqrt(N)/10^q)`: both sides select the unique integer r
with `r*r*10^(2q) <= N < (r+1)*(r+1)*10^(2q)`. Thus a short prefix root is
exact, without a floating-point estimate or an assumed number of guard digits.
Start with a positive power-of-ten seed, then lift each exact prefix root when
extending precision. The integer Newton first-step bound and subsequent strict
decrease above the floor retain the existing stop-on-nondecrease rule, including
adjacent-integer cycles. At q=0 the result is the exact integer floor sqrt(N).
Truncating W-T digits finally gives the exact decimal floor at T.

Plan stages backward from the final coefficient width by ceiling-halves, then
execute them forward. The first stage has at most eight digits; every following
extension is at most twice its predecessor. A fixed stack array bounded by the
storage-index bit width holds the plan, without an extra heap allocation. This
avoids a penultimate stage that is almost as large as the final one.
Exact-prefix computation still copies/truncates a
radicand per stage; allocation traffic, retained capacity and process RSS must be
measured. In particular, exact powers and near-square inputs may already have
an excellent old seed and must not lose their fast path merely to exercise
staging. Compare the original coefficient A with the square of the old
power-of-ten seed, expressed as 10^e in that same coefficient domain. When
the residual is zero or its decimal width establishes at least half the
required root precision, preserve the original Newton path. This is a work
selection rule, never a returned approximation or a different stopping test;
both paths retain exact floor convergence. The legacy fallback also requires
its original input-width domain. Development counters retain both favorable
and unfavorable cases separately from elapsed acceptance evidence.

`sqrt_iterations` counts every actual Newton request in either path.
`sqrt_working_scale_sum` retains the fractional scale of those requests (zero
for integer-prefix iterations). `sqrt_precision_stages` counts exact prefix
stages, and `sqrt_root_digits_sum` sums their root-coefficient widths once per
Newton iteration. `sqrt_precise_seed_fallbacks` counts large roots routed back
to the original path by the exact-residual selector. Division request/operand
dimensions and allocator counters
remain independently recorded. These are logical work and storage diagnostics,
not CPU time or RSS.

Validation retains NumberTraits8 precision/guard-boundary tests, focused wider
integer-domain checks, independent Python integer-root inequalities, snapshots,
adjacent squares, tiny fractions and large requested scales. The interpreted
math library is also checked as ordinary source after changing builtin sqrt.
Native paired full/held-out evidence is required before adoption; no speedup or
completed issue is claimed by the initial correct prototype.
