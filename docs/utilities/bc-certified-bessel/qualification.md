# Certified Bessel prospective qualification

Refs #217, #200 and #197. Base is accepted main
`3beebb1461ac7549832707f9b84e547127e3c14a`. The numerical implementation is
byte-identical to correctness-tested `ca75c979`; its exact publication revision
and native checkout inputs are retained by the workflow. This is a different
algorithm from rejected PR #712. All eighteen original #712 math archives,
including its regressions, were retained and reviewed before this declaration.
No old timings, hosts, intervals or qualification transfer to this candidate.
PR #713's function-storage experiment is independent and is not integrated.

## Fixed scope before measurement

Run this workflow once, attempt one, on standard `ubuntu-24.04`, `macos-15`
and `windows-2025` runners. Do not restart incomplete or unfavorable cohorts,
replace hosts, pool revisions or select favorable subsets. Retain every original
success, failure, timeout, missing observation and upload limitation. A failed
measurement family is unqualified, even when its numerical tests pass. All work,
including queues and review, consumes the existing 864,000-second total goal cap.

The mathematical matrix has 21 fixed groups per platform:

- All 188 previously exposed program/oracle records from #712 are controls.
  Their original unmeasured bytes, hashes, dimensions and artifact identities
  are recorded in `math-qualification-exposed-inputs.json`. Both original full
  and holdout families are now exposed controls. Older parameter descriptions
  remain byte-preserved historical metadata, not claims of fresh validation.
- Each preset is divided into low (S<=20), middle (20<S<=100), and high (S>100)
  bands. Sort the unique `(group,function,arguments,scale)` keys within each
  band and assign consecutive indices modulo three to three shards. Keep each
  one-shot/repeated pair together. Automated union/disjointness and identity
  checks prove no original input is dropped or replaced. This produces eighteen
  control groups, with five or six distinct workloads each.
- Fresh validation contains eighteen additional workloads, each in one-shot
  and repeated mode. The RNG seed is the integer value of frozen numerical
  revision `ca75c979cc3eba5ddacd01503da05ac9c4030937` XOR 217714, independent
  of the later control publication revision. For each order zero/one, draw
  independent arguments with integer part in [1,3] and fractional digits for
  `(scale,digits)` = `(0,6)`, `(1,256)`, `(20,6)`, `(20,256)`, `(100,6)`,
  `(100,256)`, `(1000,6)`, `(1000,64)`. Add order-two controls at scales 20
  and 100 with six fractional digits. Exact forward rational series plus a
  geometric omitted-tail bound give independent outward decimal enclosures;
  no implementation supplies the oracle. One fresh group per scale band uses
  the harness's holdout namespace. Fresh validation is distinct from exposed
  historical holdout controls. It includes S0 order one, absent from #712.

Each math group independently builds candidate, candidate A/A, base, pinned
Howard 7.0.3 and matched diagnostic profiles. Ordinary platform defaults remain
unchanged, including Windows source loading and disabled native scalar/function
activation. Split point remains 126. Correctness completes before calibration.
Use the existing per-role median doubling and common maximum count, three
checked calibration trials per count, and count cap 1,048,576. Prospectively set
the calibration target to 0.5 seconds and the required repeated-measurement
duration floor to 0.25 seconds. Both are recorded separately. This margin is
chosen before observations; later duration shortfall remains inconclusive and
never triggers recalibration. One-shot latency remains unbatched and excluded
from substantive throughput claims.

Use 31 paired rounds, three warmups, existing fixed seeds, output checks and
the existing prespecified A/A and regression confirmation stages. Retain raw
per-role trials, frozen program/oracle bytes, preparation declarations,
same-job pairs, paired IID/circular-block interval union and confirmations.
Every math controller has a 600-second child limit, 20,000-second total limit
and 360-minute workflow bound. No candidate threshold or runtime recognition
depends on workload names, seeds or benchmark arguments.

## Arithmetic, startup and resources

Three additional platform jobs run fresh full/holdout arithmetic at 31 samples,
including the existing separate sparse repeated-duration qualification. Keep
the existing A/A, confirmation and Howard comparisons; retain actual executable
bytes and recipes. Build independent candidate/base profiles before elapsed
work. Collect matched counters and five serial native RSS observations after
all arithmetic elapsed stages, retaining availability and path-policy limits.

After arithmetic/resource collection in each such job, run the unchanged fixed
startup-calibration controller using those exact base, independently rebuilt
base A/A, and candidate images. Retain every null block and candidate stage,
the existing 2,400-second total and five-second child bounds, measured clock
identity and source policy hash. Startup requires its existing qualified
empirical calibration and candidate acceptance; Howard margins cannot waive
it. If earlier stages fail, missing startup observations remain unqualified.
No local production elapsed benchmark or CPU sampling is used.

Math resources also use separately built matched candidate/base profiles,
exact one-shot declarations and the existing representative controls. Merge
duplicate names only when program/oracle bytes agree. Instrumented event and
allocation counters are not ordinary elapsed observations; profile fallbacks
are explicitly retained. Native RSS runs serially after elapsed stages, five
observations per role, with adapters and unavailable fields explicit. Retain
both actual profile images separately from release images. Do not infer
unmeasured process memory from compiler object sizes or software counters.

## Acceptance and retention

Apply the unchanged approved per-class rules: substantive GD/Howard median
at most 0.90 with paired 95% upper bound below 1; usable A/A controls, required
sample counts and duration; Linux/Windows regression cap 5%, macOS cap 0%.
A regression beyond the platform cap can qualify only when candidate/primary
Howard 95% upper bound is at most 0.90, all controls pass and a meaningful gain
elsewhere is established. No uncertainty waiver, class averaging, startup
exception or post-observation selection is allowed. Resource regressions and
missing/incomparable evidence remain explicit review constraints. Green CI
alone does not establish performance acceptance or authorize adoption.

The 66 original matrix jobs retain actual source checkout bytes, canonical
prospective input hashes, declarations, role identities, raw samples,
ordinary/profile images, resources, logs and failed partial outputs. The
source pin manifest deliberately excludes itself to avoid a circular hash.
Review all originals before any source change, replacement experiment, merge
or adoption decision. Keep the branch and PR draft while unqualified. Maintain
the 20 GiB local free-space floor and preserve active work and unique evidence.
