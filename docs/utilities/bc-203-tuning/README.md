# Stable default and finite tuning sweeps

The default changes from build-time auto calibration to the stable portable
126-limb setting already used by the three-OS native matrix. Explicit numeric
overrides and `auto` calibration remain available; cross `auto` retains its
128-limb fallback. This work changes build policy and the tuner, not arithmetic
kernels. No universal optimum or competitive-target achievement is claimed.

The portable configuration test is committed failing first in 64d875a, then
fixed in 05c07fa. Tuner source `2e7e84ba7f3bbb91ac4507d913f7d8d6f4b74be4`
replaces directional search with a finite shuffled sweep and paired medians.
The [protocol](../bc-benchmarking.md#explicit-multiplication-calibration)
defines shape weighting, batch bounds, warm-up and the 1% stability tolerance.

## Original process-CPU-clock stage

On Apple M1/macOS 27.2 with the compiler/build identity in provenance.json,
three independent seeds each pass all 1440 timed result comparisons across
36 shapes, eight points and five rounds. All select 32 on this host; their
aggregate ratios are about 0.962–0.967 versus the portable point. This does
not change the default to 32: a finite corpus, one host and CPU clock samples
do not establish a portable boundary. No build/test/profile overlaps timing;
frequency, thermals and unrelated shared-host load remain uncontrolled.

Every raw observation and generated header remains in repeated/. summary.json
retains every shape and every candidate, including losses. The table below
shows the selected point against 126 per shape for all three seeds, so an
aggregate does not hide unfavorable cases. These medians have no confidence
interval and are not native performance acceptance results.

| Left limbs | Right limbs | Square | Seed 197203 | Seed 197204 | Seed 197205 |
| ---: | ---: | --- | ---: | ---: | ---: |
| 1 | 1 | False | 1.000 | 1.000 | 1.000 |
| 1 | 2 | False | 1.000 | 1.000 | 1.000 |
| 1 | 8 | False | 1.000 | 1.000 | 1.000 |
| 1 | 1 | True | 1.125 | 0.857 | 1.000 |
| 2 | 2 | False | 1.000 | 1.000 | 1.000 |
| 2 | 4 | False | 1.000 | 1.059 | 1.000 |
| 2 | 16 | False | 1.000 | 1.000 | 1.000 |
| 2 | 2 | True | 1.000 | 1.000 | 0.923 |
| 10 | 10 | False | 1.000 | 1.037 | 1.000 |
| 10 | 20 | False | 1.000 | 1.000 | 1.023 |
| 10 | 80 | False | 1.007 | 1.008 | 1.000 |
| 10 | 10 | True | 1.000 | 0.982 | 1.019 |
| 12 | 12 | False | 1.000 | 1.000 | 1.000 |
| 12 | 24 | False | 1.034 | 1.000 | 0.982 |
| 12 | 96 | False | 1.012 | 1.000 | 1.000 |
| 12 | 12 | True | 1.000 | 1.014 | 1.014 |
| 112 | 112 | False | 0.849 | 0.888 | 0.886 |
| 112 | 224 | False | 1.004 | 1.005 | 1.006 |
| 112 | 896 | False | 1.003 | 1.003 | 1.003 |
| 112 | 112 | True | 0.627 | 0.603 | 0.650 |
| 128 | 128 | False | 0.962 | 0.991 | 0.991 |
| 128 | 256 | False | 1.001 | 0.999 | 1.002 |
| 128 | 1024 | False | 1.000 | 1.000 | 0.994 |
| 128 | 128 | True | 0.800 | 0.784 | 0.796 |
| 256 | 256 | False | 0.970 | 0.989 | 0.990 |
| 256 | 512 | False | 1.000 | 1.000 | 1.000 |
| 256 | 2048 | False | 0.997 | 0.999 | 1.000 |
| 256 | 256 | True | 0.820 | 0.809 | 0.813 |
| 512 | 512 | False | 1.002 | 0.990 | 0.991 |
| 512 | 1024 | False | 1.000 | 1.000 | 1.003 |
| 512 | 4096 | False | 1.000 | 1.000 | 1.002 |
| 512 | 512 | True | 0.819 | 0.825 | 0.822 |
| 1112 | 1112 | False | 1.102 | 1.100 | 1.102 |
| 1112 | 2224 | False | 0.998 | 1.001 | 1.000 |
| 1112 | 8896 | False | 0.999 | 0.998 | 0.999 |
| 1112 | 1112 | True | 0.867 | 0.866 | 0.870 |

Validation: five configuration groups (including native/cross portable
selection), 176 Release cases / 592102 assertions, deterministic median and
nonmonotonic-selection tests, and five tuner CLI groups. The quick CLI test
checks all 288 samples and every candidate/shape/round plus header seed.
No actual foreign cross executable was run locally; native CI remains required.

## Portable elapsed-clock correction

Windows CI 41285156338 rejects zero process-CPU intervals in both extension
configurations. The failing quick-sweep excerpt remains in
windows-clock-failure.log; no sample is reinterpreted as a speedup. Source
`027bffd107580791b4bc8101222a78d8e203ce1d` switches the tuner to the portable
steady monotonic elapsed clock and labels that metric in stdout/CSV. The
unchanged quick protocol still requires positive durations and every checked
candidate/shape/round. Scheduling now belongs to the measured interval. The
original CPU-clock data above remains unchanged with its original source.

Three fresh full runs retain all 4320 exact-checked elapsed samples and every
candidate/shape in steady-repeated/ and steady-summary.json. Their actual source,
binary/compiler identity and limitations are in steady-provenance.json. These
runs do not change the portable 126 default. Windows confirmation was pending at collection time. The subsequent
[accepted native run](accepted-native/README.md) records both Windows extension
jobs passing and complete three-OS evidence.

| Seed | Selected point | Aggregate / 126 |
| ---: | ---: | ---: |
| 197203 | 32 | 0.965406 |
| 197204 | 32 | 0.963511 |
| 197205 | 32 | 0.965960 |
