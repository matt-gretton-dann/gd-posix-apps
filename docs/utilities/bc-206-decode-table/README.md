# Indexed nondecimal decoder: initial held candidate

Refs #206 and #197. Exact baseline 9669ef73bc00e5b89f3fb00e2867a8f8ed9c4d32
and candidate dbcdcad7fc68e80da10cae1757f3e022279b2635. Recipes retain all actual
compiler/library identities, flags, commands and executable hashes. Later
integration with accepted main does not replace these measured source identities.

A 256-entry immutable table replaces repeated alphabet searches in the raw
nondecimal decoder on eight-bit character platforms. Index through unsigned
char; wider character models retain the original generic alphabet search.
The exhaustive 256-byte oracle retains every raw search result, including
the terminator index, independently of arithmetic value tests. Existing base
assertions, single-digit rules, point and invalid-character paths stay in place.
The measured decimal and single-decimal-digit arithmetic paths are unchanged.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, fixed split 126,
extensions ON, profiling/coverage OFF. Full 2160 and held-out 460 invocations
pass exact Python integer/decimal/length checksum, status, stderr and deadline
checks. One checked warmup and nine shuffled paired rounds; 2000 bootstrap
resamples. No local builds/tests/profiles overlap timing. Shared-host scheduling,
frequency and thermals remain uncontrolled. Controls are separate, not subtracted.

The candidate improves short/modest nondecimal conversion in measured regimes:
binary 16/64/256 characters is about 0.726/0.676/0.672; hexadecimal 16/64
characters is about 0.736/0.757. At 100000 characters growing arithmetic
dominates and binary/hex ratios are near 0.983/0.996.

**Hold this source:** held-out decimal 3-character input is 1.106
[1.083,1.132], a measured local regression despite no source change to the
decimal fast path. Do not dismiss it as noise or claim a proven layout cause.
No full-set interval has its lower endpoint above 1.05. The complete losing
and inconclusive intervals remain below and in raw samples. Any integrated
candidate needs fresh exact-source comparisons and native acceptance.

Release passes 194 cases/691695 assertions and eight CLI groups. Extensions-
OFF ASan/UBSan passes four radix cases/36256 assertions, fifteen VM CLI
and seven component groups. These logs belong to this initial source.

## full

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-1d-x100000` | 1.000 [1.000,1.004] |
| `input-b2-16d-x10000` | 0.726 [0.723,0.729] |
| `input-b2-64d-x5000` | 0.676 [0.674,0.678] |
| `input-b2-256d-x1000` | 0.672 [0.667,0.677] |
| `input-b2-1024d-x20` | 0.741 [0.732,0.755] |
| `input-b2-10000d-x2` | 0.895 [0.886,0.905] |
| `input-b2-100000d-x1` | 0.983 [0.981,0.985] |
| `input-b3-1d-x100000` | 1.002 [0.997,1.010] |
| `input-b3-16d-x10000` | 0.731 [0.727,0.731] |
| `input-b3-64d-x5000` | 0.710 [0.707,0.718] |
| `input-b3-256d-x1000` | 0.660 [0.657,0.662] |
| `input-b3-1024d-x20` | 0.717 [0.708,0.733] |
| `input-b3-10000d-x2` | 0.916 [0.907,0.916] |
| `input-b3-100000d-x1` | 0.985 [0.981,0.986] |
| `input-b4-1d-x100000` | 1.000 [0.998,1.002] |
| `input-b4-16d-x10000` | 0.731 [0.728,0.744] |
| `input-b4-64d-x5000` | 0.724 [0.722,0.743] |
| `input-b4-256d-x1000` | 0.703 [0.678,0.722] |
| `input-b4-1024d-x20` | 0.761 [0.743,0.766] |
| `input-b4-10000d-x2` | 0.943 [0.942,0.947] |
| `input-b4-100000d-x1` | 0.994 [0.992,0.994] |
| `input-b5-1d-x100000` | 1.000 [1.000,1.007] |
| `input-b5-16d-x10000` | 0.765 [0.762,0.766] |
| `input-b5-64d-x5000` | 0.744 [0.740,0.750] |
| `input-b5-256d-x1000` | 0.730 [0.718,0.733] |
| `input-b5-1024d-x20` | 0.774 [0.769,0.785] |
| `input-b5-10000d-x2` | 0.951 [0.948,0.957] |
| `input-b5-100000d-x1` | 0.995 [0.994,0.997] |
| `input-b6-1d-x100000` | 1.000 [0.998,1.004] |
| `input-b6-16d-x10000` | 0.737 [0.733,0.740] |
| `input-b6-64d-x5000` | 0.745 [0.737,0.752] |
| `input-b6-256d-x1000` | 0.812 [0.803,0.823] |
| `input-b6-1024d-x20` | 0.793 [0.786,0.797] |
| `input-b6-10000d-x2` | 0.943 [0.943,0.946] |
| `input-b6-100000d-x1` | 0.993 [0.986,0.993] |
| `input-b7-1d-x100000` | 1.000 [0.998,1.002] |
| `input-b7-16d-x10000` | 0.734 [0.716,0.818] |
| `input-b7-64d-x5000` | 0.745 [0.744,0.756] |
| `input-b7-256d-x1000` | 0.849 [0.834,0.851] |
| `input-b7-1024d-x20` | 0.800 [0.779,0.824] |
| `input-b7-10000d-x2` | 0.939 [0.937,0.942] |
| `input-b7-100000d-x1` | 0.991 [0.989,1.015] |
| `input-b8-1d-x100000` | 1.000 [1.000,1.004] |
| `input-b8-16d-x10000` | 0.721 [0.719,0.726] |
| `input-b8-64d-x5000` | 0.733 [0.729,0.738] |
| `input-b8-256d-x1000` | 0.829 [0.813,0.841] |
| `input-b8-1024d-x20` | 0.756 [0.750,0.772] |
| `input-b8-10000d-x2` | 0.932 [0.929,0.934] |
| `input-b8-100000d-x1` | 0.990 [0.989,0.991] |
| `input-b9-1d-x100000` | 1.000 [0.983,1.002] |
| `input-b9-16d-x10000` | 0.729 [0.724,0.752] |
| `input-b9-64d-x5000` | 0.751 [0.743,0.781] |
| `input-b9-256d-x1000` | 0.876 [0.857,0.883] |
| `input-b9-1024d-x20` | 0.788 [0.783,0.793] |
| `input-b9-10000d-x2` | 0.936 [0.932,0.940] |
| `input-b9-100000d-x1` | 0.992 [0.989,0.993] |
| `input-b10-1d-x100000` | 1.000 [1.000,1.047] |
| `input-b10-16d-x10000` | 0.991 [0.986,0.996] |
| `input-b10-64d-x5000` | 1.002 [0.999,1.010] |
| `input-b10-256d-x1000` | 1.006 [1.002,1.011] |
| `input-b10-1024d-x20` | 1.009 [0.999,1.044] |
| `input-b10-10000d-x2` | 1.013 [0.999,1.056] |
| `input-b10-100000d-x1` | 1.000 [0.997,1.012] |
| `input-b11-1d-x100000` | 0.923 [0.923,0.928] |
| `input-b11-16d-x10000` | 0.729 [0.726,0.735] |
| `input-b11-64d-x5000` | 0.737 [0.733,0.746] |
| `input-b11-256d-x1000` | 0.850 [0.829,0.861] |
| `input-b11-1024d-x20` | 0.785 [0.772,0.799] |
| `input-b11-10000d-x2` | 0.939 [0.937,0.941] |
| `input-b11-100000d-x1` | 0.992 [0.974,1.002] |
| `input-b12-1d-x100000` | 0.922 [0.920,0.923] |
| `input-b12-16d-x10000` | 0.717 [0.708,0.723] |
| `input-b12-64d-x5000` | 0.731 [0.729,0.740] |
| `input-b12-256d-x1000` | 0.896 [0.887,0.913] |
| `input-b12-1024d-x20` | 0.809 [0.797,0.829] |
| `input-b12-10000d-x2` | 0.955 [0.951,0.961] |
| `input-b12-100000d-x1` | 0.995 [0.991,1.015] |
| `input-b13-1d-x100000` | 0.923 [0.921,0.923] |
| `input-b13-16d-x10000` | 0.731 [0.721,0.733] |
| `input-b13-64d-x5000` | 0.747 [0.711,0.755] |
| `input-b13-256d-x1000` | 0.887 [0.880,0.898] |
| `input-b13-1024d-x20` | 0.810 [0.808,0.816] |
| `input-b13-10000d-x2` | 0.954 [0.951,0.955] |
| `input-b13-100000d-x1` | 0.993 [0.993,0.997] |
| `input-b14-1d-x100000` | 0.923 [0.921,0.930] |
| `input-b14-16d-x10000` | 0.724 [0.717,0.730] |
| `input-b14-64d-x5000` | 0.739 [0.705,0.744] |
| `input-b14-256d-x1000` | 0.888 [0.864,0.898] |
| `input-b14-1024d-x20` | 0.822 [0.814,0.829] |
| `input-b14-10000d-x2` | 0.957 [0.956,0.960] |
| `input-b14-100000d-x1` | 0.995 [0.992,1.003] |
| `input-b15-1d-x100000` | 0.923 [0.916,0.923] |
| `input-b15-16d-x10000` | 0.718 [0.714,0.723] |
| `input-b15-64d-x5000` | 0.748 [0.747,0.757] |
| `input-b15-256d-x1000` | 0.873 [0.866,0.890] |
| `input-b15-1024d-x20` | 0.822 [0.806,0.831] |
| `input-b15-10000d-x2` | 0.960 [0.958,0.963] |
| `input-b15-100000d-x1` | 0.993 [0.991,1.038] |
| `input-b16-1d-x100000` | 0.923 [0.923,0.924] |
| `input-b16-16d-x10000` | 0.736 [0.733,0.743] |
| `input-b16-64d-x5000` | 0.757 [0.752,0.771] |
| `input-b16-256d-x1000` | 0.922 [0.918,0.929] |
| `input-b16-1024d-x20` | 0.857 [0.851,0.861] |
| `input-b16-10000d-x2` | 0.965 [0.964,0.966] |
| `input-b16-100000d-x1` | 0.996 [0.992,0.998] |
| `control-1d` | 1.000 [0.995,1.001] |
| `control-1000d` | 0.994 [0.965,1.003] |
| `control-10000d` | 0.973 [0.946,1.000] |

## holdout

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-3d-x10000` | 0.904 [0.901,0.908] |
| `input-b2-257d-x1000` | 0.676 [0.669,0.689] |
| `input-b2-2048d-x20` | 0.776 [0.742,0.791] |
| `input-b2-20000d-x1` | 0.932 [0.920,0.936] |
| `input-b2-120000d-x1` | 0.986 [0.984,0.991] |
| `input-b8-3d-x10000` | 0.883 [0.883,0.901] |
| `input-b8-257d-x1000` | 0.819 [0.811,0.825] |
| `input-b8-2048d-x20` | 0.813 [0.807,0.820] |
| `input-b8-20000d-x1` | 0.959 [0.956,0.964] |
| `input-b8-120000d-x1` | 0.992 [0.990,0.995] |
| `input-b10-3d-x10000` | 1.106 [1.083,1.132] |
| `input-b10-257d-x1000` | 0.999 [0.995,1.012] |
| `input-b10-2048d-x20` | 1.017 [1.002,1.025] |
| `input-b10-20000d-x1` | 1.000 [0.990,1.004] |
| `input-b10-120000d-x1` | 1.003 [0.989,1.031] |
| `input-b16-3d-x10000` | 0.891 [0.888,0.895] |
| `input-b16-257d-x1000` | 0.919 [0.891,0.927] |
| `input-b16-2048d-x20` | 0.894 [0.890,0.905] |
| `input-b16-20000d-x1` | 0.981 [0.977,0.983] |
| `input-b16-120000d-x1` | 0.996 [0.867,0.999] |
| `control-1d` | 0.999 [0.979,1.004] |
| `control-1000d` | 1.003 [1.000,1.041] |
| `control-10000d` | 0.999 [0.895,1.029] |
