# Integrated decode-table component comparisons

Refs #206 and #197. Exact baseline dd8c60c274511e26ed453eb1d8e175936ea6f576
and candidate 81fae3adce980e32e07dd8860b456bb7bb01071d. Both include accepted large-input
runtime conversion. Complete actual flags, compiler/library/source identities,
commands and binary hashes remain in recipes. Earlier source results, including
the held decimal microcase, retain their original identities and are not replaced.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, fixed split 126,
extensions ON, profiling/coverage OFF. One checked warmup and nine shuffled
paired rounds, separate full/held-out seeds, 2000 bootstrap resamples. All
exact integer/decimal/continuation-length checksum and process checks pass.
No local builds/tests/profiles overlap timing. Host scheduling, frequency
and thermals remain uncontrolled. Controls stay separate and are not subtracted.

All 2620 invocations pass; no interval lower endpoint exceeds 1.05. The
held-out three-character decimal case is now 0.989 of this changed baseline.
The original dbcdcad loss (1.106) remains held in the parent archive; this
new source and baseline do not prove its cause. Binary 16/64 characters is
about 0.711/0.673, hex 16/64 about 0.745/0.745. Production DC leaf decoding
also benefits in the measured large-input regimes. Native acceptance and
the final combined formatter/decoder source still require their own evaluation.

## full

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-1d-x100000` | 1.002 [1.000,1.004] |
| `input-b2-16d-x10000` | 0.711 [0.709,0.717] |
| `input-b2-64d-x5000` | 0.673 [0.666,0.675] |
| `input-b2-256d-x1000` | 0.666 [0.658,0.687] |
| `input-b2-1024d-x20` | 0.736 [0.732,0.739] |
| `input-b2-10000d-x2` | 0.900 [0.898,0.901] |
| `input-b2-100000d-x1` | 0.939 [0.934,0.942] |
| `input-b3-1d-x100000` | 1.000 [0.997,1.004] |
| `input-b3-16d-x10000` | 0.713 [0.711,0.714] |
| `input-b3-64d-x5000` | 0.691 [0.686,0.710] |
| `input-b3-256d-x1000` | 0.655 [0.651,0.658] |
| `input-b3-1024d-x20` | 0.709 [0.704,0.726] |
| `input-b3-10000d-x2` | 0.919 [0.918,0.919] |
| `input-b3-100000d-x1` | 0.965 [0.958,0.969] |
| `input-b4-1d-x100000` | 0.999 [0.994,1.002] |
| `input-b4-16d-x10000` | 0.728 [0.723,0.732] |
| `input-b4-64d-x5000` | 0.702 [0.701,0.712] |
| `input-b4-256d-x1000` | 0.692 [0.678,0.713] |
| `input-b4-1024d-x20` | 0.752 [0.749,0.755] |
| `input-b4-10000d-x2` | 0.940 [0.937,0.947] |
| `input-b4-100000d-x1` | 0.978 [0.977,0.983] |
| `input-b5-1d-x100000` | 1.000 [1.000,1.008] |
| `input-b5-16d-x10000` | 0.774 [0.770,0.781] |
| `input-b5-64d-x5000` | 0.729 [0.725,0.746] |
| `input-b5-256d-x1000` | 0.728 [0.722,0.741] |
| `input-b5-1024d-x20` | 0.770 [0.757,0.783] |
| `input-b5-10000d-x2` | 0.949 [0.946,0.951] |
| `input-b5-100000d-x1` | 0.983 [0.979,0.986] |
| `input-b6-1d-x100000` | 1.000 [0.998,1.002] |
| `input-b6-16d-x10000` | 0.770 [0.763,0.772] |
| `input-b6-64d-x5000` | 0.735 [0.730,0.746] |
| `input-b6-256d-x1000` | 0.757 [0.753,0.769] |
| `input-b6-1024d-x20` | 0.780 [0.777,0.788] |
| `input-b6-10000d-x2` | 0.955 [0.952,0.960] |
| `input-b6-100000d-x1` | 0.984 [0.979,0.987] |
| `input-b7-1d-x100000` | 1.000 [0.998,1.002] |
| `input-b7-16d-x10000` | 0.773 [0.765,0.780] |
| `input-b7-64d-x5000` | 0.770 [0.753,0.775] |
| `input-b7-256d-x1000` | 0.803 [0.795,0.811] |
| `input-b7-1024d-x20` | 0.812 [0.805,0.828] |
| `input-b7-10000d-x2` | 0.960 [0.959,0.962] |
| `input-b7-100000d-x1` | 0.989 [0.987,0.990] |
| `input-b8-1d-x100000` | 1.000 [0.997,1.001] |
| `input-b8-16d-x10000` | 0.773 [0.765,0.778] |
| `input-b8-64d-x5000` | 0.774 [0.768,0.776] |
| `input-b8-256d-x1000` | 0.823 [0.819,0.825] |
| `input-b8-1024d-x20` | 0.821 [0.810,0.826] |
| `input-b8-10000d-x2` | 0.963 [0.961,0.964] |
| `input-b8-100000d-x1` | 0.989 [0.985,0.992] |
| `input-b9-1d-x100000` | 1.000 [0.996,1.002] |
| `input-b9-16d-x10000` | 0.762 [0.758,0.764] |
| `input-b9-64d-x5000` | 0.780 [0.780,0.783] |
| `input-b9-256d-x1000` | 0.891 [0.885,0.898] |
| `input-b9-1024d-x20` | 0.850 [0.839,0.852] |
| `input-b9-10000d-x2` | 0.963 [0.960,0.967] |
| `input-b9-100000d-x1` | 0.982 [0.980,0.985] |
| `input-b10-1d-x100000` | 1.000 [1.000,1.002] |
| `input-b10-16d-x10000` | 0.993 [0.992,0.994] |
| `input-b10-64d-x5000` | 1.006 [0.994,1.014] |
| `input-b10-256d-x1000` | 1.002 [0.996,1.003] |
| `input-b10-1024d-x20` | 1.003 [0.993,1.005] |
| `input-b10-10000d-x2` | 1.000 [0.997,1.010] |
| `input-b10-100000d-x1` | 1.000 [0.994,1.005] |
| `input-b11-1d-x100000` | 0.925 [0.923,0.926] |
| `input-b11-16d-x10000` | 0.748 [0.742,0.758] |
| `input-b11-64d-x5000` | 0.772 [0.765,0.775] |
| `input-b11-256d-x1000` | 0.874 [0.862,0.877] |
| `input-b11-1024d-x20` | 0.820 [0.812,0.832] |
| `input-b11-10000d-x2` | 0.951 [0.947,0.954] |
| `input-b11-100000d-x1` | 0.970 [0.967,0.971] |
| `input-b12-1d-x100000` | 0.926 [0.917,0.928] |
| `input-b12-16d-x10000` | 0.727 [0.726,0.729] |
| `input-b12-64d-x5000` | 0.748 [0.743,0.751] |
| `input-b12-256d-x1000` | 0.895 [0.884,0.905] |
| `input-b12-1024d-x20` | 0.823 [0.804,0.834] |
| `input-b12-10000d-x2` | 0.956 [0.949,0.973] |
| `input-b12-100000d-x1` | 0.969 [0.967,0.973] |
| `input-b13-1d-x100000` | 0.925 [0.925,0.932] |
| `input-b13-16d-x10000` | 0.725 [0.715,0.737] |
| `input-b13-64d-x5000` | 0.745 [0.738,0.748] |
| `input-b13-256d-x1000` | 0.885 [0.880,0.902] |
| `input-b13-1024d-x20` | 0.803 [0.798,0.822] |
| `input-b13-10000d-x2` | 0.952 [0.951,0.955] |
| `input-b13-100000d-x1` | 0.968 [0.955,0.971] |
| `input-b14-1d-x100000` | 0.927 [0.924,0.930] |
| `input-b14-16d-x10000` | 0.724 [0.722,0.729] |
| `input-b14-64d-x5000` | 0.741 [0.738,0.745] |
| `input-b14-256d-x1000` | 0.881 [0.871,0.897] |
| `input-b14-1024d-x20` | 0.806 [0.797,0.813] |
| `input-b14-10000d-x2` | 0.951 [0.946,0.952] |
| `input-b14-100000d-x1` | 0.966 [0.964,0.979] |
| `input-b15-1d-x100000` | 0.925 [0.924,0.926] |
| `input-b15-16d-x10000` | 0.737 [0.718,0.740] |
| `input-b15-64d-x5000` | 0.747 [0.738,0.752] |
| `input-b15-256d-x1000` | 0.868 [0.838,0.874] |
| `input-b15-1024d-x20` | 0.792 [0.770,0.802] |
| `input-b15-10000d-x2` | 0.952 [0.947,0.954] |
| `input-b15-100000d-x1` | 0.967 [0.964,0.968] |
| `input-b16-1d-x100000` | 0.925 [0.923,0.926] |
| `input-b16-16d-x10000` | 0.745 [0.743,0.825] |
| `input-b16-64d-x5000` | 0.745 [0.742,0.751] |
| `input-b16-256d-x1000` | 0.904 [0.897,0.919] |
| `input-b16-1024d-x20` | 0.809 [0.802,0.819] |
| `input-b16-10000d-x2` | 0.956 [0.950,0.957] |
| `input-b16-100000d-x1` | 0.968 [0.966,0.969] |
| `control-1d` | 1.000 [1.000,1.005] |
| `control-1000d` | 1.000 [0.994,1.012] |
| `control-10000d` | 0.973 [0.762,1.077] |

## holdout

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-3d-x10000` | 0.906 [0.903,0.912] |
| `input-b2-257d-x1000` | 0.667 [0.663,0.683] |
| `input-b2-2048d-x20` | 0.781 [0.765,0.793] |
| `input-b2-20000d-x1` | 0.937 [0.934,0.939] |
| `input-b2-120000d-x1` | 0.942 [0.937,0.976] |
| `input-b8-3d-x10000` | 0.909 [0.907,0.910] |
| `input-b8-257d-x1000` | 0.816 [0.808,0.821] |
| `input-b8-2048d-x20` | 0.873 [0.861,0.876] |
| `input-b8-20000d-x1` | 0.977 [0.972,0.978] |
| `input-b8-120000d-x1` | 0.991 [0.989,1.011] |
| `input-b10-3d-x10000` | 0.989 [0.980,0.990] |
| `input-b10-257d-x1000` | 1.002 [0.999,1.007] |
| `input-b10-2048d-x20` | 0.997 [0.992,1.000] |
| `input-b10-20000d-x1` | 1.002 [0.952,1.020] |
| `input-b10-120000d-x1` | 1.006 [0.998,1.017] |
| `input-b16-3d-x10000` | 0.888 [0.887,0.901] |
| `input-b16-257d-x1000` | 0.914 [0.909,0.932] |
| `input-b16-2048d-x20` | 0.862 [0.856,0.878] |
| `input-b16-20000d-x1` | 0.976 [0.976,0.978] |
| `input-b16-120000d-x1` | 0.970 [0.966,0.978] |
| `control-1d` | 1.000 [0.999,1.009] |
| `control-1000d` | 1.003 [0.994,1.006] |
| `control-10000d` | 0.999 [0.905,1.083] |
