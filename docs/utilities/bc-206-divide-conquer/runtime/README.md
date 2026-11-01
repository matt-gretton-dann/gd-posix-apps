# Actual runtime input dispatch comparisons

Refs #206 and #197. Exact baseline 9669ef73bc00e5b89f3fb00e2867a8f8ed9c4d32
and candidate a54210149ced1f8b773cd0af2b434790ae52e34c. Both use the ordinary
`conversion-in` mode and actual Number constructor, not a forced DC diagnostic.
Every compiler/library, flag, command, source and binary hash remains in recipes.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, fixed split 126,
extensions ON, profiling/coverage OFF. Full 960 and held-out 460 invocations
pass exact Python integer/decimal/length-checksum, status, stderr and deadline
checks. One checked warmup and nine shuffled paired rounds; 2000 bootstrap
resamples. No local builds, tests or profiling overlap timing. Shared-host
contention, frequency and thermals remain uncontrolled. Separate copy/reset/
consumer controls remain auxiliary and are never subtracted.

At 100000 input characters, runtime ratios are approximately 0.257 binary,
0.201 base three, 0.148 octal and 0.128 hexadecimal. Held-out 120000-character
ratios are 0.235, 0.139 and 0.121 for binary, octal and hex. Decimal remains
near 1.0. No measured interval has its lower endpoint above 1.05. That is
scoped local evidence, not a universal or native-platform acceptance claim.

The measured runtime policy retains short/fractional/exponent-width fallbacks.
The earlier prototype memory observations are separate process measurements
and are not relabeled as actual runtime workspace or final peak RSS.

The first full Release log predates the corrected narrow length fixtures;
the separate domain-test log and sanitizer logs record the corrected tests.
All three corrected domain cases pass; native CI must still pass at the PR head.

## full

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-1d-x100000` | 1.019 [1.018,1.019] |
| `input-b2-16d-x10000` | 1.022 [1.018,1.028] |
| `input-b2-64d-x5000` | 0.998 [0.975,1.005] |
| `input-b2-256d-x1000` | 0.997 [0.988,1.009] |
| `input-b2-10000d-x2` | 1.000 [0.999,1.003] |
| `input-b2-65535d-x1` | 1.002 [1.000,1.009] |
| `input-b2-65536d-x1` | 0.315 [0.315,0.316] |
| `input-b2-65537d-x1` | 0.340 [0.339,0.342] |
| `input-b2-100000d-x1` | 0.257 [0.255,0.257] |
| `input-b3-1d-x100000` | 1.019 [1.019,1.021] |
| `input-b3-16d-x10000` | 1.019 [1.016,1.022] |
| `input-b3-64d-x5000` | 1.002 [0.947,1.037] |
| `input-b3-256d-x1000` | 0.999 [0.996,1.003] |
| `input-b3-10000d-x2` | 0.998 [0.983,0.998] |
| `input-b3-65535d-x1` | 0.995 [0.994,0.997] |
| `input-b3-65536d-x1` | 0.255 [0.254,0.264] |
| `input-b3-65537d-x1` | 0.279 [0.277,0.280] |
| `input-b3-100000d-x1` | 0.201 [0.201,0.202] |
| `input-b8-1d-x100000` | 1.000 [0.998,1.002] |
| `input-b8-16d-x10000` | 0.894 [0.884,0.897] |
| `input-b8-64d-x5000` | 0.881 [0.855,0.882] |
| `input-b8-256d-x1000` | 0.913 [0.910,0.942] |
| `input-b8-10000d-x2` | 0.932 [0.914,0.934] |
| `input-b8-65535d-x1` | 0.989 [0.982,0.991] |
| `input-b8-65536d-x1` | 0.192 [0.191,0.193] |
| `input-b8-65537d-x1` | 0.213 [0.213,0.213] |
| `input-b8-100000d-x1` | 0.148 [0.148,0.149] |
| `input-b10-1d-x100000` | 1.001 [0.999,1.011] |
| `input-b10-16d-x10000` | 0.909 [0.904,0.915] |
| `input-b10-64d-x5000` | 0.968 [0.967,0.976] |
| `input-b10-256d-x1000` | 0.994 [0.991,0.996] |
| `input-b10-10000d-x2` | 1.006 [0.993,1.009] |
| `input-b10-65535d-x1` | 0.999 [0.853,1.003] |
| `input-b10-65536d-x1` | 1.001 [0.996,1.003] |
| `input-b10-65537d-x1` | 1.001 [0.986,1.024] |
| `input-b10-100000d-x1` | 1.001 [0.997,1.002] |
| `input-b16-1d-x100000` | 1.031 [1.027,1.031] |
| `input-b16-16d-x10000` | 0.999 [0.995,1.004] |
| `input-b16-64d-x5000` | 1.011 [1.004,1.016] |
| `input-b16-256d-x1000` | 0.998 [0.977,1.018] |
| `input-b16-10000d-x2` | 1.019 [1.018,1.022] |
| `input-b16-65535d-x1` | 1.001 [0.940,1.005] |
| `input-b16-65536d-x1` | 0.155 [0.155,0.155] |
| `input-b16-65537d-x1` | 0.170 [0.166,0.170] |
| `input-b16-100000d-x1` | 0.128 [0.128,0.129] |
| `control-1d` | 1.000 [0.995,1.012] |
| `control-1000d` | 1.003 [0.994,1.009] |
| `control-10000d` | 1.027 [0.366,1.205] |

## holdout

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-3d-x10000` | 1.030 [1.023,1.038] |
| `input-b2-257d-x1000` | 1.006 [1.000,1.023] |
| `input-b2-2048d-x20` | 1.001 [0.983,1.007] |
| `input-b2-70000d-x1` | 0.319 [0.319,0.319] |
| `input-b2-120000d-x1` | 0.235 [0.235,0.236] |
| `input-b8-3d-x10000` | 0.969 [0.966,0.972] |
| `input-b8-257d-x1000` | 0.937 [0.917,0.947] |
| `input-b8-2048d-x20` | 0.829 [0.825,0.831] |
| `input-b8-70000d-x1` | 0.192 [0.192,0.193] |
| `input-b8-120000d-x1` | 0.139 [0.139,0.140] |
| `input-b10-3d-x10000` | 0.969 [0.959,0.978] |
| `input-b10-257d-x1000` | 0.989 [0.988,0.992] |
| `input-b10-2048d-x20` | 1.001 [1.000,1.008] |
| `input-b10-70000d-x1` | 0.998 [0.982,1.020] |
| `input-b10-120000d-x1` | 1.000 [0.994,1.008] |
| `input-b16-3d-x10000` | 1.029 [1.021,1.033] |
| `input-b16-257d-x1000` | 0.985 [0.966,0.998] |
| `input-b16-2048d-x20` | 1.050 [1.043,1.051] |
| `input-b16-70000d-x1` | 0.153 [0.153,0.154] |
| `input-b16-120000d-x1` | 0.121 [0.121,0.122] |
| `control-1d` | 1.000 [1.000,1.005] |
| `control-1000d` | 1.006 [0.863,1.016] |
| `control-10000d` | 1.028 [0.943,1.156] |

