# Combined decimal formatting and indexed radix decoding

Refs #206 and #197. Exact accepted-input baseline
`dd8c60c274511e26ed453eb1d8e175936ea6f576` and combined candidate
`68c22f90b4c8e2457ea2bb91df551c0fe8653e53`. The candidate combines the
fixed-buffer classic-locale formatter and unsigned-index byte decoder with
accepted balanced large input. Generic nonclassic-locale and character-model
fallbacks remain. One pre-reserved coefficient string and one bounded limb
buffer replace the classic formatter stream; this is not constant-space output.

Every actual source, compiler/library, flag, command and binary hash remains
in immutable recipes. Earlier held source dbcdcad and its 1.106 decimal microcase
remain in the decoder archive; no later data replace them or establish a cause.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, fixed split 126,
extensions ON, profiling/coverage OFF. Full/held-out output 400/200 and input
2160/460 invocations pass exact Python integer/decimal/continuation-length
checksum and process checks. One checked warmup and nine shuffled paired
rounds, separate seeds and 2000 bootstrap resamples. No local builds, tests,
profiling or memory diagnostics overlap timing; host scheduling/frequency/
thermals remain uncontrolled. Controls are separate and never subtracted.

No measured interval has its lower endpoint above 1.05. Output ratios are
about 0.552 for one digit and 0.206 at 100000 digits; held-out 120000 is 0.202.
Binary input at 16/64 characters is 0.713/0.667, hex 0.744/0.739. Held-out
three-character decimal is 1.038: retain that result and its interval rather
than claiming universal neutrality. Decimal parse arithmetic is unchanged.

Release passes 203 cases/691890 assertions and eight CLI groups. Extensions-
OFF ASan/UBSan passes 36 focused cases/55604 assertions, fifteen VM CLI, ten
component and six offset groups. Twelve profiling CLI groups pass. Mapping
checks exhaust all 256 byte values against the prior decoder; arithmetic
oracles separately cover all supported bases, trait widths and boundary forms.

## Separate memory and code-size diagnostics

Twenty-four serial fresh processes (three per case/implementation) pass exact
oracles after all timing, builds, tests and profiles finish. Local getrusage(2)
documents raw peak bytes. These are complete process peaks, including loader,
the one-MiB fixture buffer, preparsed Number, conversion buffers and final JSON;
they are not isolated algorithm workspace or selected allocator payloads.

| Case | Baseline median MiB | Candidate median MiB |
| --- | ---: | ---: |
| `input-b16-100000d-x1` | 4.516 | 4.297 |
| `input-b2-100000d-x1` | 3.734 | 3.672 |
| `output-b10-100000d-x100` | 3.703 | 3.469 |
| `output-b10-1000d-x2000` | 2.797 | 2.781 |

The sampled peaks are slightly lower, not a universal memory reduction or a
confidence bound. Both ordinary bc binaries have 311296-byte Mach-O __TEXT
segments; file sizes are 507536/507776 bytes. __TEXT is code/read-only storage,
not resident process memory. Exact hashes and raw size/time logs remain.

## Native acceptance status

The staged formatter source 6431f7d passed all fourteen checks and has no
confirmed native regressions in 1650 checked rows per OS. Its original ZIPs
are retained under staged-formatter-native with explicit source/digest audits.
Those rows do not validate the later combined source. Final combined-head
three-platform CI, CodeQL and artifact audits remain required before merge.

## output full

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b10-1d-x50000` | 0.552 [0.542,0.569] |
| `output-b10-9d-x25000` | 0.554 [0.536,0.561] |
| `output-b10-10d-x25000` | 0.456 [0.430,0.461] |
| `output-b10-26d-x10000` | 0.537 [0.513,0.572] |
| `output-b10-27d-x10000` | 0.527 [0.514,0.545] |
| `output-b10-28d-x10000` | 0.473 [0.446,0.503] |
| `output-b10-29d-x10000` | 0.491 [0.484,0.500] |
| `output-b10-35d-x10000` | 0.493 [0.469,0.525] |
| `output-b10-36d-x10000` | 0.486 [0.481,0.504] |
| `output-b10-37d-x10000` | 0.450 [0.438,0.458] |
| `output-b10-68d-x5000` | 0.403 [0.390,0.415] |
| `output-b10-69d-x5000` | 0.423 [0.411,0.426] |
| `output-b10-70d-x5000` | 0.439 [0.436,0.482] |
| `output-b10-128d-x2500` | 0.370 [0.124,0.381] |
| `output-b10-1000d-x2000` | 0.249 [0.239,0.255] |
| `output-b10-10000d-x200` | 0.208 [0.204,0.212] |
| `output-b10-100000d-x100` | 0.206 [0.204,0.210] |
| `control-1d` | 0.976 [0.910,1.000] |
| `control-1000d` | 1.000 [0.997,1.018] |
| `control-10000d` | 0.977 [0.870,1.052] |

## output holdout

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b10-3d-x50000` | 0.554 [0.549,0.577] |
| `output-b10-67d-x5000` | 0.396 [0.077,0.404] |
| `output-b10-71d-x5000` | 0.427 [0.418,0.448] |
| `output-b10-257d-x500` | 0.343 [0.329,0.349] |
| `output-b10-2048d-x1000` | 0.229 [0.226,0.235] |
| `output-b10-20000d-x200` | 0.201 [0.197,0.202] |
| `output-b10-120000d-x100` | 0.202 [0.200,0.205] |
| `control-1d` | 0.999 [0.975,1.020] |
| `control-1000d` | 0.997 [0.918,1.021] |
| `control-10000d` | 1.027 [0.907,1.085] |

## input full

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-1d-x100000` | 0.975 [0.965,1.000] |
| `input-b2-16d-x10000` | 0.713 [0.697,0.727] |
| `input-b2-64d-x5000` | 0.667 [0.662,0.706] |
| `input-b2-256d-x1000` | 0.674 [0.659,0.689] |
| `input-b2-1024d-x20` | 0.741 [0.734,0.790] |
| `input-b2-10000d-x2` | 0.901 [0.899,0.911] |
| `input-b2-100000d-x1` | 0.939 [0.905,0.956] |
| `input-b3-1d-x100000` | 0.994 [0.956,1.043] |
| `input-b3-16d-x10000` | 0.725 [0.566,0.745] |
| `input-b3-64d-x5000` | 0.698 [0.679,0.706] |
| `input-b3-256d-x1000` | 0.654 [0.632,0.665] |
| `input-b3-1024d-x20` | 0.732 [0.704,0.758] |
| `input-b3-10000d-x2` | 0.918 [0.911,0.925] |
| `input-b3-100000d-x1` | 0.963 [0.957,0.965] |
| `input-b4-1d-x100000` | 1.018 [0.983,1.025] |
| `input-b4-16d-x10000` | 0.744 [0.726,0.772] |
| `input-b4-64d-x5000` | 0.736 [0.691,0.742] |
| `input-b4-256d-x1000` | 0.689 [0.658,0.718] |
| `input-b4-1024d-x20` | 0.746 [0.713,0.753] |
| `input-b4-10000d-x2` | 0.945 [0.910,0.981] |
| `input-b4-100000d-x1` | 0.974 [0.943,0.990] |
| `input-b5-1d-x100000` | 1.000 [0.979,1.031] |
| `input-b5-16d-x10000` | 0.779 [0.753,0.780] |
| `input-b5-64d-x5000` | 0.734 [0.715,0.789] |
| `input-b5-256d-x1000` | 0.719 [0.688,0.734] |
| `input-b5-1024d-x20` | 0.771 [0.712,0.834] |
| `input-b5-10000d-x2` | 0.953 [0.915,0.991] |
| `input-b5-100000d-x1` | 0.993 [0.967,1.006] |
| `input-b6-1d-x100000` | 0.975 [0.931,1.005] |
| `input-b6-16d-x10000` | 0.776 [0.767,0.791] |
| `input-b6-64d-x5000` | 0.746 [0.715,0.781] |
| `input-b6-256d-x1000` | 0.755 [0.734,0.766] |
| `input-b6-1024d-x20` | 0.779 [0.730,0.793] |
| `input-b6-10000d-x2` | 0.954 [0.929,0.973] |
| `input-b6-100000d-x1` | 0.983 [0.968,0.990] |
| `input-b7-1d-x100000` | 0.993 [0.953,1.001] |
| `input-b7-16d-x10000` | 0.774 [0.749,0.780] |
| `input-b7-64d-x5000` | 0.766 [0.744,0.770] |
| `input-b7-256d-x1000` | 0.801 [0.765,0.818] |
| `input-b7-1024d-x20` | 0.810 [0.757,0.833] |
| `input-b7-10000d-x2` | 0.975 [0.961,1.016] |
| `input-b7-100000d-x1` | 0.982 [0.888,0.991] |
| `input-b8-1d-x100000` | 1.000 [0.984,1.026] |
| `input-b8-16d-x10000` | 0.778 [0.775,0.801] |
| `input-b8-64d-x5000` | 0.785 [0.758,0.798] |
| `input-b8-256d-x1000` | 0.826 [0.815,0.845] |
| `input-b8-1024d-x20` | 0.826 [0.811,0.861] |
| `input-b8-10000d-x2` | 0.959 [0.870,0.975] |
| `input-b8-100000d-x1` | 0.986 [0.969,0.992] |
| `input-b9-1d-x100000` | 0.989 [0.974,1.010] |
| `input-b9-16d-x10000` | 0.784 [0.770,0.817] |
| `input-b9-64d-x5000` | 0.771 [0.749,0.817] |
| `input-b9-256d-x1000` | 0.902 [0.887,0.926] |
| `input-b9-1024d-x20` | 0.849 [0.843,0.891] |
| `input-b9-10000d-x2` | 0.973 [0.953,1.008] |
| `input-b9-100000d-x1` | 0.979 [0.970,0.991] |
| `input-b10-1d-x100000` | 1.005 [1.000,1.050] |
| `input-b10-16d-x10000` | 0.975 [0.894,0.995] |
| `input-b10-64d-x5000` | 0.994 [0.979,1.033] |
| `input-b10-256d-x1000` | 1.002 [0.986,1.050] |
| `input-b10-1024d-x20` | 1.000 [0.963,1.017] |
| `input-b10-10000d-x2` | 1.001 [0.989,1.058] |
| `input-b10-100000d-x1` | 1.001 [0.975,1.012] |
| `input-b11-1d-x100000` | 0.927 [0.896,0.940] |
| `input-b11-16d-x10000` | 0.760 [0.730,0.761] |
| `input-b11-64d-x5000` | 0.757 [0.738,0.778] |
| `input-b11-256d-x1000` | 0.891 [0.866,0.915] |
| `input-b11-1024d-x20` | 0.793 [0.766,0.813] |
| `input-b11-10000d-x2` | 0.948 [0.927,0.971] |
| `input-b11-100000d-x1` | 0.969 [0.943,0.972] |
| `input-b12-1d-x100000` | 0.918 [0.915,0.926] |
| `input-b12-16d-x10000` | 0.731 [0.728,0.738] |
| `input-b12-64d-x5000` | 0.744 [0.733,0.775] |
| `input-b12-256d-x1000` | 0.890 [0.841,0.929] |
| `input-b12-1024d-x20` | 0.829 [0.813,0.834] |
| `input-b12-10000d-x2` | 0.954 [0.912,0.964] |
| `input-b12-100000d-x1` | 0.965 [0.952,0.976] |
| `input-b13-1d-x100000` | 0.924 [0.915,0.929] |
| `input-b13-16d-x10000` | 0.724 [0.695,0.742] |
| `input-b13-64d-x5000` | 0.730 [0.691,0.748] |
| `input-b13-256d-x1000` | 0.868 [0.856,0.924] |
| `input-b13-1024d-x20` | 0.800 [0.760,0.826] |
| `input-b13-10000d-x2` | 0.954 [0.952,0.964] |
| `input-b13-100000d-x1` | 0.964 [0.939,0.968] |
| `input-b14-1d-x100000` | 0.917 [0.892,0.927] |
| `input-b14-16d-x10000` | 0.731 [0.710,0.737] |
| `input-b14-64d-x5000` | 0.727 [0.693,0.755] |
| `input-b14-256d-x1000` | 0.886 [0.863,0.895] |
| `input-b14-1024d-x20` | 0.798 [0.776,0.840] |
| `input-b14-10000d-x2` | 0.953 [0.905,0.972] |
| `input-b14-100000d-x1` | 0.969 [0.966,0.996] |
| `input-b15-1d-x100000` | 0.926 [0.923,0.958] |
| `input-b15-16d-x10000` | 0.734 [0.704,0.766] |
| `input-b15-64d-x5000` | 0.755 [0.735,0.771] |
| `input-b15-256d-x1000` | 0.877 [0.845,0.919] |
| `input-b15-1024d-x20` | 0.797 [0.788,0.855] |
| `input-b15-10000d-x2` | 0.949 [0.928,0.953] |
| `input-b15-100000d-x1` | 0.967 [0.957,0.983] |
| `input-b16-1d-x100000` | 0.925 [0.903,0.958] |
| `input-b16-16d-x10000` | 0.744 [0.729,0.770] |
| `input-b16-64d-x5000` | 0.739 [0.705,0.748] |
| `input-b16-256d-x1000` | 0.944 [0.910,0.998] |
| `input-b16-1024d-x20` | 0.821 [0.797,0.831] |
| `input-b16-10000d-x2` | 0.954 [0.914,0.977] |
| `input-b16-100000d-x1` | 0.963 [0.949,0.976] |
| `control-1d` | 0.975 [0.957,1.000] |
| `control-1000d` | 1.009 [0.991,1.012] |
| `control-10000d` | 0.949 [0.892,1.462] |

## input holdout

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `input-b2-3d-x10000` | 0.905 [0.879,0.908] |
| `input-b2-257d-x1000` | 0.666 [0.651,0.680] |
| `input-b2-2048d-x20` | 0.773 [0.756,0.790] |
| `input-b2-20000d-x1` | 0.938 [0.903,0.974] |
| `input-b2-120000d-x1` | 0.939 [0.888,0.953] |
| `input-b8-3d-x10000` | 0.905 [0.888,0.927] |
| `input-b8-257d-x1000` | 0.816 [0.768,0.834] |
| `input-b8-2048d-x20` | 0.881 [0.863,0.884] |
| `input-b8-20000d-x1` | 0.968 [0.953,1.001] |
| `input-b8-120000d-x1` | 0.988 [0.985,1.002] |
| `input-b10-3d-x10000` | 1.038 [0.991,1.069] |
| `input-b10-257d-x1000` | 1.019 [0.996,1.023] |
| `input-b10-2048d-x20` | 0.991 [0.965,1.025] |
| `input-b10-20000d-x1` | 1.002 [0.985,1.004] |
| `input-b10-120000d-x1` | 1.005 [0.949,1.019] |
| `input-b16-3d-x10000` | 0.893 [0.865,0.910] |
| `input-b16-257d-x1000` | 0.943 [0.892,1.026] |
| `input-b16-2048d-x20` | 0.866 [0.704,0.876] |
| `input-b16-20000d-x1` | 0.982 [0.967,1.008] |
| `input-b16-120000d-x1` | 0.972 [0.951,0.997] |
| `control-1d` | 1.000 [0.987,1.032] |
| `control-1000d` | 1.019 [0.984,1.224] |
| `control-10000d` | 1.027 [0.804,1.053] |
