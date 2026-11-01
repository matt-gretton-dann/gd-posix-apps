# Integrated decimal-format component comparisons

Refs #206 and #197. Exact baseline dd8c60c274511e26ed453eb1d8e175936ea6f576
and candidate 6431f7deb50b9f9f665ef1dc42cdc5fb0ef5236d. Both include accepted large-input
runtime conversion. Complete actual flags, compiler/library/source identities,
commands and binary hashes remain in recipes. Earlier source results, including
the held decimal microcase, retain their original identities and are not replaced.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, fixed split 126,
extensions ON, profiling/coverage OFF. One checked warmup and nine shuffled
paired rounds, separate full/held-out seeds, 2000 bootstrap resamples. All
exact integer/decimal/continuation-length checksum and process checks pass.
No local builds/tests/profiles overlap timing. Host scheduling, frequency
and thermals remain uncontrolled. Controls stay separate and are not subtracted.

All 600 invocations pass; no interval lower endpoint exceeds 1.05. One-digit
output ratio is about 0.547, 100000 digits 0.204 and held-out 120000 0.203.
Native acceptance and final combined decoder/formatter evaluation remain required.

## full

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b10-1d-x50000` | 0.547 [0.547,0.548] |
| `output-b10-9d-x25000` | 0.538 [0.538,0.540] |
| `output-b10-10d-x25000` | 0.453 [0.451,0.456] |
| `output-b10-26d-x10000` | 0.509 [0.498,0.513] |
| `output-b10-27d-x10000` | 0.522 [0.520,0.524] |
| `output-b10-28d-x10000` | 0.485 [0.476,0.488] |
| `output-b10-29d-x10000` | 0.487 [0.486,0.493] |
| `output-b10-35d-x10000` | 0.489 [0.487,0.491] |
| `output-b10-36d-x10000` | 0.497 [0.489,0.500] |
| `output-b10-37d-x10000` | 0.450 [0.446,0.455] |
| `output-b10-68d-x5000` | 0.403 [0.398,0.407] |
| `output-b10-69d-x5000` | 0.417 [0.416,0.421] |
| `output-b10-70d-x5000` | 0.426 [0.422,0.433] |
| `output-b10-128d-x2500` | 0.371 [0.368,0.375] |
| `output-b10-1000d-x2000` | 0.250 [0.249,0.253] |
| `output-b10-10000d-x200` | 0.205 [0.205,0.206] |
| `output-b10-100000d-x100` | 0.204 [0.203,0.206] |
| `control-1d` | 1.004 [0.998,1.005] |
| `control-1000d` | 0.997 [0.985,1.016] |
| `control-10000d` | 1.026 [0.926,1.151] |

## holdout

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b10-3d-x50000` | 0.556 [0.556,0.560] |
| `output-b10-67d-x5000` | 0.404 [0.397,0.406] |
| `output-b10-71d-x5000` | 0.426 [0.424,0.428] |
| `output-b10-257d-x500` | 0.338 [0.329,0.346] |
| `output-b10-2048d-x1000` | 0.230 [0.228,0.232] |
| `output-b10-20000d-x200` | 0.200 [0.199,0.201] |
| `output-b10-120000d-x100` | 0.203 [0.200,0.204] |
| `control-1d` | 1.000 [0.999,1.000] |
| `control-1000d` | 0.997 [0.985,1.216] |
| `control-10000d` | 1.027 [1.000,1.167] |
