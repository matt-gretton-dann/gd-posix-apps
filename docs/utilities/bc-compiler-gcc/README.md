# Linux GCC optimization matrix

Refs #213. Run the checked full and held-out arithmetic corpora with
matched GCC O2 and O3 builds, each with IPO disabled and required. All
four variants use the same checkout, native GCC toolchain, Release
mode, extensions and a multiplication split point of 126. A pinned
Howard build uses the same GCC C compiler.

```sh
bash docs/utilities/bc-compiler-gcc/run.sh . /path/to/new-output
```

The runner checks the actual optimization and IPO flags in each VM
compile command and runs the CLI suite before timing. It retains
compiler versions, revision and binary identities, compile commands,
build logs, checked samples and paired summaries. The build logs give
single wall-time observations. Results from the hosted runner are
advisory and have no separate A/A control.

## Ubuntu GCC 13.3 results

Run 41781494455 tested source `d9969a31ff3344e393c8924f66be76d21cc5e084`
with Ubuntu GCC 13.3.0-6ubuntu2~24.04.1. Each preset checked 3,300
outputs over 66 workloads. The [full samples](full.jsonl.gz) and
[holdout samples](holdout.jsonl.gz) accompany their
[full](full-summary.json.gz) and [holdout](holdout-summary.json.gz)
paired summaries, including the pinned Howard role.

| GCC variant | Configure seconds | Build seconds | Executable bytes | Full / holdout intervals wholly above 1.05 |
| --- | ---: | ---: | ---: | ---: |
| O2, IPO off | 1.83 | 17.48 | 632464 | 12 / 13 |
| O2, IPO required | 2.21 | 12.28 | 456712 | 7 / 9 |
| O3, IPO off | 1.42 | 18.78 | 695872 | 7 / 4 |
| O3, IPO required | 1.62 | 12.91 | 499432 | reference |

No alternative had an interval wholly below 0.95 relative to O3
with IPO required. Startup medians for the three alternatives ranged
from 0.999 to 1.048 relative to that reference across both presets.
This run supports retaining the existing release configuration.

## Native target experiment

The separate GCC native-target workflow compares Release O3 with IPO
using the portable target, an explicit `-march=native` build, and an
independently rebuilt portable control. It records CPU features,
compiler and executable identities, build time, size, and checked
full and holdout timing with the pinned Howard comparator. The native
setting is confined to the workflow build and depends on the runner's
CPU. The project's distributable target remains portable.

[Run 41805291829](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41805291829)
tested `d1719019c8c6bcb726a7e62f57df13032d825fff` on a hosted Linux x86-64
AMD EPYC 9V74 with GCC 13.3. Each preset checked 2,640 outputs across 66
workloads and four roles. The portable builds produced identical binaries.
The native build used `-march=native` throughout. All three builds used
`-O3` and CMake-required IPO for the relevant targets.

| Build | Build seconds | Executable bytes |
| --- | ---: | ---: |
| Portable | 23.33 | 505024 |
| Portable A/A | 22.76 | 505024 |
| Native | 22.74 | 523080 |

Native file statement timings were 0.938 [0.932, 0.952] full and
0.932 [0.905, 0.935] holdout relative to portable. Dense arrays took
1.043 [1.030, 1.059] and 1.044 [1.039, 1.051]. No native interval was
wholly above 1.05. Full decimal input had a median of 1.056 with an
interval spanning the 5% limit, and its identical-binary A/A median was
1.100. Only 49 full and 48 holdout A/A intervals fit within [0.95, 1.05].
This CPU's mixed results and the 3.6% size increase support keeping the
portable release default.

Archived evidence includes the checked
[full samples](native-full-samples.jsonl.gz),
[summary](native-full-summary.json.gz), and
[manifest](native-full-manifest.json.gz), plus the corresponding
[holdout samples](native-holdout-samples.jsonl.gz),
[summary](native-holdout-summary.json.gz), and
[manifest](native-holdout-manifest.json.gz).
[Executable identities and comparator configuration](native-implementations.json.gz)
and [runner CPU features](native-cpu.txt.gz) apply to both presets.
The original Actions ZIP SHA-256 was
`7491d3b2ff71eea329054abaeef599452942743aea3fc3faba3a7090caf7707e`.
