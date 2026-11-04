# Apple Clang and upstream Clang without IPO

This #213 comparison holds the operating system, SDK, bc sources and
build options constant while varying the compiler and optimization
level. It compares the complete compiler distributions, including their
libc++ headers (220106 and 230104 respectively). Both binaries link
the same system libc++ and libSystem. It used Apple Clang 21.0.0 (clang-2100.3.34.2) and Homebrew Clang
23.1.4 on macOS 27.2 arm64, SDK 27.0, CMake 4.4.5 and Ninja 1.13.2.

The Apple builds used `72485e3490bc22a91a1d63cc057e7da73c7d019e`.
The upstream builds used `965a263b6a31c261b12d60694e9b0e218d832c6b`.
The commits differ only in compiler experiment documentation. All
four builds enabled extensions, disabled IPO and used a multiplication
split point of 126. Both upstream binaries passed 34 CLI tests.

| Variant | Configure seconds | Build seconds | Executable bytes |
| --- | ---: | ---: | ---: |
| Apple Clang O2 | 2.37 | 9.15 | 746896 |
| Apple Clang O3 | 2.42 | 9.60 | 754752 |
| Homebrew Clang O2 | 3.57 | 8.35 | 699024 |
| Homebrew Clang O3 | 2.64 | 8.02 | 695168 |

Build times are single observations. All four configurations used
`-O2 -DNDEBUG` or `-O3 -DNDEBUG` for both C and C++. To reproduce,
configure a separate directory for each compiler and optimization
level with the following options.

```sh
cmake -S . -B "$build_dir" -G Ninja -DCMAKE_BUILD_TYPE=Release \
  "-DCMAKE_C_COMPILER=$cc" "-DCMAKE_CXX_COMPILER=$cxx" \
  "-DCMAKE_C_FLAGS_RELEASE=-O$level -DNDEBUG" \
  "-DCMAKE_CXX_FLAGS_RELEASE=-O$level -DNDEBUG" \
  -DBUILD_TESTING=OFF -DIPO_MODE=OFF -DBC_MULTIPLY_SPLIT_POINT=126
cmake --build "$build_dir" --target bc -j 3
```

The checked arithmetic full and holdout presets each use nine measured
launches and one warmup per role and workload, with schedule and corpus
seeds of 197200. The fifth role is the source-built Howard 7.0.3
comparator at `c8dabac65d39fd527a9264a5e505a99b1d8df3d8`, built with
Apple Clang O3. See the [comparator recipe](../bc-comparator-builds.md).
The paired intervals are pointwise and there was no separate A/A run.
This experiment covers a single macOS host without IPO.

All 6,600 outputs passed their oracles. Across both presets, neither
Homebrew variant had a nominal interval entirely above 1.05 or below
0.95 relative to Apple Clang O3. For Homebrew Clang O3, the startup
ratios were 0.992 [0.952, 1.038] in full and 0.999 [0.975, 1.004] in
holdout. Its executable was 7.9% smaller than the Apple Clang O3
executable in this IPO-disabled comparison. These measurements leave
the release compiler defaults unchanged.

The [full](full.jsonl.gz) and [holdout](holdout.jsonl.gz) archives retain
the checked samples. The [ratios](ratios.csv) report comparisons against
Apple Clang O3 and Howard.
