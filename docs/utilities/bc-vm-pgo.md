# Experimental profile-guided optimization for the bc VM

Clang and AppleClang builds using GNU-style options can collect an LLVM IR
profile for `vm.cc` and use it in a separate build. The default build needs
no profile. Keep the source revision, compiler, build type, extension mode,
compact and typed execution settings, and arithmetic configuration the same
between the training and profile-use builds.

```sh
cmake -S . -B build-pgo-train -G Ninja -DCMAKE_BUILD_TYPE=Release \
  -DENABLE_BC_VM_PGO_GENERATE=ON
cmake --build build-pgo-train --target bc
mkdir -p bc-vm-profiles
LLVM_PROFILE_FILE="$PWD/bc-vm-profiles/%p.profraw" \
  build-pgo-train/src/bc/bc training.bc </dev/null
llvm-profdata merge bc-vm-profiles/*.profraw -o bc-vm.profdata
cmake -S . -B build-pgo-use -G Ninja -DCMAKE_BUILD_TYPE=Release \
  -DBC_VM_PGO_PROFILE="$PWD/bc-vm.profdata"
cmake --build build-pgo-use --target bc
```

Use a fresh profile directory, a matching `llvm-profdata`, and representative
training inputs that include arithmetic, functions, arrays, streaming input
and error paths. Run correctness tests and measure on separate evaluation
inputs. Record the source and profile hashes, compiler version, actual
compiler commands, and per-workload results. The compiler treats stale
profiles and missing translation-unit profile data as errors.

The initial macOS arm64 pilot applied an IR profile only to `vm.cc`. Across
two runs, the call workload took about 31% less time, loops 28% less, and
sparse arrays 24% less. Square-root intervals included parity in the first
run and showed a 0.34% slowdown in the second. These are advisory results
from one host. See issue #213 for the training and evaluation scope.

## Linux sample profile capability

The sampling PGO capability workflow tests whether the hosted Linux runner can
capture userspace branch stacks with `perf` and whether LLVM can convert and
consume the resulting profile. It also attempts software CPU-clock sampling
for attribution when the host lacks branch-stack support. The workflow keeps
the runner CPU, perf permissions, tool versions and failure diagnostics in
the artifact. This probe establishes collection capability. Workload training,
holdout timing and correctness for a sample-profiled `bc` are separate.

Hosted run 41807427320 reported `perf_event_paranoid=4` and denied both
unprivileged events. The workflow also tries a privileged collector on its
disposable runner, with the probe running under the regular runner UID and
GID. It keeps the host perf policy intact and records both attempts.

[Hosted run 41807637935](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41807637935)
used Ubuntu Clang 18.1.3 and perf 7.0.14 on an AMD EPYC 9V45 x86-64
runner. The collector recorded 2,573 software CPU-clock samples, including
2,569 attributed to the probe's training function. The sampled program
produced the expected output. The PMU exposed by this runner rejected
branch-stack recording even with the privileged collector.

Use the CPU-clock trace for CPU attribution. This run did not produce the
branch-stack input needed by the workflow's LLVM converter, so it provides
no sample-profiled executable or performance comparison. The original
artifact ZIP SHA-256 was
`8a6cd8afedfebbbdc34eebc86b8cd3235eeb5dd53d4e0864a48527c58e4cbdaa`.
