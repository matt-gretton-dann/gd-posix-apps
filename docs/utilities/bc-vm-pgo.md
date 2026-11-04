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
