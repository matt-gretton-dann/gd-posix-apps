#!/usr/bin/env python3
"""Run asserted bc tests and collect isolated LLVM line/branch/MC/DC evidence.

Configure and build an instrumented Debug tree first; see docs/utilities/bc-testing.md.
Each invocation requires a new output directory. Build/tuner/manual profiles are
never read. Extensions ON and OFF must use separate build and output directories.
"""

import argparse
import json
import os
import platform
from pathlib import Path
import re
import subprocess
import sys


def run(command, cwd=None, env=None):
    return subprocess.run(command, cwd=cwd, env=env, universal_newlines=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def save_run(path, command, cwd=None, env=None):
    result = run(command, cwd, env)
    path.write_text(result.stdout + result.stderr)
    if result.returncode:
        raise RuntimeError("command failed; see " + str(path))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--config", default="Debug")
    parser.add_argument("--llvm-cov", default="llvm-cov")
    parser.add_argument("--llvm-profdata", default="llvm-profdata")
    parser.add_argument("--ctest", default="ctest")
    args = parser.parse_args()
    build, output = args.build.resolve(), args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    profiles = output / "test-profiles"
    profiles.mkdir()

    cache = {}
    for line in (build / "CMakeCache.txt").read_text().splitlines():
        if "=" in line and not line.startswith(("#", "//")):
            key, value = line.split("=", 1)
            cache[key.split(":", 1)[0]] = value
    source = Path(cache["CMAKE_HOME_DIRECTORY"]).resolve()
    inventory = save_run(output / "inventory.json",
                         [args.ctest, "-C", args.config, "--show-only=json-v1"], cwd=build)
    tests = json.loads(inventory.stdout)["tests"]
    selected = [test for test in tests if test["name"].startswith("bc/") or
                any("test-libbc" in part for part in test.get("command", []))]
    if not selected:
        raise RuntimeError("no bc tests discovered")
    (output / "selected-tests.json").write_text(json.dumps(selected, indent=2) + "\n")

    # Resolve the actual tested binaries, including multi-configuration layouts.
    unit_commands = [test["command"] for test in selected
                     if any("test-libbc" in p for p in test.get("command", []))]
    cli_commands = [test["command"] for test in selected if test["name"] == "bc/cli"]
    if not unit_commands or not cli_commands:
        raise RuntimeError("both the unit executable and asserted CLI test are required")
    unit = next(p for p in unit_commands[0] if "test-libbc" in p)
    bc = next(p for p in cli_commands[0] if Path(p).name in ("bc", "bc.exe"))

    env = dict(os.environ, LLVM_PROFILE_FILE=str(profiles / "%m-%p.profraw"))
    expression = "^(" + "|".join(re.escape(test["name"]) for test in selected) + ")$"
    test_run = run([args.ctest, "-C", args.config, "-R", expression,
                    "--output-on-failure", "--timeout", "120"], cwd=build, env=env)
    (output / "tests.log").write_text(test_run.stdout + test_run.stderr)
    raw = sorted(profiles.glob("*.profraw"))
    if not raw:
        raise RuntimeError("tests generated no profiles; check instrumentation")
    manifest = output / "profiles.txt"
    manifest.write_text("\n".join(str(p) for p in raw) + "\n")
    merged = output / "tests.profdata"
    save_run(output / "merge.log", [args.llvm_profdata, "merge", "-sparse",
                                    "-f", str(manifest), "-o", str(merged)])

    # Preserve the broad baseline's header/template scope. Never sum repeated
    # raw MC/DC records: LLVM's summary is the condition-obligation denominator.
    common = [bc, "-object", unit, "-instr-profile=" + str(merged),
              r"-ignore-filename-regex=(^|[/\\])(test-[^/\\]*|find-multiply-split-point[^/\\]*|benchmark-support[^/\\]*|bc-messages[^/\\]*)$",
              str(source / "src" / "bc")]
    exported = run([args.llvm_cov, "export", *common])
    (output / "coverage.json").write_text(exported.stdout)
    (output / "coverage-warnings.log").write_text(exported.stderr)
    if exported.returncode:
        raise RuntimeError("llvm-cov export failed; see coverage-warnings.log")
    coverage = json.loads(exported.stdout)["data"][0]
    # Losing a linked implementation object must not silently shrink the scope.
    baseline_files = {"bc.cc", "bc.hh", "instruction.cc", "lexer.cc", "number.hh",
                      "parser.cc", "reader.cc", "token.cc", "verification.cc", "vm.cc"}
    expected = {source / "src" / "bc" / name for name in baseline_files
                if (source / "src" / "bc" / name).exists()}
    mapped = {Path(entry["filename"]).resolve() for entry in coverage["files"]}
    if not expected.issubset(mapped):
        raise RuntimeError("missing production mappings; use the default static-library build: " +
                           ", ".join(sorted(path.name for path in expected - mapped)))
    if not coverage["totals"].get("mcdc", {}).get("count", 0):
        raise RuntimeError("no MC/DC obligations found; use a supported Clang with -fcoverage-mcdc")
    # A second view uses only the application's mappings, excluding extra small
    # trait instantiations present solely in the unit executable. Keep the broad
    # baseline above unchanged; these are different reporting scopes.
    native_common = [common[0], *common[3:]]
    native_run = run([args.llvm_cov, "export", *native_common])
    (output / "native-coverage.json").write_text(native_run.stdout)
    (output / "native-warnings.log").write_text(native_run.stderr)
    if native_run.returncode:
        raise RuntimeError("native llvm-cov export failed; see native-warnings.log")
    native = json.loads(native_run.stdout)["data"][0]
    report = {
        "compiler_version": run([cache["CMAKE_CXX_COMPILER"], "--version"]).stdout.splitlines()[0],
        "coverage_option": cache.get("ENABLE_BC_COVERAGE"),
        "platform": {"system": platform.system(), "release": platform.release(),
                     "machine": platform.machine()},
        "python_version": platform.python_version(),
        "ctest_version": run([args.ctest, "--version"]).stdout.splitlines()[0],
        "source_revision": save_run(output / "revision.txt",
                                    ["git", "-C", str(source), "rev-parse", "HEAD"]).stdout.strip(),
        "source_status": run(["git", "-C", str(source), "status", "--short"]).stdout,
        "extensions": cache.get("ENABLE_NON_POSIX_EXTENSIONS"),
        "configuration": args.config,
        "test_exit_code": test_run.returncode,
        "selected_bc_tests": len(selected),
        "profile_count": len(raw),
        "raw_totals": coverage["totals"],
        "files": {entry["filename"]: entry["summary"] for entry in coverage["files"]},
        "application_mappings_totals": native["totals"],
        "application_mappings_files": {entry["filename"]: entry["summary"]
                                       for entry in native["files"]},
        "application_mappings_warnings": native_run.stderr,
        "warnings": exported.stderr,
        "adjusted_coverage": None,
        "scope": "production src/bc; broad native and test-trait header instantiations",
    }
    (output / "summary.json").write_text(json.dumps(report, indent=2) + "\n")
    save_run(output / "coverage.txt", [args.llvm_cov, "report", "-show-mcdc-summary", *common])
    save_run(output / "html.log", [args.llvm_cov, "show", "-format=html", "-show-branches=count",
                                   "-show-mcdc", "-output-dir=" + str(output / "html"), *common])
    save_run(output / "llvm-version.txt", [args.llvm_cov, "--version"])
    print(json.dumps({"output": str(output), "raw_totals": report["raw_totals"],
                      "test_exit_code": test_run.returncode}, indent=2))
    return 1 if test_run.returncode else 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError, ValueError, KeyError, StopIteration) as error:
        print("coverage collection failed: " + str(error), file=sys.stderr)
        sys.exit(1)
