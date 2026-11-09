#!/usr/bin/env python3
"""Run asserted bc tests and collect isolated LLVM line/branch/MC/DC evidence.

Configure and build an instrumented Debug tree first; see docs/utilities/bc-testing.md.
Each invocation requires a new output directory. Build/tuner/manual profiles are
never read. Extensions, divisor reuse and compact execution modes require separate build/output directories.
"""

import argparse
import hashlib
import json
import os
import platform
from pathlib import Path
import re
import shutil
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


def production_mappings(source, build=None):
    """Require configured production objects, preserving mandatory core mappings.

    The source-only inventory remains useful for legacy callers and for listing
    files not required by this configuration. It cannot identify active targets.
    A real collection must supply its build's compilation database instead of
    assuming every optional backend and build-time generator can be linked.
    """
    directory = Path(source).resolve() / "src" / "bc"
    helpers = ("test-", "benchmark-", "find-multiply-split-point")
    diagnostic_only = {"profiling.cc", "profiling-entry.cc"}
    implementations = {path for path in directory.glob("*.cc")
                       if not path.name.startswith(helpers) and path.name not in diagnostic_only}
    headers = {directory / name for name in ("bc.hh", "number.hh")
               if (directory / name).exists()}
    if build is None:
        return implementations | headers
    database = Path(build) / 'compile_commands.json'
    try:
        rows = json.loads(database.read_text(encoding='utf-8'))
    except (OSError, ValueError) as error:
        raise RuntimeError('coverage needs compile_commands.json; configure a supported '
                           'generator with CMAKE_EXPORT_COMPILE_COMMANDS=ON: ' + str(error))
    if not isinstance(rows, list) or not rows:
        raise RuntimeError('invalid or empty coverage compilation inventory')
    compiled = set()
    target = re.compile(r'(?:^|/)CMakeFiles/(?:bc|libbc)\.dir/')
    for row in rows:
        if not isinstance(row, dict) or not isinstance(row.get('file'), str):
            raise RuntimeError('invalid source in coverage compilation inventory')
        output = row.get('output')
        if output is None:
            # CMake 3.15 databases need not provide an output field. Target
            # directories in the generated compiler command identify the same
            # production units; unit-test and generator targets cannot replace
            # them. This is inventory inspection, never command execution.
            command = row.get('command')
            if command is None:
                args = row.get('arguments')
                if isinstance(args, list) and all(isinstance(a, str) for a in args):
                    outputs = [args[i + 1] for i, arg in enumerate(args[:-1]) if arg == '-o']
                    outputs += [arg[3:] for arg in args if arg.startswith('/Fo')]
                    output = outputs[0] if len(outputs) == 1 else None
            elif isinstance(command, str):
                # Inspect only the output argument. An include/definition path
                # mentioning libbc.dir must not turn a test unit into production.
                outputs = re.findall(r'''(?:^|\s)(?:-o\s+|/Fo)("[^"]*"|'[^']*'|[^\s]+)''', command)
                output = outputs[0].strip('"\'') if len(outputs) == 1 else None
        if not isinstance(output, str):
            raise RuntimeError('missing target in coverage compilation inventory')
        if not target.search(output.replace('\\', '/')):
            continue
        path = Path(row['file'])
        if not path.is_absolute():
            if not isinstance(row.get('directory'), str):
                raise RuntimeError('relative coverage source lacks its build directory')
            path = Path(row['directory']) / path
        path = path.resolve()
        if path.suffix in ('.c', '.cc', '.cpp', '.cxx') and path.name not in diagnostic_only:
            compiled.add(path)
    # These core objects must not vanish merely because the database is empty,
    # truncated, or contains a test-only compilation of the same source.
    core = {directory / name for name in ('bc.cc', 'vm.cc', 'verification.cc', 'lexer.cc', 'parser.cc')
            if (directory / name).exists()}
    missing = core - compiled
    if missing:
        raise RuntimeError('missing production compilation inventory: ' +
                           ', '.join(sorted(path.name for path in missing)))
    return compiled | headers


def require_production_mappings(source, mapped, build=None):
    missing = production_mappings(source, build) - mapped
    if missing:
        raise RuntimeError("missing production mappings; use the default static-library build: " +
                           ", ".join(sorted(path.name for path in missing)))


def coverage_tests(tests):
    """Reject incomplete selected builds before executing or merging profiles."""
    selected = [test for test in tests if test['name'].startswith('bc/') or
                any('test-libbc' in part for part in (test.get('command') or []))]
    if not selected:
        raise RuntimeError('no bc tests discovered')
    for test in selected:
        command = test.get('command')
        if (not isinstance(command, list) or not command or
                not all(isinstance(part, str) for part in command) or
                shutil.which(command[0]) is None):
            raise RuntimeError('selected coverage test is not runnable; build its executable first: ' +
                               test['name'])
    return selected


def build_modes(cache):
    # A missing option denotes older source; do not silently label it OFF.
    return {"extensions": cache.get("ENABLE_NON_POSIX_EXTENSIONS"),
            "prepared_divisors": cache.get("ENABLE_BC_PREPARED_DIVISORS"),
            "compact_execution": cache.get("ENABLE_BC_COMPACT_EXECUTION"),
            "typed_results": cache.get("ENABLE_BC_TYPED_RESULTS")}


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
    selected = coverage_tests(tests)
    (output / "selected-tests.json").write_text(json.dumps(selected, indent=2) + "\n")
    expected = production_mappings(source, build)
    database = build / 'compile_commands.json'
    database_hash = hashlib.sha256(database.read_bytes()).hexdigest()
    shutil.copyfile(str(database), str(output / 'compile_commands.json'))
    scope = dict(schema=1, targets=['bc', 'libbc'],
                 compile_commands_sha256=database_hash,
                 collector_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                 required=sorted(str(path) for path in expected),
                 not_required=sorted(str(path) for path in production_mappings(source) - expected),
                 scope='Required-mapping guard only; full LLVM exports and raw denominators are unchanged')
    (output / 'mapping-scope.json').write_text(json.dumps(scope, indent=2) + '\n')

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
    mapped = {Path(entry["filename"]).resolve() for entry in coverage["files"]}
    scope.update(mapped=sorted(str(path) for path in mapped),
                 missing=sorted(str(path) for path in expected - mapped))
    (output / 'mapping-scope.json').write_text(json.dumps(scope, indent=2) + '\n')
    if hashlib.sha256(database.read_bytes()).hexdigest() != database_hash:
        raise RuntimeError('coverage compilation inventory changed during collection')
    require_production_mappings(source, mapped, build)
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
        **build_modes(cache),
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
