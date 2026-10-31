#!/usr/bin/env python3
"""Run output-checked, bounded and paired bc process benchmarks.

Use --list to inspect the corpus. A JSON configuration supplies named argv lists;
--output receives a new directory containing programs, oracles, raw samples and
provenance. Every warm-up and measured invocation must pass its exact oracle.
Python 3.6+; no shell command construction or optional numerical dependencies.
SPDX-License-Identifier: Apache-2.0
"""

import argparse
import csv
from datetime import datetime, timezone
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import random
import re
import shutil
import statistics
import subprocess
import sys
import time


class BenchmarkError(RuntimeError):
    """Invalid experiment or a failed correctness/process check."""


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def digest_file(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def integer_sqrt(value):
    """Exact floor root, also on Python versions predating math.isqrt."""
    if value == 0:
        return 0
    root = 1 << ((value.bit_length() + 1) // 2)
    while True:
        next_root = (root + value // root) // 2
        if next_root >= root:
            return root
        root = next_root


def scaled(coefficient, places):
    if coefficient == 0:
        return "0"
    sign = "-" if coefficient < 0 else ""
    text = str(abs(coefficient))
    if places:
        text = text.zfill(places + 1)
        text = text[:-places] + "." + text[-places:]
    return sign + text


def fibonacci(count):
    first, second = 0, 1
    for _ in range(count):
        first, second = second, first + second
    return first


def corpus(preset="full", selected=None, listing=False, seed=197200):
    """Keep the historical sixteen cases, then add explicitly named classes.

    Quick mode reduces both dimensions and iterations; its parameterized names
    and manifest distinguish it from the full baseline. Oracles are evaluated
    only for selected cases, so startup-only validation stays inexpensive.
    """
    if hasattr(sys, "set_int_max_str_digits"):
        sys.set_int_max_str_digits(100000)
    rng = random.Random(seed)
    cases, known = [], []
    def iterations(count):
        if preset == "quick":
            return max(1, count // 100)
        return max(1, count - count // 8) if preset == "holdout" else count

    def dimension(count):
        if preset == "quick":
            return max(1, count // 10)
        return count + max(1, count // 8) if preset == "holdout" else count

    def add(name, group, program, oracle, parameters=None):
        known.append(name)
        if selected and name not in selected:
            return
        expected = None if listing else (oracle() if callable(oracle) else oracle)
        if expected is not None:
            expected = ([str(value) for value in expected] if isinstance(expected, list)
                        else [str(expected)])
        cases.append({"name": name, "group": group, "program": program.rstrip() + "\n",
                      "expected": expected, "parameters": parameters or {}})

    def dense(digits):
        return str(rng.randrange(1, 10)) + "".join(str(rng.randrange(10)) for _ in range(digits - 1))

    def multiply(name, left, right, repeats, shape):
        add(name, "multiply", "a={}\nb={}\nfor(i=0;i<{};i++) c=a*b\nc".format(
            left, right, repeats), lambda: int(left) * int(right),
            {"left_digits": len(left), "right_digits": len(right),
             "iterations": repeats, "shape": shape})

    add("startup", "startup", "1+1", 2)
    count = iterations(1000000)
    add("loop_{}".format(count), "loop", "s=0\nfor(i=0;i<{};i++) s+=i\ns".format(count),
        lambda: count * (count - 1) // 2, {"iterations": count})
    count = iterations(200000)
    add("calls_{}".format(count), "calls",
        "define f(x) {\nreturn(x*x+1)\n}\ns=0\nfor(i=0;i<%d;i++) s+=f(i)\ns" % count,
        lambda: count * (count - 1) * (2 * count - 1) // 6 + count, {"iterations": count})
    add("array_{}".format(count), "array",
        "s=0\nfor(i=0;i<%d;i++) a[i]=i\nfor(i=0;i<%d;i++) s+=a[i]\ns" % (count, count),
        lambda: count * (count - 1) // 2, {"elements": count})
    digits, repeats = dimension(1000), iterations(100000)
    left = "7" * digits
    for position, right in (("high", "8" + left[1:]),
                            ("low", left[:-1] + "8"), ("equal", left)):
        add("compare_{}_{}d_x{}".format(position, digits, repeats), "comparison",
            "a={}\nb={}\ns=0\nfor(i=0;i<{};i++) {{if(a<=b) s+=1}}\ns".format(left, right, repeats),
            lambda: repeats, {"digits": digits, "iterations": repeats,
                              "first_unequal_limb": position})
    for size, repeats in ((1000, 100), (10000, 10)):
        size, repeats = dimension(size), iterations(repeats)
        body = Path(__file__).with_name("add-bench.bc").read_text(encoding="utf-8")
        add("fibonacci_{}_x{}".format(size, repeats), "fibonacci",
            body + "\nfor(k=0;k<%d;k++) e=f(%d)\ne" % (repeats, size),
            lambda: fibonacci(size), {"index": size, "iterations": repeats})
    for size, repeats in ((1000, 50), (5000, 5)):
        size, repeats = dimension(size), iterations(repeats)
        body = Path(__file__).with_name("mul-bench.bc").read_text(encoding="utf-8")
        add("factorial_{}_x{}".format(size, repeats), "factorial",
            body + "\nfor(k=0;k<%d;k++) e=f(%d)\ne" % (repeats, size),
            lambda: math.factorial(size), {"argument": size, "iterations": repeats})
    for digits, repeats in ((100, 100000), (1000, 10000), (10000, 1000)):
        digits, repeats = dimension(digits), iterations(repeats)
        multiply("multiply_{}d_x{}".format(digits, repeats), "9" * digits, "8" * digits,
                 repeats, "repeated")
    for places, repeats in ((100, 100000), (1000, 10000), (5000, 5000)):
        places, repeats = dimension(places), iterations(repeats)
        add("divide_scale{}_x{}".format(places, repeats), "divide_single_limb",
            "scale=%d\nfor(i=0;i<%d;i++) c=1/7\nc" % (places, repeats),
            lambda: scaled(10 ** places // 7, places),
            {"scale": places, "iterations": repeats, "divisor": "7"})
    for places, repeats in ((100, 5000), (1000, 50), (5000, 5)):
        places, repeats = dimension(places), iterations(repeats)
        add("sqrt_scale{}_x{}".format(places, repeats), "sqrt",
            "scale=%d\nfor(i=0;i<%d;i++) c=sqrt(2)\nc" % (places, repeats),
            lambda: scaled(integer_sqrt(2 * 10 ** (2 * places)), places),
            {"scale": places, "iterations": repeats})

    for digits, repeats in ((1000, 5000), (10000, 500)):
        digits, repeats = dimension(digits), iterations(repeats)
        multiply("multiply_dense_{}d_x{}".format(digits, repeats), dense(digits), dense(digits),
                 repeats, "seeded_dense")
    large, small, repeats = dimension(10000), dimension(100), iterations(2000)
    multiply("multiply_unbalanced_{}d_{}d_x{}".format(large, small, repeats),
             dense(large), dense(small), repeats, "unbalanced")
    digits, repeats = dimension(10000), iterations(1000)
    sparse = "1" + "0" * (digits - 2) + "1"
    multiply("multiply_sparse_{}d_x{}".format(digits, repeats), sparse, sparse,
             repeats, "sparse")
    for left_size, right_size, places, repeats in ((900, 450, 100, 1000),
                                                  (1000, 900, 1000, 500)):
        left_size, right_size = dimension(left_size), dimension(right_size)
        places, repeats = dimension(places), iterations(repeats)
        left, right = dense(left_size), dense(right_size)
        add("divide_multilimb_{}d_{}d_s{}_x{}".format(left_size, right_size, places, repeats),
            "divide_multilimb", "scale={}\na={}\nb={}\nfor(i=0;i<{};i++) c=a/b\nc".format(
                places, left, right, repeats),
            lambda: scaled(int(left) * 10 ** places // int(right), places),
            {"dividend_digits": left_size, "divisor_digits": right_size,
             "scale": places, "iterations": repeats})
    left_size, right_size, repeats = dimension(1000), dimension(350), iterations(3000)
    left, right = dense(left_size), dense(right_size)
    add("modulo_{}d_{}d_x{}".format(left_size, right_size, repeats), "modulo",
        "a={}\nb={}\nfor(i=0;i<{};i++) c=a%b\nc".format(left, right, repeats),
        lambda: int(left) % int(right), {"dividend_digits": left_size,
                                       "divisor_digits": right_size, "iterations": repeats})
    for digits, exponent, repeats in ((100, 17, 1000), (20, 32, 2000)):
        digits, repeats = dimension(digits), iterations(repeats)
        left = dense(digits)
        add("power_{}d_e{}_x{}".format(digits, exponent, repeats), "power",
            "a={}\nfor(i=0;i<{};i++) c=a^{}\nc".format(left, repeats, exponent),
            lambda: int(left) ** exponent,
            {"digits": digits, "exponent": exponent, "iterations": repeats})
    count = iterations(20000)
    add("ibase_literal_flip_{}".format(count), "literal_base",
        "define f() {\nreturn(10)\n}\ns=0\nfor(i=0;i<%d;i++) {\n"
        "ibase=2\ns+=f()\nibase=A\ns+=f()\n}\ns" % count,
        lambda: 12 * count, {"iterations": count, "bases": [2, 10]})
    depth, repeats = dimension(50), iterations(1000)
    add("recursion_{}_x{}".format(depth, repeats), "recursion",
        "define f(n) {\nif(n<=1) return(1)\nreturn(n*f(n-1))\n}\n"
        "for(i=0;i<%d;i++) a=f(%d)\na" % (repeats, depth),
        lambda: math.factorial(depth), {"depth": depth, "iterations": repeats})
    count = iterations(200000)
    add("sparse_array_{}".format(count), "array_sparse",
        "s=0\nfor(i=0;i<%d;i++) {\nk=(i%%31)*313\na[k]=i\ns+=a[k]\n}\ns" % count,
        lambda: count * (count - 1) // 2, {"iterations": count, "keys": 31, "stride": 313})
    count = iterations(2000)
    add("frontend_definitions_{}".format(count), "frontend_definition",
        "define f(x) {\nreturn(x*x+1)\n}\n" * count + "1", 1, {"definitions": count})
    digits, count = dimension(4000), iterations(100)
    value = dense(digits)
    add("decimal_input_{}d_x{}".format(digits, count), "decimal_input",
        ("a=" + value + "\n") * count + "a", value,
        {"digits": digits, "literals": count})
    digits, count = dimension(200), iterations(1000)
    value = dense(digits)
    add("decimal_output_{}d_x{}".format(digits, count), "decimal_output",
        "a={}\nfor(i=0;i<{};i++) a".format(value, count), lambda: [value] * count,
        {"digits": digits, "lines": count})
    count = iterations(20000)
    add("streaming_statements_{}".format(count), "streaming",
        "i=0\n" + "i+=1\ni\n" * count, lambda: list(range(1, count + 1)),
        {"statements": count * 2 + 1, "output_lines": count, "termination": "EOF"})
    if selected:
        missing = set(selected) - set(known)
        if missing:
            raise BenchmarkError("unknown workload: " + ", ".join(sorted(missing)))
    return cases


def canonical_output(data):
    text = data.decode("utf-8").replace("\\\r\n", "").replace("\\\n", "")
    result = []
    for line in text.splitlines():
        if not re.fullmatch(r"-?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+)", line):
            raise ValueError("non-numeric output")
        negative = line.startswith("-")
        if negative:
            line = line[1:]
        integer, dot, fraction = line.partition(".")
        integer = integer.lstrip("0") or "0"
        zero = integer == "0" and (not fraction or set(fraction) == {"0"})
        result.append("0" if zero else ("-" if negative else "") + integer + dot + fraction)
    return result


def environment():
    result = {key: value for key, value in os.environ.items()
              if not key.startswith(("BC_", "DC_"))}
    result.update(LC_ALL="C", LANG="C", BC_LINE_LENGTH="70", BC_ENV_ARGS="")
    return result


def run_checked(implementation, case, timeout):
    label = implementation["name"] + " / " + case["name"]
    payload, child_environment = case["program"].encode("utf-8"), environment()
    started = time.perf_counter()
    try:
        child = subprocess.run(implementation["argv"], input=payload,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               env=child_environment, timeout=timeout)
    except subprocess.TimeoutExpired:
        # subprocess.run kills and waits for its direct child on timeout.
        raise BenchmarkError(label + " timed out after {} seconds".format(timeout))
    except OSError as error:
        raise BenchmarkError(label + " could not start: " + str(error))
    elapsed = time.perf_counter() - started
    if child.returncode:
        raise BenchmarkError(label + " exited with status " + str(child.returncode))
    if child.stderr:
        raise BenchmarkError(label + " wrote to stderr: " +
                             child.stderr.decode("utf-8", errors="replace")[:500])
    try:
        actual = canonical_output(child.stdout)
    except (UnicodeError, ValueError):
        raise BenchmarkError(label + " produced incorrect output (not numeric)")
    expected = canonical_output(("\n".join(case["expected"]) + "\n").encode("utf-8"))
    if actual != expected:
        raise BenchmarkError(label + " produced incorrect output")
    return {"seconds": elapsed, "output_bytes": len(child.stdout), "checked": True,
            "output_sha256": hashlib.sha256(child.stdout).hexdigest()}


def quantile(values, fraction):
    ordered = sorted(values)
    position = (len(ordered) - 1) * fraction
    low = int(position)
    weight = position - low
    return ordered[low] * (1 - weight) + ordered[min(low + 1, len(ordered) - 1)] * weight


def paired_statistics(candidate, reference, seed, resamples=2000):
    if not candidate or len(candidate) != len(reference):
        raise BenchmarkError("paired measurements must have equal nonzero lengths")
    if any(not math.isfinite(value) or value <= 0
           for values in (candidate, reference) for value in values):
        raise BenchmarkError("paired measurements must be finite and positive")
    if resamples < 200:
        raise BenchmarkError("at least 200 bootstrap resamples are required")
    ratios = [left / right for left, right in zip(candidate, reference)]
    result = {"pairs": len(ratios), "median_paired_ratio": statistics.median(ratios),
              "ratio_of_medians": statistics.median(candidate) / statistics.median(reference),
              "ci95": None, "method": "paired percentile bootstrap of median ratios"}
    if len(ratios) >= 3:
        rng = random.Random(seed)
        bootstraps = [statistics.median([ratios[rng.randrange(len(ratios))]
                                        for _ in ratios]) for _ in range(resamples)]
        result["ci95"] = [quantile(bootstraps, 0.025), quantile(bootstraps, 0.975)]
    else:
        result["interval_status"] = "insufficient_pairs"
    return result


def dispersion(values):
    median = statistics.median(values)
    return {"samples": len(values), "median_seconds": median, "min_seconds": min(values),
            "max_seconds": max(values),
            "mad_seconds": statistics.median([abs(value - median) for value in values])}


def implementations(path, require_provenance=False):
    try:
        entries = json.loads(path.read_text(encoding="utf-8"))["implementations"]
    except (OSError, ValueError, KeyError, TypeError) as error:
        raise BenchmarkError("invalid implementation configuration: " + str(error))
    if not isinstance(entries, list) or not entries:
        raise BenchmarkError("configuration needs at least one implementation")
    result, names = [], set()
    for entry in entries:
        if not isinstance(entry, dict) or not isinstance(entry.get("name"), str):
            raise BenchmarkError("each implementation needs a name and argv list")
        name = entry["name"]
        if not name or name in names:
            raise BenchmarkError("duplicate implementation or empty name: " + name)
        names.add(name)
        argv = entry.get("argv")
        if not isinstance(argv, list) or not argv or any(not isinstance(arg, str) for arg in argv):
            raise BenchmarkError(name + " needs a nonempty string argv list")
        binary = shutil.which(argv[0])
        if not binary:
            raise BenchmarkError(name + " executable not found: " + argv[0])
        metadata = entry.get("metadata", {})
        if not isinstance(metadata, dict):
            raise BenchmarkError(name + " metadata must be an object")
        if metadata.get("instrumented"):
            raise BenchmarkError(name + " is instrumented; use a separate timing binary")
        provenance_fields = ("source_revision", "compiler", "flags", "version")
        complete = (all(metadata.get(key) for key in provenance_fields) and
                    metadata.get("instrumented") is False)
        role = entry.get("role", "candidate")
        if require_provenance and role != "installed" and not complete:
            raise BenchmarkError(name + " lacks source/compiler/flags/version provenance")
        result.append({"name": name, "argv": [str(Path(binary).resolve())] + argv[1:],
                       "binary_sha256": digest_file(binary), "metadata": metadata,
                       "role": role, "complete_provenance": complete})
    return result


def summarize(cases, rows, reference, seed, resamples):
    workloads = []
    for index, case in enumerate(cases):
        values = {}
        for row in rows:
            if row["workload"] == case["name"] and row["phase"] == "measurement":
                values.setdefault(row["implementation"], []).append(row["seconds"])
        comparisons = {}
        for name in values:
            if name != reference:
                comparisons[name] = paired_statistics(values[name], values[reference],
                                                       seed + index, resamples)
        workloads.append({"name": case["name"], "group": case["group"],
                          "parameters": case["parameters"],
                          "implementations": {name: dispersion(times)
                                              for name, times in values.items()},
                          "candidate_over_reference": comparisons})
    return {"schema": 1, "reference": reference, "workloads": workloads,
            "performance_gate": "advisory; no aggregate or hard timing threshold"}


def positive_integer(value):
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError("must be positive")
    return number


def nonnegative_integer(value):
    number = int(value)
    if number < 0:
        raise argparse.ArgumentTypeError("must be nonnegative")
    return number


def positive_timeout(value):
    number = float(value)
    if not math.isfinite(number) or number <= 0:
        raise argparse.ArgumentTypeError("must be finite and positive")
    return number


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, help="JSON implementations with named argv lists")
    parser.add_argument("--output", type=Path, help="new experiment directory")
    parser.add_argument("--list", action="store_true", help="list case names and parameters")
    parser.add_argument("--preset", choices=("full", "quick", "holdout"), default="full")
    parser.add_argument("--workload", action="append", help="select an exact name; repeatable")
    parser.add_argument("--reference", help="reference implementation (default: first configured)")
    parser.add_argument("--samples", type=positive_integer, default=9)
    parser.add_argument("--warmups", type=nonnegative_integer, default=1)
    parser.add_argument("--timeout", type=positive_timeout, default=30)
    parser.add_argument("--seed", type=int, default=197200, help="schedule/bootstrap seed")
    parser.add_argument("--corpus-seed", type=int, default=197200, help="operand-generation seed")
    parser.add_argument("--bootstrap", type=positive_integer, default=2000)
    parser.add_argument("--require-provenance", action="store_true")
    args = parser.parse_args()
    if args.list:
        try:
            cases = corpus(args.preset, args.workload, listing=True, seed=args.corpus_seed)
        except BenchmarkError as error:
            parser.error(str(error))
        print(json.dumps([{key: case[key] for key in ("name", "group", "parameters")}
                          for case in cases], indent=2))
        return 0
    if not args.config or not args.output:
        parser.error("--config and --output are required unless --list is used")
    if args.bootstrap < 200:
        parser.error("--bootstrap must be at least 200")
    output = args.output.resolve()
    try:
        output.mkdir(parents=True, exist_ok=False)
    except OSError as error:
        parser.error("cannot create a fresh output directory: " + str(error))
    try:
        tools = implementations(args.config, args.require_provenance)
        reference = args.reference or tools[0]["name"]
        if reference not in [tool["name"] for tool in tools]:
            raise BenchmarkError("reference is not a configured implementation")
        if args.require_provenance and next(tool for tool in tools
                                           if tool["name"] == reference)["role"] == "installed":
            raise BenchmarkError("an installed comparator cannot be the controlled reference")
        cases = corpus(args.preset, args.workload, seed=args.corpus_seed)
        programs = output / "workloads"
        programs.mkdir()
        for case in cases:
            # Preserve exactly the UTF-8 LF bytes hashed below and sent to the
            # child; native text-mode newline translation would change artifacts.
            (programs / (case["name"] + ".bc")).write_bytes(case["program"].encode("utf-8"))
            (programs / (case["name"] + ".expected")).write_bytes(
                ("\n".join(case["expected"]) + "\n").encode("utf-8"))
        clock = time.get_clock_info("perf_counter")
        manifest = {"schema": 1, "started_utc": datetime.now(timezone.utc).isoformat(),
                    "implementations": tools, "reference": reference, "preset": args.preset,
                    "samples": args.samples, "warmups": args.warmups, "timeout": args.timeout,
                    "seed": args.seed, "corpus_seed": args.corpus_seed,
                    "harness_sha256": digest_file(__file__),
                    "bootstrap_resamples": args.bootstrap,
                    "platform": {"system": platform.system(), "release": platform.release(),
                                 "machine": platform.machine(), "python": platform.python_version()},
                    "clock": {"implementation": clock.implementation, "resolution": clock.resolution,
                              "monotonic": clock.monotonic},
                    "environment": {"LC_ALL": "C", "LANG": "C", "BC_LINE_LENGTH": "70",
                                    "BC_ENV_ARGS": ""},
                    "pending_classes": [{"group": "posix_math_library", "issue": 217,
                                         "status": "not implemented; not a performance pass"}],
                    "corpus": [{"name": case["name"], "group": case["group"],
                                "parameters": case["parameters"],
                                "oracle_sha256": hashlib.sha256(
                                    ("\n".join(case["expected"]) + "\n").encode("utf-8")).hexdigest(),
                                "program_sha256": hashlib.sha256(
                                    case["program"].encode("utf-8")).hexdigest()}
                               for case in cases]}
        write_json(output / "manifest.json", manifest)
        rows, rng = [], random.Random(args.seed)
        fields = ("phase", "round", "order", "workload", "implementation", "seconds",
                  "checked", "output_bytes", "output_sha256")
        with (output / "samples.jsonl").open("w", encoding="utf-8") as raw, \
                (output / "samples.csv").open("w", newline="", encoding="utf-8") as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=fields)
            writer.writeheader()
            for phase, rounds in (("warmup", args.warmups), ("measurement", args.samples)):
                for round_number in range(rounds):
                    round_cases = list(cases)
                    rng.shuffle(round_cases)
                    for case in round_cases:
                        block = list(tools)
                        rng.shuffle(block)
                        for order, tool in enumerate(block):
                            row = run_checked(tool, case, args.timeout)
                            row.update(phase=phase, round=round_number, order=order,
                                       workload=case["name"], implementation=tool["name"])
                            rows.append(row)
                            raw.write(json.dumps(row, sort_keys=True) + "\n")
                            raw.flush()
                            writer.writerow(row)
                            csv_file.flush()
        summary = summarize(cases, rows, reference, args.seed, args.bootstrap)
        write_json(output / "summary.json", summary)
        print("Checked {} workloads across {} implementations; results: {}".format(
            len(cases), len(tools), output))
        return 0
    except (BenchmarkError, OSError, ValueError) as error:
        write_json(output / "failure.json", {"error": str(error), "validation": "failed"})
        print("Benchmark failed: " + str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
