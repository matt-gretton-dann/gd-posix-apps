#!/usr/bin/env python3
"""Check bounded benchmark execution, output validation and paired statistics."""

import argparse
import contextlib
import importlib.util
import io
import hashlib
import json
import re
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


class BenchmarkCli(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.invocations = 0
        self.driver = self.root / "fake bc's tool.py"
        self.driver.write_text(
            "import sys, time\n"
            "from pathlib import Path\n"
            "mode = sys.argv[1]\n"
            "source = sys.stdin.read()\n"
            "if mode == 'warmup':\n"
            "    marker = Path(sys.argv[2])\n"
            "    mode = 'ok' if marker.exists() else 'wrong'\n"
            "    marker.write_text('used')\n"
            "if mode == 'timeout': time.sleep(10)\n"
            "if mode == 'exit':\n"
            "    sys.stderr.write('deliberate child failure\\n')\n"
            "    sys.exit(23)\n"
            "sys.stdout.write('3\\n' if mode == 'wrong' else '2\\n')\n"
            "if mode == 'stderr': sys.stderr.write('unexpected warning\\n')\n",
            encoding="utf-8")

    def invoke(self, mode, timeout=3, duplicate=False, names=("fake",), samples=1,
               warmups=0, extra=(), metadata=None):
        entries = [{"name": name, "argv": [sys.executable, str(self.driver), mode,
                                           str(self.root / "marker")],
                    "metadata": metadata or {}} for name in names]
        implementation = entries[0]
        config = self.root / "implementations.json"
        config.write_text(json.dumps({"implementations":
                                     [implementation, implementation] if duplicate else
                                     entries}), encoding="utf-8")
        self.invocations += 1
        output = self.root / ("results-" + str(self.invocations))
        try:
            result = subprocess.run(
                [sys.executable, BENCHMARK, "--config", str(config), "--output", str(output),
                 "--workload", "startup", "--samples", str(samples), "--warmups", str(warmups),
                 "--timeout", str(timeout)] + list(extra),
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                universal_newlines=True, timeout=8)
        except subprocess.TimeoutExpired:
            self.fail("benchmark did not terminate within the test's outer deadline")
        return result, output

    def test_checked_success_and_quoted_argv(self):
        result, output = self.invoke("ok")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        summary = json.loads((output / "summary.json").read_text(encoding="utf-8"))
        self.assertEqual(summary["workloads"][0]["name"], "startup")
        self.assertEqual(summary["workloads"][0]["implementations"]["fake"]["samples"], 1)
        rows = [json.loads(line) for line in
                (output / "samples.jsonl").read_text(encoding="utf-8").splitlines()]
        self.assertEqual(len(rows), 1)
        self.assertTrue(rows[0]["checked"])
        self.assertGreater(rows[0]["seconds"], 0)
        manifest = json.loads((output / "manifest.json").read_text(encoding="utf-8"))
        self.assertEqual(len(manifest["implementations"][0]["binary_sha256"]), 64)
        for case in manifest['corpus']:
            for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                artifact = output / 'workloads' / (case['name'] + suffix)
                self.assertEqual(hashlib.sha256(artifact.read_bytes()).hexdigest(), case[field])

    def test_artifact_hashes_survive_windows_text_translation(self):
        config = self.root / 'windows-config.json'
        config.write_text(json.dumps({'implementations': [
            {'name': 'fake', 'argv': [sys.executable, str(self.driver), 'ok']}]}), encoding='utf-8')
        output = self.root / 'windows-artifacts'
        spec = importlib.util.spec_from_file_location('bc_benchmark_windows', BENCHMARK)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        original = Path.write_text

        def translated(path, data, *args, **kwargs):
            if path.suffix in ('.bc', '.expected'):
                return path.write_bytes(data.replace('\n', '\r\n').encode('utf-8'))
            return original(path, data, *args, **kwargs)

        argv = [BENCHMARK, '--config', str(config), '--output', str(output),
                '--workload', 'startup', '--samples', '1', '--warmups', '0']
        with mock.patch.object(Path, 'write_text', translated), \
                mock.patch.object(sys, 'argv', argv), contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(module.main(), 0)
        manifest = json.loads((output / 'manifest.json').read_text())
        case = manifest['corpus'][0]
        for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
            artifact = output / 'workloads' / ('startup' + suffix)
            self.assertEqual(hashlib.sha256(artifact.read_bytes()).hexdigest(), case[field])

    def test_native_quick_corpus(self):
        if BC_BINARY is None:
            self.skipTest("native executable not supplied")
        config = self.root / "native.json"
        config.write_text(json.dumps({"implementations": [{"name": "gd", "argv": [BC_BINARY]}]}),
                          encoding="utf-8")
        output = self.root / "native-results"
        try:
            result = subprocess.run(
                [sys.executable, BENCHMARK, "--config", str(config), "--output", str(output),
                 "--preset", "quick", "--samples", "1", "--warmups", "0", "--timeout", "5"],
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, timeout=20)
        except subprocess.TimeoutExpired:
            self.fail("native corpus exceeded the outer test deadline")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        rows = [json.loads(line) for line in
                (output / "samples.jsonl").read_text(encoding="utf-8").splitlines()]
        self.assertEqual(len(rows), 56)
        self.assertTrue(all(row["checked"] for row in rows))
        manifest = json.loads((output / "manifest.json").read_text(encoding="utf-8"))
        files = [case for case in manifest['corpus'] if case['group'] == 'file_streaming']
        self.assertEqual(len(files), 1)
        self.assertEqual(files[0]['input_mode'], 'file')
        artifact = output / 'workloads' / (files[0]['name'] + '.bc')
        self.assertEqual(hashlib.sha256(artifact.read_bytes()).hexdigest(),
                         files[0]['program_sha256'])

    def check_failure(self, mode, diagnostic, timeout=3):
        result, output = self.invoke(mode, timeout)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn(diagnostic, result.stderr)
        failure = json.loads((output / "failure.json").read_text(encoding="utf-8"))
        self.assertIn(diagnostic, failure["error"])

    def test_reject_child_failure(self):
        self.check_failure("exit", "exited with status 23")

    def test_reject_incorrect_output(self):
        self.check_failure("wrong", "incorrect output")

    def test_reject_stderr(self):
        self.check_failure("stderr", "wrote to stderr")

    def test_timeout_is_enforced(self):
        self.check_failure("timeout", "timed out", timeout=0.05)

    def test_duplicate_implementations_are_rejected(self):
        result, _ = self.invoke("ok", duplicate=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("duplicate implementation", result.stderr)

    def test_warmup_output_is_also_checked(self):
        result, _ = self.invoke("warmup", warmups=1)
        self.assertEqual(result.returncode, 1)
        self.assertIn("incorrect output", result.stderr)

    def test_strict_provenance_and_instrumented_binary_rejection(self):
        result, _ = self.invoke("ok", extra=("--require-provenance",))
        self.assertEqual(result.returncode, 1)
        self.assertIn("lacks source/compiler/flags/version provenance", result.stderr)
        result, _ = self.invoke("ok", metadata={"instrumented": True})
        self.assertEqual(result.returncode, 1)
        self.assertIn("separate timing binary", result.stderr)

    def test_pair_schedule_is_replayable(self):
        schedules = []
        for _ in range(2):
            result, output = self.invoke("ok", names=("first", "second"), samples=3)
            self.assertEqual(result.returncode, 0, result.stderr)
            rows = [json.loads(line) for line in
                    (output / "samples.jsonl").read_text(encoding="utf-8").splitlines()]
            schedules.append([(row["round"], row["order"], row["implementation"])
                              for row in rows])
            for round_number in range(3):
                block = [row["implementation"] for row in rows if row["round"] == round_number]
                self.assertCountEqual(block, ["first", "second"])
            summary = json.loads((output / "summary.json").read_text(encoding="utf-8"))
            paired = summary["workloads"][0]["candidate_over_reference"]["second"]
            self.assertEqual(paired["pairs"], 3)
            self.assertIsNotNone(paired["ci95"])
        self.assertEqual(schedules[0], schedules[1])

    def test_math_cli_preserves_enclosures_and_state_in_artifacts(self):
        library = self.root / 'ordinary library.bc'
        library.write_bytes(b'scale=20\n')
        config = self.root / 'math-config.json'
        config.write_text(json.dumps({'implementations': [
            {'name': 'fake-math', 'argv': [sys.executable, '-c',
                                         "print('.84\\n2\\n10\\n2')", str(library)],
             'input_artifacts': [dict(path=str(library), purpose='ordinary library',
                                      sha256=hashlib.sha256(library.read_bytes()).hexdigest())]}]}))
        output = self.root / 'math-results'
        result = subprocess.run(
            [sys.executable, BENCHMARK, '--suite', 'math', '--preset', 'quick',
             '--workload', 'math_regular_s-one_scale2_repeated_x2',
             '--config', str(config), '--output', str(output),
             '--samples', '1', '--warmups', '1', '--calibrate-seconds', '.000001'],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, timeout=8)
        self.assertEqual(result.returncode, 0, result.stderr)
        manifest = json.loads((output / 'manifest.json').read_text())
        self.assertEqual(manifest['suite'], 'math')
        self.assertEqual(len(manifest['oracle_checker_sha256']), 64)
        self.assertEqual(len(manifest['reference_fixture_sha256']), 64)
        self.assertIn('duration_calibration', manifest)
        declared, = manifest['implementations'][0]['input_artifacts']
        copied = output / declared['archived_as']
        self.assertEqual(copied.read_bytes(), library.read_bytes())
        self.assertEqual(hashlib.sha256(copied.read_bytes()).hexdigest(), declared['sha256'])
        entry, = manifest['corpus']
        payload = (output / 'workloads' / (entry['name'] + '.expected')).read_bytes()
        self.assertEqual(hashlib.sha256(payload).hexdigest(), entry['oracle_sha256'])
        oracle = json.loads(payload)
        self.assertEqual(oracle['kind'], 'decimal-ulp')
        self.assertEqual(oracle['expected_state'], ['2', '10', '2'])
        rows = [json.loads(line) for line in (output / 'samples.jsonl').read_text().splitlines()]
        self.assertEqual(len(rows), 2)
        self.assertTrue(all(row['checked'] for row in rows))
        calibration = [json.loads(line) for line in
                       (output / 'calibration/samples.jsonl').read_text().splitlines()]
        self.assertEqual(len(calibration), 3)
        self.assertTrue(all(row['checked'] for row in calibration))
        replay_output = self.root / 'math-replayed'
        replay = subprocess.run(
            [sys.executable, BENCHMARK, '--suite', 'math', '--replay-corpus', str(output),
             '--config', str(config), '--output', str(replay_output),
             '--samples', '1', '--warmups', '0'],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, timeout=8)
        self.assertEqual(replay.returncode, 0, replay.stderr)
        repeated = json.loads((replay_output / 'manifest.json').read_text())
        self.assertEqual(repeated['preset'], 'quick')
        self.assertEqual(repeated['corpus'], manifest['corpus'])
        self.assertEqual(repeated['replayed_corpus']['source_manifest_sha256'],
                         hashlib.sha256((output / 'manifest.json').read_bytes()).hexdigest())
        self.assertFalse((replay_output / 'calibration').exists())
        program = output / 'workloads' / (entry['name'] + '.bc')
        program.write_bytes(program.read_bytes() + b'1\n')
        invalid_output = self.root / 'math-invalid-replay'
        invalid = subprocess.run(
            [sys.executable, BENCHMARK, '--suite', 'math', '--replay-corpus', str(output),
             '--config', str(config), '--output', str(invalid_output), '--samples', '1'],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, timeout=8)
        self.assertEqual(invalid.returncode, 1)
        self.assertIn('artifact hash mismatch', invalid.stderr)
        self.assertTrue((invalid_output / 'failure.json').exists())
        self.assertFalse((invalid_output / 'samples.jsonl').exists())
        prepared = self.root / 'math-prepared'
        preparation = subprocess.run(
            [sys.executable, BENCHMARK, '--suite', 'math', '--preset', 'quick',
             '--workload', 'math_regular_s-one_scale2_repeated_x2', '--config', str(config),
             '--output', str(prepared), '--calibrate-seconds', '.000001', '--calibrate-only'],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, timeout=8)
        self.assertEqual(preparation.returncode, 0, preparation.stderr)
        self.assertEqual(json.loads((prepared / 'calibration-only.json').read_text())['workloads'], 1)
        self.assertTrue((prepared / 'setup.json').exists())
        self.assertFalse((prepared / 'samples.jsonl').exists())


class PairedStatistics(unittest.TestCase):
    def module(self):
        spec = importlib.util.spec_from_file_location("bc_benchmark", BENCHMARK)
        module = importlib.util.module_from_spec(spec)
        try:
            with contextlib.redirect_stderr(io.StringIO()):
                spec.loader.exec_module(module)
        except SystemExit:
            self.fail("benchmark module starts its CLI while being imported")
        return module

    def test_declared_inputs_are_verified_before_starting_a_child(self):
        module = self.module()
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            library = root / 'math source.bc'
            library.write_bytes(b'scale=20\n')
            config = root / 'implementations.json'
            entry = dict(name='fake', argv=[sys.executable, '-c', 'print(2)', str(library)],
                         input_artifacts=[dict(path=str(library),
                                               sha256=hashlib.sha256(library.read_bytes()).hexdigest())])
            config.write_text(json.dumps(dict(implementations=[entry])))
            tool, = module.implementations(config)
            case = dict(name='input-check', program='1+1\n', expected=['2'])
            self.assertTrue(module.run_checked(tool, case, 3)['checked'])
            library.write_bytes(b'scale=21\n')
            with mock.patch.object(module.subprocess, 'run') as child:
                with self.assertRaisesRegex(module.BenchmarkError, 'artifact changed'):
                    module.run_checked(tool, case, 3)
                child.assert_not_called()
            with self.assertRaisesRegex(module.BenchmarkError, 'hash does not match'):
                module.implementations(config)

    def test_stale_declared_binary_identity_is_rejected(self):
        module = self.module()
        actual = hashlib.sha256(Path(sys.executable).read_bytes()).hexdigest()
        with tempfile.TemporaryDirectory() as temporary:
            config = Path(temporary) / 'implementations.json'
            for location in ('metadata', 'entry', 'contradictory'):
                with self.subTest(location=location):
                    entry = dict(name='fixture', argv=[sys.executable],
                                 metadata={'instrumented': False})
                    if location == 'metadata':
                        entry['metadata']['binary_sha256'] = '0' * 64
                    else:
                        entry['binary_sha256'] = '0' * 64
                    if location == 'contradictory':
                        entry['metadata']['binary_sha256'] = actual
                    config.write_text(json.dumps(dict(implementations=[entry])))
                    with self.assertRaisesRegex(module.BenchmarkError, 'binary.*hash'):
                        module.implementations(config)

    def test_strict_binary_provenance_requires_a_declared_identity(self):
        module = self.module()
        with tempfile.TemporaryDirectory() as temporary:
            config = Path(temporary) / 'implementations.json'
            entry = dict(name='fixture', argv=[sys.executable],
                         metadata=dict(source_revision='unit-fixture', compiler='unit-fixture',
                                       flags='unit-fixture', version='unit-fixture', instrumented=False))
            config.write_text(json.dumps(dict(implementations=[entry])))
            with self.assertRaisesRegex(module.BenchmarkError, 'provenance'):
                module.implementations(config, True)
            actual = hashlib.sha256(Path(sys.executable).read_bytes()).hexdigest()
            entry['metadata']['binary_sha256'] = actual
            config.write_text(json.dumps(dict(implementations=[entry])))
            controlled, = module.implementations(config, True)
            self.assertTrue(controlled['complete_provenance'])
            self.assertEqual(controlled['binary_sha256'], entry['metadata']['binary_sha256'])
            del entry['metadata']['binary_sha256']
            entry['binary_sha256'] = actual.upper()
            config.write_text(json.dumps(dict(implementations=[entry])))
            controlled, = module.implementations(config, True)
            self.assertTrue(controlled['complete_provenance'])
            self.assertEqual(controlled['binary_sha256'], actual)
            entry['metadata']['binary_sha256'] = 'malformed'
            config.write_text(json.dumps(dict(implementations=[entry])))
            with self.assertRaisesRegex(module.BenchmarkError, 'binary.*hash'):
                module.implementations(config)

    def test_installed_fallback_preserves_observed_identity_without_controlled_provenance(self):
        module = self.module()
        with tempfile.TemporaryDirectory() as temporary:
            config = Path(temporary) / 'implementations.json'
            config.write_text(json.dumps(dict(implementations=[dict(
                name='installed', role='installed', argv=[sys.executable])])))
            entry, = module.implementations(config, True)
            self.assertFalse(entry['complete_provenance'])
            actual = hashlib.sha256(Path(sys.executable).read_bytes()).hexdigest()
            self.assertEqual(entry['binary_sha256'], actual)

    def test_legacy_full_corpus_retains_its_programs_and_oracles(self):
        module = self.module()
        baseline = Path(__file__).resolve().parents[2] / (
            'docs/utilities/bc-performance-baseline-20261031/competitive-manifest.json')
        recorded = json.loads(baseline.read_text(encoding='utf-8'))['corpus']
        current = {row['name']: row for row in module.corpus('full')}
        for previous in recorded:
            case = current[previous['name']]
            self.assertEqual(hashlib.sha256(case['program'].encode()).hexdigest(),
                             previous['program_sha256'])
            oracle = ('\n'.join(case['expected']) + '\n').encode()
            self.assertEqual(hashlib.sha256(oracle).hexdigest(), previous['oracle_sha256'])

    def test_scaled_division_cases_preserve_all_original_payloads(self):
        module = self.module()
        path = Path(__file__).resolve().parent / 'fixtures/benchmark-corpus-before-scaled-division.json'
        recorded = json.loads(path.read_text())
        for preset, previous in recorded['presets'].items():
            cases = module.corpus(preset, seed=previous['seed'])
            self.assertEqual(len(cases), len(previous['cases']) + 6)
            by_name = {case['name']: case for case in cases}
            for name, digest in previous['cases'].items():
                payload = json.dumps(by_name[name], sort_keys=True, separators=(',', ':')).encode()
                self.assertEqual(hashlib.sha256(payload).hexdigest(), digest)

    def test_scaled_division_oracles_cover_scale_directions_and_holdout(self):
        from fractions import Fraction
        module = self.module()
        inventories = {}
        for preset, seed in [('quick', 197200), ('full', 197200), ('holdout', 197201)]:
            cases = [case for case in module.corpus(preset, seed=seed)
                     if case['group'].startswith('divide_scaled_')]
            self.assertEqual(len(cases), 6)
            for shape in ('dense', 'trailing_zero'):
                rows = [case for case in cases if case['parameters']['shape'] == shape]
                self.assertEqual(len(rows), 3)
                relations = set()
                for case in rows:
                    parameters = case['parameters']
                    source = re.search(r'\na=([0-9.]+)\n', case['program']).group(1)
                    value = Fraction(source) / int(parameters['divisor'])
                    scale = parameters['scale']
                    expected = Fraction(int(value * 10 ** scale), 10 ** scale)
                    self.assertEqual(Fraction(case['expected'][0]), expected)
                    self.assertEqual(len(case['expected'][0].split('.')[1]), scale)
                    delta = scale - parameters['input_scale']
                    relations.add(-1 if delta < 0 else 1 if delta > 0 else 0)
                    self.assertGreater(parameters['iterations'], 0)
                self.assertEqual(relations, {-1, 0, 1})
            inventories[preset] = cases
        self.assertNotEqual(inventories['full'][0]['parameters']['input_scale'],
                            inventories['holdout'][0]['parameters']['input_scale'])
        self.assertNotEqual(inventories['full'][0]['parameters']['divisor'],
                            inventories['holdout'][0]['parameters']['divisor'])

    def test_expanded_sqrt_cases_preserve_every_previous_case(self):
        module = self.module()
        fixture = Path(__file__).resolve().parent / 'fixtures/benchmark-corpus-before-sqrt.json'
        report = json.loads(fixture.read_text())
        for preset, recorded in report['presets'].items():
            cases = module.corpus(preset, seed=recorded['seed'])
            self.assertEqual(len(cases), 56)
            for actual, previous in zip(cases[:46], recorded['cases']):
                self.assertEqual(actual['name'], previous['name'])
                payload = json.dumps(actual, sort_keys=True, separators=(',', ':')).encode()
                self.assertEqual(hashlib.sha256(payload).hexdigest(), previous['sha256'])
            extra = cases[46:50]
            self.assertEqual([c['group'] for c in extra],
                             ['sqrt_large_scale', 'sqrt_tiny', 'sqrt_tiny', 'sqrt_large_integer'])
            for case in extra:
                parameters = case['parameters']
                self.assertGreater(parameters['iterations'], 0)
                self.assertIn('sqrt(', case['program'])
                self.assertEqual(len(case['expected']), 1)
                if case['group'] == 'sqrt_tiny':
                    places = parameters['input_scale']
                    self.assertEqual(places % 2, 0)
                    expected = module.scaled(2 * 10 ** (places // 2), places)
                    self.assertEqual(case['expected'], [expected])
                    self.assertIn('scale=0', case['program'])
                elif case['group'] == 'sqrt_large_integer':
                    root = 2 * 10 ** parameters['root_power']
                    self.assertEqual(case['expected'],
                                     [module.scaled(root * 10 ** parameters['scale'],
                                                    parameters['scale'])])
                else:
                    places = parameters['scale']
                    coefficient = int(case['expected'][0].replace('.', ''))
                    self.assertLessEqual(coefficient * coefficient, 2 * 10 ** (2 * places))
                    self.assertGreater((coefficient + 1) ** 2, 2 * 10 ** (2 * places))

    def test_nondecimal_output_classes_have_independent_roundtrip_oracles(self):
        cases = self.module().corpus('quick')
        selected = {case['group']: case for case in cases
                    if case['group'] in ('binary_output', 'octal_output')}
        self.assertEqual(set(selected), {'binary_output', 'octal_output'})
        for group, base in (('binary_output', 2), ('octal_output', 8)):
            case = selected[group]
            literal = re.match(r'a=([0-9]+)\n', case['program']).group(1)
            self.assertIn('obase={}\n'.format(base), case['program'])
            self.assertEqual(len(case['expected']), case['parameters']['lines'])
            for line in case['expected']:
                self.assertTrue(set(line) <= set('01234567'[:base]))
                self.assertEqual(int(line, base), int(literal))

    def test_large_radix_input_classes_have_independent_integer_oracles(self):
        for preset in ('full', 'holdout', 'quick'):
            selected = {c['group']: c for c in self.module().corpus(preset)
                        if c['group'] in ('binary_input', 'hex_input')}
            self.assertEqual(set(selected), {'binary_input', 'hex_input'})
            for group, base in (('binary_input', 2), ('hex_input', 16)):
                case = selected[group]
                literals = re.findall(r'^a=([0-9A-F]+)$', case['program'], re.MULTILINE)
                self.assertEqual(len(literals), case['parameters']['literals'])
                self.assertTrue(all(len(x) == case['parameters']['digits'] for x in literals))
                self.assertEqual(case['expected'], [str(int(literals[-1], base))])
                self.assertIn('ibase=A\n', case['program'])
                if preset != 'quick':
                    self.assertGreaterEqual(len(literals[0]), 65536)

    def test_temporary_chain_has_an_independent_integer_oracle(self):
        for preset in ('full', 'holdout', 'quick'):
            selected = [c for c in self.module().corpus(preset)
                        if c['group'] == 'temporary_chain']
            self.assertEqual(len(selected), 1)
            case = selected[0]
            literal = re.search(r'^a=([0-9]+)$', case['program'], re.MULTILINE).group(1)
            self.assertEqual(len(literal), case['parameters']['digits'])
            self.assertEqual(case['parameters']['terms'], 8)
            self.assertEqual(case['expected'], [str(8 * int(literal))])
            self.assertIn('i<{}'.format(case['parameters']['iterations']), case['program'])

    def test_file_class_has_an_independent_integer_oracle(self):
        for preset in ('full', 'holdout', 'quick'):
            cases = [c for c in self.module().corpus(preset)
                     if c['group'] == 'file_streaming']
            self.assertEqual(len(cases), 1)
            case, = cases
            self.assertEqual(case['input_mode'], 'file')
            count = case['parameters']['statements']
            self.assertEqual(case['program'], 's=0\n' + 's+=1\n' * count + 's\n')
            self.assertEqual(case['expected'], [str(sum(1 for _ in range(count)))])

    def test_file_transport_preserves_argv_and_closes_stdin(self):
        module = self.module()
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name)
        path = root / "source \u03bb's.bc"
        path.write_bytes(b'1+1\n')
        driver = root / 'read artifact.py'
        driver.write_text("import sys\nfrom pathlib import Path\n"
                          "assert sys.stdin.buffer.read() == b''\n"
                          "assert Path(sys.argv[1]).read_bytes() == b'1+1\\n'\n"
                          "print(2)\n", encoding='utf-8')
        case = {'name': 'file-check', 'program': '1+1\n',
                'expected': ['2'], 'input_mode': 'file'}
        result = module.run_checked({'name': 'fake',
                                     'argv': [sys.executable, str(driver)]}, case, 3, path)
        self.assertTrue(result['checked'])

    def test_file_transport_rejects_missing_or_wrong_artifact(self):
        module = self.module()
        tool = {'name': 'fake', 'argv': [sys.executable, '-c', 'print(2)']}
        case = {'name': 'file-check', 'program': '1+1\n',
                'expected': ['2'], 'input_mode': 'file'}
        with self.assertRaisesRegex(module.BenchmarkError, 'artifact'):
            module.run_checked(tool, case, 3)
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        path = Path(temporary.name) / 'wrong.bc'
        path.write_bytes(b'1+2\n')
        with self.assertRaisesRegex(module.BenchmarkError, 'artifact'):
            module.run_checked(tool, case, 3, path)

    def test_transport_rejects_unknown_mode(self):
        module = self.module()
        case = {'name': 'mode-check', 'program': '1+1\n',
                'expected': ['2'], 'input_mode': 'unknown'}
        with self.assertRaisesRegex(module.BenchmarkError, 'input mode'):
            module.run_checked({'name': 'fake',
                                'argv': [sys.executable, '-c', 'print(2)']}, case, 3)

    def test_holdout_shapes_are_separate_and_replayable(self):
        module = self.module()
        full = module.corpus('full', listing=True)
        held = module.corpus('holdout', listing=True, seed=197201)
        self.assertEqual([case['group'] for case in full], [case['group'] for case in held])
        self.assertEqual(held, module.corpus('holdout', listing=True, seed=197201))
        for training, holdout in zip(full, held):
            if training['name'] != 'startup':
                self.assertNotEqual(training['name'], holdout['name'])
                self.assertNotEqual(training['parameters'], holdout['parameters'])
                self.assertNotEqual(training['program'], holdout['program'])

    def test_exact_paired_ratio_and_replay(self):
        module = self.module()
        result = module.paired_statistics([2, 4, 6], [1, 2, 3], seed=197200, resamples=200)
        self.assertEqual(result["median_paired_ratio"], 2)
        self.assertEqual(result["ci95"], [2, 2])
        self.assertEqual(result, module.paired_statistics(
            [2, 4, 6], [1, 2, 3], seed=197200, resamples=200))

    def test_single_pair_has_no_confidence_claim(self):
        result = self.module().paired_statistics([2], [1], seed=197200, resamples=200)
        self.assertEqual(result["median_paired_ratio"], 2)
        self.assertIsNone(result["ci95"])
        self.assertEqual(result["pairs"], 1)

    def test_serial_dependence_is_reported_without_narrowing_iid_intervals(self):
        module = self.module()
        ratios = [1, 1, 1, 2, 2, 2, 3, 3, 3]
        result = module.paired_statistics(ratios, [1] * len(ratios), 216, 400)
        self.assertAlmostEqual(result['autocorrelation_lag1'], 2 / 3)
        self.assertEqual(result['block_length'], 3)
        for kind in ('iid_ci95', 'block_ci95'):
            self.assertLessEqual(result['ci95'][0], result[kind][0])
            self.assertGreaterEqual(result['ci95'][1], result[kind][1])
        self.assertEqual(result, module.paired_statistics(ratios, [1] * len(ratios), 216, 400))
        alternating = module.paired_statistics([1, 3] * 4, [1] * 8, 216, 400)
        self.assertAlmostEqual(alternating['autocorrelation_lag1'], -7 / 8)
        large = module.paired_statistics([value * 1e200 for value in ratios], [1] * 9, 216, 400)
        self.assertAlmostEqual(large['autocorrelation_lag1'], 2 / 3)
        with self.assertRaisesRegex(module.BenchmarkError, 'ratios must be finite'):
            module.paired_statistics([1e308] * 3, [1e-308] * 3, 216, 400)

    def test_math_calibration_freezes_the_slowest_required_common_count(self):
        module = self.module()
        source = module.corpus('quick', suite='math')[:2]
        roles = [dict(name='fast'), dict(name='slow')]
        def checked(tool, case, timeout):
            factor = .001 if tool['name'] == 'fast' else .004
            return dict(seconds=case['parameters']['iterations'] * factor, checked=True,
                        output_sha256='a' * 64, output_bytes=10)
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary)
            with mock.patch.object(module, 'run_checked', side_effect=checked) as invoked:
                cases, report = module.calibrate_math(source, roles, .01, 3, 16, 1, output)
            self.assertEqual(invoked.call_count, 18)
            self.assertEqual(cases[0], source[0])
            self.assertEqual(source[1]['parameters']['iterations'], 2)
            frozen = cases[1]
            self.assertEqual(frozen['parameters']['iterations'], 16)
            self.assertTrue(frozen['name'].endswith('_x16'))
            self.assertIn('i<16;', frozen['program'])
            choice = report['cases'][1]
            self.assertEqual(choice['roles']['fast']['iterations'], 16)
            self.assertEqual(choice['roles']['slow']['iterations'], 4)
            rows = [json.loads(line) for line in
                    (output / 'calibration/samples.jsonl').read_text().splitlines()]
            self.assertEqual(len(rows), 18)
            self.assertTrue(all(row['checked'] for row in rows))
            for row in rows:
                payload = (output / 'calibration/workloads' / (row['workload'] + '.bc')).read_bytes()
                self.assertEqual(hashlib.sha256(payload).hexdigest(), row['program_sha256'])
            observed = [dict(workload=frozen['name'], phase='measurement', implementation=name,
                             seconds=seconds) for name, seconds in [('fast', .005), ('slow', .1)]]
            summary = module.summarize([frozen], observed, 'fast', 216, 200)['workloads'][0]
            self.assertEqual(summary['duration_target_met'], {'fast': False, 'slow': True})
            self.assertIn('shortfall', summary['duration_status'])

    def test_calibration_cap_failure_preserves_checked_trials(self):
        module = self.module()
        case = module.corpus('quick', suite='math')[1]
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary)
            with mock.patch.object(module, 'run_checked', return_value=dict(
                    seconds=.001, checked=True, output_sha256='b' * 64, output_bytes=10)):
                with self.assertRaisesRegex(module.BenchmarkError, 'duration target at 4'):
                    module.calibrate_math([case], [dict(name='role')], .1, 1, 4, 1, output)
            rows = [json.loads(line) for line in
                    (output / 'calibration/samples.jsonl').read_text().splitlines()]
            self.assertEqual([row['iterations'] for row in rows], [2, 4])
            self.assertTrue(all(row['checked'] for row in rows))

    def test_enclosure_oracles_accept_endpoints_and_reject_bad_results(self):
        module = self.module()
        case = dict(name='enclosure', program='1+1\n', expected=None,
                    oracle=dict(kind='decimal-ulp', scale=2, lower='199/100', upper='201/100'))
        tool = dict(name='fake', argv=[sys.executable, '-c', 'print(2)'])
        self.assertTrue(module.run_checked(tool, case, 3)['checked'])
        with self.assertRaisesRegex(module.BenchmarkError, 'error bound'):
            module.run_checked(dict(tool, argv=[sys.executable, '-c', 'print(3)']), case, 3)
        uncertain = dict(case, oracle=dict(kind='decimal-ulp', scale=2,
                                          lower='201/100', upper='203/100'))
        with self.assertRaisesRegex(module.BenchmarkError, 'refinement'):
            module.run_checked(tool, uncertain, 3)
        with self.assertRaisesRegex(module.BenchmarkError, 'exactly one'):
            module.run_checked(dict(tool, argv=[sys.executable, '-c', "print('2\\n2')"]), case, 3)
        payload = json.loads(module.oracle_payload(case))
        self.assertEqual(payload, case['oracle'])
        with self.assertRaisesRegex(module.BenchmarkError, 'unknown numerical oracle'):
            module.run_checked(tool, dict(case, oracle={'kind': 'floating-tolerance'}), 3)
        state_case = dict(case, oracle=dict(case['oracle'], expected_state=['2', '10', '2']))
        state_tool = dict(tool, argv=[sys.executable, '-c', "print('2\\n2\\n10\\n2')"])
        self.assertTrue(module.run_checked(state_tool, state_case, 3)['checked'])
        with self.assertRaisesRegex(module.BenchmarkError, 'caller state'):
            module.run_checked(dict(tool, argv=[sys.executable, '-c', "print('2\\n0\\n10\\n2')"]),
                               state_case, 3)

    def test_optional_math_suite_preserves_arithmetic_and_records_all_dimensions(self):
        module = self.module()
        original = module.corpus('quick')
        full = module.corpus('full', suite='math')
        held = module.corpus('holdout', suite='math')
        quick = module.corpus('quick', suite='math')
        self.assertEqual(module.corpus('quick'), original)
        self.assertEqual(len(full), 60)
        self.assertEqual([case['group'] for case in full], [case['group'] for case in held])
        self.assertEqual({case['parameters']['function'] for case in full}, set('scalej'))
        self.assertEqual({case['parameters']['scale'] for case in full}, {20, 100, 1000})
        self.assertEqual({case['parameters']['scale'] for case in quick}, {2, 20, 100})
        for training, holdout in zip(full, held):
            self.assertNotEqual(training['name'], holdout['name'])
            self.assertNotEqual(training['program'], holdout['program'])
            for case in (training, holdout):
                self.assertEqual(case['oracle']['kind'], 'decimal-ulp')
                self.assertEqual(case['oracle']['scale'], case['parameters']['scale'])
                self.assertIn(str(case['parameters']['iterations']), case['name'])
                self.assertEqual(case['parameters']['calibration'], 'fixed count; not calibrated')

    def test_math_holdout_changes_magnitudes_and_preserves_full_cases(self):
        from fractions import Fraction
        module = self.module()
        fixtures = Path(__file__).resolve().parent / 'fixtures'
        before = json.loads((fixtures / 'math-benchmark-before-heldout.json').read_text())
        full, held = module.corpus('full', suite='math'), module.corpus('holdout', suite='math')
        for case, previous, holdout in zip(full, before['corpus'], held):
            self.assertEqual(case['name'], previous['name'])
            payload = json.dumps(case, sort_keys=True, separators=(',', ':')).encode()
            self.assertEqual(hashlib.sha256(payload).hexdigest(), previous['sha256'])
            self.assertNotEqual([abs(Fraction(x)) for x in case['parameters']['arguments']],
                                [abs(Fraction(x)) for x in holdout['parameters']['arguments']])
        report = json.loads((fixtures / 'math-benchmark-oracles.json').read_text())
        source = fixtures / 'math-benchmark-cases.json'
        specification = json.loads(source.read_text())
        self.assertEqual(report['case_file_sha256'], hashlib.sha256(source.read_bytes()).hexdigest())
        generator = fixtures.parent / 'scripts/generate-math-oracles.py'
        self.assertEqual(report['generator_sha256'], hashlib.sha256(generator.read_bytes()).hexdigest())
        expected = {(case['id'], scale) for case in specification['cases']
                    for scale in specification['scales']}
        self.assertEqual(len(report['cases']), len(expected))
        self.assertEqual({(case['id'], case['scale']) for case in report['cases']}, expected)
        for case in report['cases']:
            lower, upper = Fraction(case['lower']), Fraction(case['upper'])
            self.assertLessEqual(lower, upper)
            self.assertLessEqual(upper - lower, Fraction(1, 10 ** (case['scale'] + report['guard_digits'])))
            self.assertEqual(lower.denominator & (lower.denominator - 1), 0)
            self.assertEqual(upper.denominator & (upper.denominator - 1), 0)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("benchmark")
    parser.add_argument("--bc", help="native executable for the checked quick corpus")
    args, remaining = parser.parse_known_args()
    BENCHMARK = str(Path(args.benchmark).resolve())
    BC_BINARY = str(Path(args.bc).resolve()) if args.bc else None
    unittest.main(argv=[__file__, *remaining])
