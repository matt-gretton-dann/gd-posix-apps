#!/usr/bin/env python3
"""Exercise offline control audits with synthetic, never executed binaries.
SPDX-License-Identifier: Apache-2.0
"""
import copy
from contextlib import redirect_stderr
import importlib.util
import io
import json
from pathlib import Path
import random
import tempfile
import unittest
import zipfile

SCRIPTS = Path(__file__).parent / 'scripts'
SPEC = importlib.util.spec_from_file_location('audit', SCRIPTS / 'audit-bc-controls.py')
AUDIT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(AUDIT)
SOURCE = 'a' * 40


class ControlAuditTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name)

    def fixture(self, windows=False, different_binary=False, configurations=True):
        binary = b'fixture image; never execute'
        cases = [dict(name='case' + str(i), group='fixture', parameters={},
                      program_sha256=AUDIT.digest(b'1+1\n'), oracle_sha256=AUDIT.digest(b'2\n'))
                 for i in range(4)]
        images = {role: binary for role in ('base', 'aa')}
        if different_binary:
            images['aa'] = b'fixture IMAGE; never execute'
        tools = [dict(name=role, argv=['/fixture/' + role], complete_provenance=True,
                      binary_sha256=AUDIT.digest(images[role]),
                      metadata=dict(source_revision=SOURCE, instrumented=False,
                                    binary_sha256=AUDIT.digest(images[role])))
                 for role in ('base', 'aa')]
        if configurations:
            for tool in tools:
                tool['metadata']['configuration'] = dict(build_type='Release')
        harness = (SCRIPTS / 'bench-bc.py').read_bytes().replace(b'\r\n', b'\n')
        if windows:
            harness = harness.replace(b'\n', b'\r\n')
        manifest = dict(corpus=cases, implementations=tools, samples=3, warmups=1,
                        seed=197216, reference='base', bootstrap_resamples=200,
                        platform=dict(system='Windows' if windows else 'Linux'),
                        harness_sha256=AUDIT.digest(harness))
        rows, rng = [], random.Random(manifest['seed'])
        for phase, rounds in [('warmup', 1), ('measurement', 3)]:
            for round_number in range(rounds):
                shuffled = list(cases)
                rng.shuffle(shuffled)
                for case in shuffled:
                    block = list(tools)
                    rng.shuffle(block)
                    for order, tool in enumerate(block):
                        seconds = [.001, .02, .1, .3][int(case['name'][-1])]
                        if tool['name'] == 'aa':
                            seconds *= 1.08
                        rows.append(dict(phase=phase, round=round_number, order=order,
                                         workload=case['name'], implementation=tool['name'],
                                         seconds=seconds, checked=True, output_bytes=2,
                                         output_sha256=AUDIT.digest(b'2\n')))
        summary = AUDIT.BENCH.summarize(cases, rows, 'base', manifest['seed'], 200)
        files = {}
        for case in cases:
            files['evidence/full-aa/workloads/' + case['name'] + '.bc'] = b'1+1\n'
            files['evidence/full-aa/workloads/' + case['name'] + '.expected'] = b'2\n'
        observations = []
        for tool in tools:
            role = tool['name']
            relative = role + ('\\binary.exe' if windows else '/binary')
            observations.append(dict(name=role, status='preserved', implementation=copy.deepcopy(tool),
                                     binary=relative, binary_sha256=AUDIT.digest(images[role]),
                                     file_bytes=len(images[role]), pe_code=AUDIT.CODEGEN.pe_code(images[role]),
                                     input_artifacts=[]))
            files['evidence/codegen/' + role + ('/binary.exe' if windows else '/binary')] = images[role]
        codegen = dict(status='preserved', observations=observations)
        return manifest, rows, summary, codegen, files

    def archive(self, fixture):
        manifest, rows, summary, codegen, files = fixture
        target = self.root / 'fixture.zip'
        with zipfile.ZipFile(target, 'w') as archive:
            for name, raw in files.items():
                archive.writestr(name, raw)
            for name, value in [('manifest', manifest), ('summary', summary)]:
                archive.writestr('evidence/full-aa/' + name + '.json', json.dumps(value))
            archive.writestr('evidence/full-aa/samples.jsonl',
                             '\n'.join(json.dumps(row) for row in rows))
            archive.writestr('evidence/codegen/manifest.json', json.dumps(codegen))
        return target, AUDIT.digest(target.read_bytes())

    def run_audit(self, fixture, **kwargs):
        path, digest = self.archive(fixture)
        options = dict(path=path, archive_sha256=digest, prefix='evidence/',
                       stages=['full-aa'], control='aa', source=SOURCE)
        options.update(kwargs)
        return AUDIT.audit(**options)

    def test_identical_bytes_do_not_erase_observed_control_bias(self):
        report = self.run_audit(self.fixture())
        stage = report['stages'][0]
        self.assertTrue(stage['binary_bytes_identical'])
        self.assertTrue(stage['declared_configurations_equal'])
        self.assertEqual(stage['checked_rows'], 32)
        self.assertEqual({r['interval_relation'] for r in stage['controls']}, {'above band'})
        self.assertEqual(set(stage['duration_bands']), {'<10ms', '10-50ms', '50-250ms', '>=250ms'})
        self.assertFalse(report['performance_acceptance'])
        self.assertFalse(report['merge_eligible'])

    def test_windows_paths_and_checkout_line_endings_are_replayed(self):
        stage = self.run_audit(self.fixture(windows=True))['stages'][0]
        self.assertEqual(stage['platform']['system'], 'Windows')
        self.assertTrue(stage['binary_bytes_identical'])

    def test_flat_provider_archives_accept_explicit_root_prefix(self):
        original, _ = self.archive(self.fixture())
        flat = self.root / 'flat.zip'
        with zipfile.ZipFile(original) as source, zipfile.ZipFile(flat, 'w') as target:
            for name in source.namelist():
                self.assertTrue(name.startswith('evidence/'))
                target.writestr(name[len('evidence/'):], source.read(name))
        for prefix in ('', '.'):
            with self.subTest(prefix=prefix):
                report = AUDIT.audit(flat, AUDIT.digest(flat.read_bytes()), prefix,
                                     ['full-aa'], 'aa', SOURCE)
                self.assertEqual(report['stages'][0]['checked_rows'], 32)
                self.assertFalse(report['merge_eligible'])

    def test_same_source_and_length_do_not_hide_different_binary_bytes(self):
        stage = self.run_audit(self.fixture(different_binary=True))['stages'][0]
        self.assertEqual(stage['identities']['base']['file_bytes'], stage['identities']['aa']['file_bytes'])
        self.assertFalse(stage['binary_bytes_identical'])

    def test_missing_configuration_is_unknown_not_equal(self):
        self.assertIsNone(self.run_audit(self.fixture(configurations=False))['stages'][0][
            'declared_configurations_equal'])

    def test_configuration_difference_remains_visible_without_a_waiver(self):
        fixture = self.fixture()
        fixture[0]['implementations'][1]['metadata']['configuration']['build_type'] = 'Debug'
        fixture[3]['observations'][1]['implementation']['metadata']['configuration']['build_type'] = 'Debug'
        report = self.run_audit(fixture)
        self.assertFalse(report['stages'][0]['declared_configurations_equal'])
        self.assertFalse(report['performance_acceptance'])

    def test_external_input_bytes_are_verified(self):
        for corrupted in (False, True):
            fixture = self.fixture()
            entry = dict(path='/fixture/library.bc', sha256=AUDIT.digest(b'1+1\n'))
            fixture[0]['implementations'][0]['input_artifacts'] = [entry]
            observation = fixture[3]['observations'][0]
            observation['implementation']['input_artifacts'] = [entry]
            observation['input_artifacts'] = [dict(entry, archive_path='base/input-0')]
            fixture[4]['evidence/codegen/base/input-0'] = b'changed' if corrupted else b'1+1\n'
            with self.subTest(corrupted=corrupted):
                if corrupted:
                    with self.assertRaises(ValueError): self.run_audit(fixture)
                else:
                    self.assertFalse(self.run_audit(fixture)['merge_eligible'])

    def test_requested_confirmation_remains_a_separate_stage(self):
        path, digest = self.archive(self.fixture())
        with zipfile.ZipFile(path, 'a') as archive:
            originals = [(name, archive.read(name)) for name in archive.namelist()
                         if name.startswith('evidence/full-aa/')]
            for name, raw in originals:
                archive.writestr(name.replace('/full-aa/', '/full-aa-confirmation/'), raw)
        report = AUDIT.audit(path, AUDIT.digest(path.read_bytes()), 'evidence/',
                             ['full-aa', 'full-aa-confirmation'], 'aa', SOURCE)
        self.assertEqual([s['stage'] for s in report['stages']], ['full-aa', 'full-aa-confirmation'])
        self.assertEqual([s['checked_rows'] for s in report['stages']], [32, 32])
        self.assertFalse(report['merge_eligible'])

    def test_only_autocorrelation_diagnostic_accepts_tiny_rounding(self):
        fixture = self.fixture()
        ratio = fixture[2]['workloads'][0]['candidate_over_reference']['aa']
        ratio['autocorrelation_lag1'] = 1e-16
        self.run_audit(fixture)
        ratio['ci95'][1] += 1e-12
        with self.assertRaises(ValueError): self.run_audit(fixture)

    def test_archive_source_harness_and_role_bindings_are_required(self):
        for mutation in ('digest', 'source', 'harness', 'metadata', 'binary', 'instrumented',
                         'provenance', 'extra-role', 'role-hash'):
            fixture = self.fixture()
            manifest, _, _, codegen, files = fixture
            options = {}
            if mutation == 'digest': options['archive_sha256'] = '0' * 64
            elif mutation == 'source': options['source'] = 'b' * 40
            elif mutation == 'harness': manifest['harness_sha256'] = '0' * 64
            elif mutation == 'metadata': codegen['observations'][0]['implementation']['argv'] = ['changed']
            elif mutation == 'binary': files['evidence/codegen/base/binary'] = b'changed'
            elif mutation == 'instrumented': manifest['implementations'][0]['metadata']['instrumented'] = True
            elif mutation == 'provenance': manifest['implementations'][0]['complete_provenance'] = False
            elif mutation == 'extra-role': manifest['implementations'].append(manifest['implementations'][0])
            elif mutation == 'role-hash': manifest['implementations'][0]['binary_sha256'] = '0' * 64
            with self.subTest(mutation=mutation), self.assertRaises(ValueError):
                self.run_audit(fixture, **options)

    def test_missing_reordered_unchecked_and_invalid_samples_are_rejected(self):
        for mutation in ('missing', 'extra', 'order', 'unchecked', 'nan', 'boolean',
                         'zero', 'round', 'output', 'summary', 'workload', 'count'):
            fixture = self.fixture()
            manifest, rows, summary, _, files = fixture
            if mutation == 'missing': rows.pop()
            elif mutation == 'extra': rows.append(rows[-1])
            elif mutation == 'order': rows[0], rows[1] = rows[1], rows[0]
            elif mutation == 'unchecked': rows[0]['checked'] = False
            elif mutation == 'nan': rows[0]['seconds'] = float('nan')
            elif mutation == 'boolean': rows[0]['seconds'] = True
            elif mutation == 'zero': rows[0]['seconds'] = 0
            elif mutation == 'round': rows[0]['round'] = False
            elif mutation == 'output': rows[0]['output_sha256'] = '0' * 64
            elif mutation == 'summary': summary['workloads'][0]['candidate_over_reference']['aa']['ci95'][1] = 2.0
            elif mutation == 'workload': files['evidence/full-aa/workloads/case0.bc'] = b'changed'
            elif mutation == 'count': manifest['samples'] = True
            with self.subTest(mutation=mutation), self.assertRaises(ValueError):
                self.run_audit(fixture)

    def test_path_traversal_and_duplicate_selection_are_rejected(self):
        for options in [dict(prefix='../evidence'), dict(prefix='C:\\evidence'),
                        dict(prefix='/'), dict(prefix='//'),
                        dict(stages=['full-aa', 'full-aa']),
                        dict(stages=['../full-aa']), dict(control='base')]:
            with self.subTest(options=options), self.assertRaises(ValueError):
                self.run_audit(self.fixture(), **options)

    def test_cli_never_overwrites_existing_evidence(self):
        path, digest = self.archive(self.fixture())
        output = self.root / 'report.json'
        argv = ['--archive', str(path), '--sha256', digest, '--stage', 'full-aa',
                '--control-role', 'aa', '--source', SOURCE, '--output', str(output)]
        self.assertEqual(AUDIT.main(argv), 0)
        original = output.read_bytes()
        with redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as caught:
            AUDIT.main(argv)
        self.assertEqual(caught.exception.code, 2)
        self.assertEqual(output.read_bytes(), original)


if __name__ == '__main__':
    unittest.main()
