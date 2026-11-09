#!/usr/bin/env python3
"""Exercise frozen-input binding and fake diagnostic children; no bc execution."""
import argparse
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import stat
import tempfile
from types import SimpleNamespace
import unittest
from unittest import mock
import zipfile

HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location('frozen_arithmetic', str(HERE / 'collect.py'))
COLLECT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(COLLECT)


def sha(raw):
    return hashlib.sha256(raw).hexdigest()


class Frozen(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.archive = self.root / 'input.zip'
        self.output = self.root / 'output'
        self.image = b'\x7fELF\x02\x01' + b'\0' * 12 + b'\x3e\0' + b'controlled fixture'
        self.sources = dict(base='b' * 40, candidate='c' * 40)
        self.payloads = {}
        observations = []
        images = {}
        for role in ('base', 'candidate'):
            name = 'images/' + role
            self.payloads[name] = self.image
            image = dict(path=name, sha256=sha(self.image), bytes=len(self.image), source_revision=self.sources[role])
            images[role] = image
            metadata = dict(source_revision=self.sources[role], instrumented=False, binary_sha256=sha(self.image),
                compiler=dict(version='controlled fixture'), flags=['-O3'], version='test',
                configuration=dict(diagnostic_profile=False, prefix_result_policy=dict(
                    enabled=role == 'candidate', implementation_present=role == 'candidate')))
            observations.append(dict(name=role, implementation=dict(name=role, argv=['/original/' + role + '/bc'],
                metadata=metadata), binary_sha256=sha(self.image), file_bytes=len(self.image)))
        self.payloads['codegen.json'] = json.dumps(dict(status='preserved', observations=observations)).encode()
        program, expected = b'1\n', b'1\n'
        manifest = dict(suite='arithmetic', preset='full', corpus_seed=197200,
            harness_sha256=COLLECT.digest(SCRIPTS / 'bench-bc.py'), corpus=[dict(
            name='frozen', group='controlled', parameters=dict(iterations=3), input_mode='file',
            program_sha256=sha(program), oracle_sha256=sha(expected))])
        self.payloads['full/manifest.json'] = json.dumps(manifest).encode()
        self.payloads['full/summary.json'] = json.dumps(dict(workloads=[dict(name='frozen', implementations={
            role: dict(median_seconds=1) for role in self.sources})])).encode()
        self.payloads['full/workloads/frozen.bc'] = program
        self.payloads['full/workloads/frozen.expected'] = expected
        self.pin = dict(source=self.sources['candidate'], base=self.sources['base'], artifact_id=1,
            artifact_name='controlled-fixture', repository_id=2, run_id=3, codegen='codegen.json', images=images,
            groups=dict(full=dict(path='full/', workloads=['frozen'])),
            scripts={n:COLLECT.digest(SCRIPTS / n) for n in ['bench-bc.py', 'profile-bc.py']},
            files={n:sha(v) for n,v in self.payloads.items()}, scope='controlled test; no native execution')
        self.rewrite_archive()

    def rewrite_archive(self, extra=None):
        with zipfile.ZipFile(str(self.archive), 'w') as z:
            for name, data in list(self.payloads.items()) + (extra or []):
                info = zipfile.ZipInfo(name)
                info.external_attr = (stat.S_IFREG | 0o644) << 16
                z.writestr(info, data)
        self.pin['artifact_sha256'] = COLLECT.digest(self.archive)
        self.meta = dict(id=1, name='controlled-fixture', digest='sha256:' + self.pin['artifact_sha256'],
                         expired=False, workflow_run=dict(id=3, head_sha=self.sources['candidate'],
                                                         repository_id=2, head_repository_id=2))

    def prepare(self):
        return COLLECT.prepare(self.archive, self.meta, self.output, SCRIPTS, self.pin)

    def test_preparation_relocates_identical_bytes_without_executing_children(self):
        with mock.patch.object(COLLECT.subprocess, 'run', side_effect=AssertionError('no child during preparation')):
            report = self.prepare()
        self.assertFalse(report['performance_acceptance'])
        for role in self.sources:
            image = self.output / 'images' / role / 'bc'
            self.assertEqual(image.read_bytes(), self.image)
            config = json.loads(Path(report['configs'][role]).read_text())['implementations'][0]
            self.assertEqual(config['argv'], [str(image.resolve())])
            self.assertEqual(config['metadata']['source_revision'], self.sources[role])
        bench = COLLECT.load(SCRIPTS, 'bench-bc')
        cases, _ = bench.replay_corpus(self.output / 'replay/full', 'arithmetic', ['frozen'])
        self.assertEqual(cases[0]['input_mode'], 'file')
        self.assertEqual(cases[0]['parameters']['iterations'], 3)

    def test_provider_digest_and_source_are_both_required(self):
        for field, value in [('digest', 'sha256:' + '0' * 64), ('id', 99), ('expired', True)]:
            with self.subTest(field=field):
                bad = copy.deepcopy(self.meta);bad[field] = value
                with self.assertRaises(ValueError):COLLECT.prepare(self.archive, bad, self.output, SCRIPTS, self.pin)
        bad = copy.deepcopy(self.meta);bad['workflow_run']['head_sha'] = 'other source'
        with self.assertRaisesRegex(ValueError, 'another source'):COLLECT.prepare(self.archive, bad, self.output, SCRIPTS, self.pin)
        self.assertFalse(self.output.exists())

    def test_changed_zip_is_rejected_before_executable_creation(self):
        self.archive.write_bytes(self.archive.read_bytes() + b'changed')
        with self.assertRaisesRegex(ValueError, 'digest'):self.prepare()
        self.assertFalse(self.output.exists())

    def test_selected_oracle_is_independently_bound_inside_a_valid_zip(self):
        self.payloads['full/workloads/frozen.expected'] = b'2\n'
        self.rewrite_archive()
        with self.assertRaisesRegex(ValueError, 'frozen input changed'):self.prepare()
        self.assertFalse(self.output.exists())

    def test_changed_profiler_is_rejected(self):
        self.pin['scripts']['profile-bc.py'] = '0' * 64
        with self.assertRaisesRegex(ValueError, 'tool differs'):self.prepare()

    def test_unsafe_member_is_rejected_even_when_not_selected(self):
        for name in ('../escape', '/absolute', 'C:/drive', 'back\\slash'):
            with self.subTest(name=name):
                self.rewrite_archive([(name,b'unsafe')])
                with self.assertRaisesRegex(ValueError, 'unsafe'):self.prepare()
        self.assertFalse((self.root.parent / 'escape').exists())

    def test_duplicate_member_is_rejected(self):
        with self.assertWarns(UserWarning):self.rewrite_archive([('full/manifest.json', b'duplicate')])
        with self.assertRaisesRegex(ValueError, 'duplicate'):self.prepare()

    def test_instrumented_role_cannot_be_relocated_as_ordinary(self):
        cg = json.loads(self.payloads['codegen.json'].decode());cg['observations'][0]['implementation']['metadata']['instrumented'] = True
        self.payloads['codegen.json'] = json.dumps(cg).encode();self.pin['files']['codegen.json'] = sha(self.payloads['codegen.json']);self.rewrite_archive()
        with self.assertRaisesRegex(ValueError, 'ordinary image'):self.prepare()

    def test_existing_output_is_not_overwritten(self):
        self.output.mkdir();(self.output / 'user-file').write_text('preserve')
        with self.assertRaisesRegex(ValueError, 'fresh'):self.prepare()
        self.assertEqual((self.output / 'user-file').read_text(), 'preserve')

    def fake_runner(self, prepared, bad_output=False):
        calls = []
        library = self.root / 'libc.so';library.write_bytes(b'controlled library')
        def run(argv, **kwargs):
            calls.append(argv)
            if Path(argv[0]).name == 'ldd':
                return SimpleNamespace(returncode=0, stdout=('libc.so => ' + str(library) + ' (0x123)\n').encode())
            self.assertTrue(argv[1].endswith('/profile-bc.py'))
            self.assertNotIn('--calibrate-seconds', argv)
            self.assertIn('--replay-corpus', argv)
            self.assertGreater(kwargs['timeout'], 0)
            def arg(n):return argv[argv.index(n)+1]
            role, name = arg('--implementation'), arg('--workload');dest = Path(arg('--output'));dest.mkdir(parents=True)
            data = dict(implementation=dict(binary_sha256=prepared['images'][role]['sha256'],
                metadata=dict(source_revision=self.sources[role], instrumented=False)),
                repeat_estimate_summary_sha256=prepared['replay']['full']['summary_sha256'],
                replay=prepared['replay']['full']['evidence'],workloads=[dict(name=name, checked=not bad_output,
                    cpu=dict(checked=True, event='cpu-clock:u'))])
            COLLECT.write(dest / 'manifest.json', data)
            (dest / (name + '.perf.txt')).write_text('bc 1 cpu-clock:u: symbol (' + str(self.output / 'images' / role / 'bc') + ')\n')
            return SimpleNamespace(returncode=0)
        return run, calls

    def native(self):
        return mock.patch.multiple(COLLECT.platform, system=lambda:'Linux', machine=lambda:'x86_64')

    def test_diagnostics_use_exact_replay_and_keep_both_role_outcomes(self):
        prepared = self.prepare();run,calls = self.fake_runner(prepared)
        with self.native():report = COLLECT.collect(prepared,self.output,SCRIPTS,Path('/fake/perf'),runner=run)
        self.assertEqual(len(report['commands']),2)
        self.assertFalse(report['performance_acceptance'])
        self.assertEqual(set(report['runtime_libraries']),{'base','candidate'})
        self.assertEqual(len(calls),4)

    def test_output_failure_preserves_partial_collection_and_stops(self):
        prepared = self.prepare();run,calls = self.fake_runner(prepared,bad_output=True)
        with self.native(),self.assertRaisesRegex(ValueError,'selected output'):
            COLLECT.collect(prepared,self.output,SCRIPTS,Path('/fake/perf'),runner=run)
        saved = json.loads((self.output / 'collection.json').read_text())
        self.assertEqual(saved['status'],'incomplete');self.assertIn('failure',saved)
        self.assertEqual(len(saved['commands']),1)

    def test_shared_deadline_stops_before_any_diagnostic(self):
        prepared = self.prepare();run,calls = self.fake_runner(prepared)
        with self.native(),mock.patch.object(COLLECT.time,'monotonic',side_effect=[0,1801]):
            with self.assertRaisesRegex(ValueError,'shared diagnostic deadline'):
                COLLECT.collect(prepared,self.output,SCRIPTS,Path('/fake/perf'),runner=run)
        self.assertEqual(calls,[])

    def test_changed_image_stops_before_any_cpu_sampling(self):
        prepared = self.prepare();run,calls = self.fake_runner(prepared)
        (self.output / 'images/candidate/bc').write_bytes(b'changed')
        with self.native(),self.assertRaisesRegex(ValueError,'image changed'):
            COLLECT.collect(prepared,self.output,SCRIPTS,Path('/fake/perf'),runner=run)
        self.assertEqual(len(calls),1)
        self.assertEqual(Path(calls[0][0]).name,'ldd')

    def test_missing_runtime_library_is_not_reported_as_a_valid_profile(self):
        prepared = self.prepare()
        run = mock.Mock(return_value=SimpleNamespace(returncode=0,stdout=b'libc.so => not found\n'))
        with self.native(),self.assertRaisesRegex(ValueError,'unavailable runtime libraries'):
            COLLECT.collect(prepared,self.output,SCRIPTS,Path('/fake/perf'),runner=run)
        saved = json.loads((self.output / 'collection.json').read_text())
        self.assertEqual(saved['commands'],[])
        self.assertFalse(saved['performance_acceptance'])

    def test_native_collection_is_unavailable_on_other_hosts(self):
        prepared = self.prepare();run = mock.Mock()
        with mock.patch.object(COLLECT.platform,'system',return_value='Darwin'):
            with self.assertRaisesRegex(ValueError,'Linux x86-64'):
                COLLECT.collect(prepared,self.output,SCRIPTS,Path('/fake/perf'),runner=run)
        run.assert_not_called()

    def test_timed_out_attempt_retains_argv_and_partial_evidence(self):
        prepared = self.prepare();good,calls = self.fake_runner(prepared)
        def run(argv, **kwargs):
            if Path(argv[0]).name == 'ldd':return good(argv, **kwargs)
            raise COLLECT.subprocess.TimeoutExpired(argv,kwargs['timeout'])
        with self.native(),self.assertRaises(COLLECT.subprocess.TimeoutExpired):
            COLLECT.collect(prepared,self.output,SCRIPTS,Path('/fake/perf'),runner=run)
        saved=json.loads((self.output / 'collection.json').read_text())
        self.assertEqual(len(saved['commands']),1)
        self.assertIsNone(saved['commands'][0]['returncode'])
        self.assertIn('--replay-corpus',saved['commands'][0]['argv'])
        self.assertIn('failure',saved)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--scripts',type=Path,required=True)
    args, rest = parser.parse_known_args();SCRIPTS = args.scripts.resolve()
    unittest.main(argv=[__file__]+rest)
