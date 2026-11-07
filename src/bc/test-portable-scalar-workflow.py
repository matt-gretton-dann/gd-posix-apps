#!/usr/bin/env python3
"""Check qualification role wiring and restart refusal without running bc."""
import contextlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest import mock


WORKFLOW = (Path(__file__).resolve().parents[2] / '.github/workflows/'
            'bc-portable-scalar-qualification.yml')


def python_steps():
    # The workflow keeps its small Python programs inline. Execute those actual
    # programs with fake process boundaries rather than duplicating their logic.
    lines = WORKFLOW.read_text(encoding='utf-8').splitlines()
    scripts = []
    for index, line in enumerate(lines):
        if line != '        shell: python':
            continue
        index += 2
        block = []
        while index < len(lines) and (lines[index].startswith('          ') or not lines[index]):
            block.append(lines[index][10:])
            index += 1
        scripts.append(compile('\n'.join(block), str(WORKFLOW), 'exec'))
    if len(scripts) != 2:
        raise AssertionError('expected collector and startup-binding Python programs')
    return scripts


@contextlib.contextmanager
def directory(path):
    previous = Path.cwd()
    try:
        os.chdir(str(path))
        yield
    finally:
        os.chdir(str(previous))


class Qualification(unittest.TestCase):
    def environment(self, suite='arithmetic', enabled=False, attempt='1'):
        return dict(GITHUB_RUN_ATTEMPT=attempt, GITHUB_WORKSPACE='/fixture',
                    BC_SUITE=suite, BC_BASE='a' * 40, BC_CANDIDATE='b' * 40,
                    BC_PORTABLE=str(enabled).lower())

    def test_explicit_roles_reach_the_actual_collector_command(self):
        for suite in ('arithmetic', 'math-full', 'math-holdout'):
            for enabled in (False, True):
                with self.subTest(suite=suite, enabled=enabled), \
                        mock.patch.dict(os.environ, self.environment(suite, enabled), clear=True), \
                        mock.patch.object(subprocess, 'run') as run:
                    exec(python_steps()[0], {})
                    run.assert_called_once()
                    self.assertIs(run.call_args[1]['check'], True)
                    command = run.call_args[0][0]
                    self.assertEqual(command[command.index('--base') + 1], 'a' * 40)
                    self.assertEqual(command[command.index('--candidate') + 1], 'b' * 40)
                    self.assertEqual(command[command.index('--candidate-portable-scalar') + 1],
                                     'ON' if enabled else 'OFF')
                    self.assertEqual('--base-portable-scalar' in command, enabled)
                    if enabled:
                        self.assertEqual(command[command.index('--base-portable-scalar') + 1], 'OFF')
                    self.assertIn('--compare-resources', command)
                    self.assertIn('--broad', command)
                    if suite == 'arithmetic':
                        self.assertIn('--sparse-qualification', command)
                    else:
                        self.assertEqual(command[command.index('--broad-preset') + 1], suite[5:])
                        self.assertEqual(command[command.index('--duration-seconds') + 1], '.25')
                        self.assertEqual(command[command.index('--child-timeout-seconds') + 1], '600')
                        self.assertEqual(command[command.index('--total-seconds') + 1], '18000')

    def test_restart_is_rejected_before_any_build_or_sampling(self):
        for attempt in ('0', '2', 'invalid'):
            with self.subTest(attempt=attempt), \
                    mock.patch.dict(os.environ, self.environment(attempt=attempt), clear=True), \
                    mock.patch.object(subprocess, 'run') as run:
                with self.assertRaises(SystemExit):
                    exec(python_steps()[0], {})
                run.assert_not_called()

    def test_collector_failure_propagates_without_replacement(self):
        with mock.patch.dict(os.environ, self.environment(), clear=True), \
                mock.patch.object(subprocess, 'run', side_effect=subprocess.CalledProcessError(
                    1, ['fixture'])) as run:
            with self.assertRaises(subprocess.CalledProcessError):
                exec(python_steps()[0], {})
            run.assert_called_once()

    def test_startup_uses_the_three_original_ordinary_roles(self):
        with tempfile.TemporaryDirectory() as temporary, directory(temporary):
            Path('cohort').mkdir()
            entries = []
            for role in ('base', 'aa', 'candidate'):
                path = Path('portable-results', role)
                path.mkdir(parents=True)
                entry = dict(name=role, argv=['original-' + role], metadata=dict(
                    source_revision='a' * 40 if role != 'candidate' else 'b' * 40,
                    binary_sha256=role * 8))
                entries.append(entry)
                (path / 'implementations.json').write_text(json.dumps(dict(implementations=[entry])))
            with mock.patch.object(subprocess, 'run') as run:
                exec(python_steps()[1], {})
                self.assertEqual(json.loads(Path('cohort/startup-config.json').read_text()),
                                 dict(implementations=entries))
                run.assert_not_called()
            Path('portable-results/aa/implementations.json').unlink()
            with mock.patch.object(subprocess, 'run') as run:
                with self.assertRaises(FileNotFoundError):
                    exec(python_steps()[1], {})
                run.assert_not_called()


if __name__ == '__main__':
    unittest.main()
