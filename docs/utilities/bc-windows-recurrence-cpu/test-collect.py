#!/usr/bin/env python3
"""Test preflight and checked-child plumbing without launching bc."""
# SPDX-License-Identifier: Apache-2.0
from pathlib import Path
import tempfile
import unittest
from unittest.mock import MagicMock, patch

import collect


class Tests(unittest.TestCase):
    def test_bad_archive_is_rejected_before_extraction(self):
        with tempfile.TemporaryDirectory() as temporary:
            archive = Path(temporary) / 'wrong.zip'
            archive.write_bytes(b'not the frozen ZIP')
            with self.assertRaisesRegex(ValueError, 'archive digest'):
                collect.inputs(archive)

    def test_file_arguments_and_windows_line_endings(self):
        child = MagicMock(pid=12, returncode=0)
        child.communicate.return_value = (b'1\r\n', b'')
        with patch.object(collect.subprocess, 'Popen') as launch:
            launch.return_value.__enter__.return_value = child
            self.assertEqual(collect.checked_run('bc.exe', 'input.bc', b'1', b'1\n', False), 12)
            self.assertEqual(launch.call_args.args[0], ['bc.exe', 'input.bc'])
            child.communicate.assert_called_once_with(None, timeout=10)

    def test_stdin_passes_original_bytes_and_checks_output(self):
        child = MagicMock(pid=12, returncode=0)
        child.communicate.return_value = (b'2\n', b'')
        with patch.object(collect.subprocess, 'Popen') as launch:
            launch.return_value.__enter__.return_value = child
            with self.assertRaisesRegex(ValueError, 'frozen oracle'):
                collect.checked_run('bc.exe', 'input.bc', b'1', b'1\n', True)
            self.assertEqual(launch.call_args.args[0], ['bc.exe'])
            child.communicate.assert_called_once_with(b'1', timeout=10)


if __name__ == '__main__':
    unittest.main()
