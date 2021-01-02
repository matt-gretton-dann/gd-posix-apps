"""Support library for integration tests"""
# Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import argparse
import filecmp
import subprocess
import sys
import os
import os.path


def _try_remove(file):
    """Try to remove a file.  Don't complain if it is not there..."""
    try:
        os.remove(file)
    except OSError:
        pass


def _use_text(stdout, stderr, stdin):
    """Are the inputs all str (and so need to use text in subprocess.run)?

        Returns True if subprocess.run(text) should be set to True, otherwise
        False.

        Will raise a TypeError if the inputs are a mix of str and bytes.
    """
    seen_str = False
    seen_bytes = False
    for s in [stdout, stderr, stdin]:
        if s is not None:
            seen_str = seen_str | isinstance(s, str)
            seen_bytes = seen_bytes | isinstance(s, bytes)
    if seen_str and seen_bytes:
        raise TypeError(
            "Mixed use of str & bytes objects for stdout, stderr, and stdin.")

    return seen_str


class TestRunner:
    """Integration Test runner."""

    def __init__(self):
        """Parse arguments"""
        parser = argparse.ArgumentParser()
        parser.add_argument("exe", help="Executable to test")
        parser.add_argument(
            "--input-dir", help="Directory which conatins input files.  If not specified will fail if we try to use input files.")
        parser.add_argument(
            "--output-dir", help="Directory which contains output files.  If not specified will fail if we try to write output.")
        parser.add_argument("--keep", action='store_true',
                            help="Keep all output files even if tests pass.")
        self._args = parser.parse_args()
        self._passes = 0
        self._fails = 0
        self._skips = 0
        self._files = dict()  # Map files to True/False to remove.

    def register_file(self, file):
        """Register a file to be cleaned up at the end of the test process.

        The first time a file is registered we attempt to remove it so that the
        tests run in a clean environment.

        If the file has been deregistered we do not re-register it."""
        if file not in self._files:
            _try_remove(file)
            self._files[file] = True

    def deregister_file(self, file):
        """Deregister a file to be cleaned up.

        It is not an error if FILE has not been previously registered - we
        just register it and mark it to not be removed."""
        if file not in self._files:
            self.register_file(file)

        self._files[file] = False

    def run_test(self, cmdline, expected_rc=0, expected_stdout=None,
                 expected_stderr=None, test_name=None, stdin=None, files=None):
        """Run a test

        Runs the executable given in the list CMDLINE and checks for the expected
        return code EXPECTED_RC.  If EXPECTED_STDOUT or EXPECTED_STDERR are not
        None then they are also checked.

        Specify STDIN if there is a standard input to use.

        FILES should contain a list of all files that the test _outputs_ to.
        These files will be removed at the end of the test run if all tests
        which use them have been successful.

        EXPECTED_STDOUT, EXPECTED_STDERR, and STDIN may be specified as `str`s or
        `byte`s.  But it must be consistent within a call you cannoy specify STDIN
        as a byte and EXPECTED_STDOUT as a str for instace.

        Outputs a PASS/FAIL line to stdout.  The test name is given as TEST_NAME or
        the basename of cmdline[0]

        returns 1 on success, 0 on failure.
        """
        capture_output = expected_stdout is not None or expected_stderr is not None
        capture_output = subprocess.PIPE if capture_output else None
        text = _use_text(expected_stdout, expected_stderr, stdin)

        if test_name is None:
            test_name = os.path.basename(cmdline[0])
        else:
            test_name = "{0} - {1}".format(
                os.path.basename(cmdline[0]), test_name)

        if files is not None:
            if not isinstance(files, list):
                files = [files]
            for file in files:
                self.register_file(file)

        success = True
        rc = subprocess.run(
            cmdline, stdout=capture_output, stderr=capture_output,
            universal_newlines=text, input=stdin)

        if rc.returncode != expected_rc:
            print(
                f"FAIL: {test_name} (incorrect exit code: expected {expected_rc} got {rc.returncode})")
            self._fails += 1
            success = False
        elif expected_stdout is not None and rc.stdout != expected_stdout:
            print(
                f"FAIL: {test_name} (stdout)\n---- EXPECTED: ----\n{expected_stdout}\n---- ACTUAL: ----\n{rc.stdout}\n")
            self._fails += 1
            success = False
        elif expected_stderr is not None and rc.stderr != expected_stderr:
            print(
                f"FAIL: {test_name} (stderr)\n---- EXPECTED: ----\n{expected_stderr}\n---- ACTUAL: ----\n{rc.stderr}\n")
            self._fails += 1
            success = False
        else:
            print(f"PASS: {test_name}")
            self._passes += 1

        if not success:
            print(f"# Command line: " + ' '.join(cmdline))
            if files is not None:
                for file in files:
                    self.deregister_file(file)

        return success

    def exe(self):
        """Get the exectuable path we are testing."""
        return self._args.exe

    def output_file(self, filename):
        """Get the path to use for an output file.

        If --output-dir was not specified on the command line this raises a
        RuntimeError.
        """
        if self._args.output_dir is None:
            raise RuntimeError(
                "Need --output-dir specified on the command line.")
        return os.path.join(self._args.output_dir, filename)

    def input_file(self, filename):
        """Get the path to use for an input file.

        Filename is the basename we want to use.

        If --input-dir was not specified on the command line this raises a
        RuntimeError.
        """
        if self._args.input_dir is None:
            raise RuntimeError(
                "Need --input-dir specified on the command line.")
        return os.path.join(self._args.input_dir, filename)

    def _strip_inout_dirs(self, file):
        """Strip the input/output directories from the start of a filename.

        This is used to get consistent output in test names/messages."""
        def rp(s, p):
            """str.removeprefix replacement if we don't have Python 3.9"""
            if s.startswith(p):
                return s[len(p):]
            else:
                return s

        if self._args.output_dir is not None:
            file = rp(file, self._args.output_dir + os.path.sep)
        if self._args.input_dir is not None:
            file = rp(file, self._args.input_dir + os.path.sep)
        return file

    def compare_files(self, file1, file2, skip=False):
        """Compare two files.

        If SKIP is True this is recorded as a skip, and the comparison is not
        done.  Otherwise the absence of the files is recorded as a fail."""
        if skip:
            self._skips += 1
            result = "SKIP"
            identical = True
        else:
            identical = filecmp.cmp(file1, file2, shallow=False)
            if not identical:
                # We failed so ensure we don't delete files.
                self.deregister_file(file1)
                self.deregister_file(file2)
                result = "FAIL"
                self._fails += 1
            else:
                result = "PASS"
                self._passes += 1

        file1out = self._strip_inout_dirs(file1)
        file2out = self._strip_inout_dirs(file2)
        print(f"{result}: File comparison {file1out} <-> {file2out}")
        return identical

    def summarize(self):
        print(
            f"======== SUMMARY ========\nPASS: {self._passes}\nFAIL: {self._fails}\nSKIP: {self._skips}")
        if self._fails > 0:
            sys.exit(1)
        else:
            # Remove all files if we're not keeping them
            if not self._args.keep:
                for f in self._files:
                    _try_remove(f)

            sys.exit(0)
