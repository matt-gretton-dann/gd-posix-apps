#!/usr/bin/env python3
"""Bounded CLI regressions; every child is killed and reaped on timeout."""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class CliTests(unittest.TestCase):
    def run_bc(self, text, expected="", files=(), keep_open=False, success=True):
        with subprocess.Popen(
            [BC, *map(str, files)], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
            env=dict(os.environ, LC_ALL="C"),
        ) as child:
            try:
                if keep_open:
                    child.stdin.write(text)
                    child.stdin.flush()
                    child.wait(timeout=2)
                    out, err = child.communicate(timeout=2)
                else:
                    out, err = child.communicate(text, timeout=2)
            except subprocess.TimeoutExpired:
                child.kill()
                child.communicate()
                self.fail("bc did not terminate within two seconds")
        self.assertEqual(out, expected)
        if success:
            self.assertEqual(err, "")
            self.assertEqual(child.returncode, 0)
        else:
            self.assertNotEqual(err, "")
            self.assertNotEqual(child.returncode, 0)

    def test_quit(self):
        for text, expected in [
            ("quit\n", ""),
            ("1+1\nquit\n", "2\n"),
            ("quit\n@invalid\n", ""),
            ("define f() {\nquit\n}\nf()\n", ""),
            ("if(0) quit\n3+4\n", ""),
            # Read-time quit discards the current input item, as GNU bc does.
            ("1+1;quit;3+4\n", ""),
        ]:
            with self.subTest(text=text):
                self.run_bc(text, expected)

    def test_quit_with_open_stdin(self):
        self.run_bc("1+1\nquit\n", "2\n", keep_open=True)

    def test_quit_stops_all_inputs(self):
        with tempfile.TemporaryDirectory() as directory:
            first = Path(directory) / "first.bc"
            quit_file = Path(directory) / "quit.bc"
            first.write_text("1+1\n")
            quit_file.write_text("3+4\nquit\n@invalid\n")
            # The nonexistent third file must never be opened.
            self.run_bc("8+9\n", "2\n7\n",
                        (first, quit_file, Path(directory) / "missing.bc"))

    def test_eof(self):
        self.run_bc("1+1\n", "2\n")

    def test_eof_advances_inputs(self):
        with tempfile.TemporaryDirectory() as directory:
            first = Path(directory) / "first.bc"
            first.write_text("1+1\n")
            self.run_bc("3+4\n", "2\n7\n", (first,))

    def test_halt(self):
        if EXTENSIONS:
            self.run_bc("1+1\nhalt\n3+4\n", "2\n", keep_open=True)
            self.run_bc("if(0) halt\n3+4\n", "7\n")
        else:
            self.run_bc("halt\n", success=False)

    def test_comments(self):
        for text in ("/**/2\n", "/*ordinary*/2\n", "/*\nmultiline\n*/2\n",
                     "/****/2\n", "/* / */2\n", "/* *x/ still inside */2\n",
                     "/**//**/2\n"):
            with self.subTest(text=text):
                self.run_bc(text, "2\n")
        for text in ("/*", "/*ordinary", "/* *", "/* *x/", "/*\n**x/"):
            with self.subTest(text=text):
                self.run_bc(text, success=False)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    parser.add_argument("--extensions", choices=("ON", "OFF"), required=True)
    args, remaining = parser.parse_known_args()
    BC = str(Path(args.bc).resolve())
    EXTENSIONS = args.extensions == "ON"
    unittest.main(argv=[__file__, *remaining])
