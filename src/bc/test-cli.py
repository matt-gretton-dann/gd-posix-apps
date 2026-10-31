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
            self.assertEqual(child.returncode, 1)

    def test_missing_file_stops_inputs(self):
        with tempfile.TemporaryDirectory() as directory:
            later = Path(directory) / "later.bc"
            later.write_text("42\n")
            self.run_bc("17\n", files=(Path(directory) / "missing.bc", later), success=False)
            self.run_bc("17\n", "42\n", files=(later, Path(directory) / "missing.bc"),
                        success=False)

    def test_directory_input_error(self):
        # A directory is a controlled failing input, independent of permissions.
        # POSIX commonly opens it successfully then fails on the first read;
        # Windows commonly rejects the open. Both must report failure.
        with tempfile.TemporaryDirectory() as directory:
            self.run_bc("", files=(directory,), success=False)

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

    def test_parenthesized_assignments(self):
        for text, expected in (
            ("a=4\na\n", "4\n"),
            ("(a=4)\na\n", "4\n4\n"),
            ("((a=4))\na\n", "4\n4\n"),
            ("a=2\n(a+=3)\na\n", "5\n5\n"),
            ("(a[0]=3)\na[0]\n", "3\n3\n"),
            ("(a=4)+1\na\n", "5\n4\n"),
            ("a=0\n(a+=1)\na\n", "1\n1\n"),
        ):
            with self.subTest(text=text):
                self.run_bc(text, expected)

    def test_malformed_delimiters(self):
        for text in ("sqrt 1\n", "length 1\n", "abs 1\n", "define f() 1\n"):
            with self.subTest(text=text):
                self.run_bc(text, success=False)

    def test_invalid_numeric_continuations(self):
        for text in ("1\\+2\n", "1\\x\n", "1\\", ".5\\+2\n"):
            with self.subTest(text=text):
                self.run_bc(text, success=False)
        self.run_bc("1\\\n+2\n", "3\n")

    def test_eof(self):
        self.run_bc("1+1\n", "2\n")

    def test_eof_advances_inputs(self):
        with tempfile.TemporaryDirectory() as directory:
            first = Path(directory) / "first.bc"
            first.write_text("1+1\n")
            self.run_bc("3+4\n", "2\n7\n", (first,))

    def test_syntax_errors_are_read_time(self):
        for text in (
            "define f() {\nauto\n}\n",
            "define f() {\nauto\n}\nf()\n",
            "define f() {\nreturn (1\n}\n",
            "define f() {\nreturn (1\n}\nf()\n",
            "if(0) {\n1+\n}\n",
            "while(0) {\n1+\n}\n",
            "define f() {\nif(0) {\n1+\n}\n}\n",
        ):
            with self.subTest(text=text):
                self.run_bc(text, success=False)
        self.run_bc("2+3\ndefine f() {\nreturn (1\n}\n", "5\n", success=False)

    def test_malformed_item_is_discarded(self):
        self.run_bc("1 2\n", success=False)
        self.run_bc("1", success=False)
        # A real error before quit must not be erased by reading ahead to quit.
        self.run_bc("define f() {\nauto\n}\nquit\n", success=False)

    def test_square_root_termination(self):
        # Each limb width has a different working-scale guard. One of these
        # hits an exact integer Newton two-cycle on each supported native trait.
        for power in (6, 10, 20):
            root = "1" + "0" * (power // 2)
            for offset in (0, 1, 2, 3):
                value = str(10 ** power + offset)
                with self.subTest(value=value):
                    self.run_bc("sqrt(" + value + ")\n", root + "\n")
            value = "1" + "0" * power + ".02"
            with self.subTest(value=value):
                self.run_bc("sqrt(" + value + ")\n", root + ".00\n")

    def test_halt(self):
        if EXTENSIONS:
            self.run_bc("1+1\nhalt\n3+4\n", "2\n", keep_open=True)
            self.run_bc("if(0) halt\n3+4\n", "7\n")
        else:
            self.run_bc("halt\n", success=False)

    def test_shebang(self):
        self.run_bc("#!ignored\n1+1\n", "2\n" if EXTENSIONS else "", success=EXTENSIONS)
        self.run_bc("#!ignored", success=EXTENSIONS)
        for text, expected in ((" #!ignored\n", ""), ("\n#!ignored\n", ""),
                               ("1\n#!ignored\n2\n", "1\n"), ("#ordinary\n", "")):
            with self.subTest(text=text):
                self.run_bc(text, expected, success=False)

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
