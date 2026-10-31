#!/usr/bin/env python3
"""Bounded POSIX PTY interrupt/recovery test with an execution readiness marker."""

import argparse
import errno
import os
import signal
import subprocess
import sys
import time
import unittest

if os.name != "posix":
    print("POSIX PTY signal-delivery facility is unavailable")
    sys.exit(77)
try:
    import pty
    import select
    import tty
except ImportError:
    print("POSIX PTY modules are unavailable")
    sys.exit(77)


class InterruptTest(unittest.TestCase):
    def read_until(self, descriptor, marker):
        result = b""
        deadline = time.monotonic() + 3
        while marker not in result:
            remaining = deadline - time.monotonic()
            self.assertGreater(remaining, 0, "execution readiness timed out")
            self.assertTrue(select.select([descriptor], [], [], remaining)[0],
                            "execution readiness timed out")
            try:
                chunk = os.read(descriptor, 4096)
            except OSError as error:
                if error.errno == errno.EIO:
                    self.fail("child closed the PTY before readiness")
                raise
            self.assertTrue(chunk, "child closed the PTY before readiness")
            result += chunk
        return result

    def drain(self, descriptor):
        result = b""
        deadline = time.monotonic() + 3
        while True:
            remaining = deadline - time.monotonic()
            self.assertGreater(remaining, 0, "PTY did not close after child exit")
            self.assertTrue(select.select([descriptor], [], [], remaining)[0])
            try:
                chunk = os.read(descriptor, 4096)
            except OSError as error:
                if error.errno == errno.EIO:
                    return result
                raise
            if not chunk:
                return result
            result += chunk

    def test_two_interrupts_then_recovery(self):
        master, slave = pty.openpty()
        try:
            tty.setraw(slave)
            with subprocess.Popen([BC], stdin=subprocess.PIPE, stdout=slave,
                                  stderr=subprocess.PIPE, universal_newlines=True,
                                  env=dict(os.environ, LC_ALL="C")) as child:
                os.close(slave)
                slave = None
                try:
                    for marker in ("READY1", "READY2"):
                        # The marker and infinite loop belong to one execution
                        # item: observing it proves the VM handler is installed.
                        program = '{"' + marker + '\n";while(1) {}}\n'
                        child.stdin.write(program)
                        child.stdin.flush()
                        self.assertEqual(self.read_until(master, (marker + "\n").encode()),
                                         (marker + "\n").encode())
                        os.kill(child.pid, signal.SIGINT)
                    child.stdin.write("2+3\n")
                    child.stdin.flush()
                    self.assertEqual(self.read_until(master, b"5\n"), b"5\n")
                    _, errors = child.communicate("quit\n", timeout=3)
                    self.assertEqual(child.returncode, 0, errors)
                    self.assertEqual(errors, "Execution interrupted.\n" * 2)
                    self.assertEqual(self.drain(master), b"")
                finally:
                    if child.poll() is None:
                        child.kill()
                        child.communicate(timeout=3)
        finally:
            os.close(master)
            if slave is not None:
                os.close(slave)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    args, remaining = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remaining])
