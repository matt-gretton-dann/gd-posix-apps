#!/usr/bin/env python3
"""Bounded POSIX PTY interrupt/recovery test with an execution readiness marker."""

import argparse
import errno
import os
import signal
import subprocess
import sys
import tempfile
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
    def test_second_interrupt_terminates_a_blocked_execution(self):
        with tempfile.TemporaryFile() as program:
            program.write(b'{"READY\n";"' + b'x' * (1024 * 1024) + b'"}\n')
            program.seek(0)
            with subprocess.Popen([BC], stdin=program, stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE) as child:
                try:
                    self.read_until(child.stdout.fileno(), b"READY\n")
                    # Leave the large write blocked in this execution item.
                    # The first SIGINT interrupts the VM, and the second
                    # must still have the default terminating disposition.
                    os.kill(child.pid, signal.SIGINT)
                    time.sleep(0.05)
                    self.assertIsNone(child.poll())
                    os.kill(child.pid, signal.SIGINT)
                    self.assertEqual(child.wait(timeout=3), -signal.SIGINT)
                    child.communicate(timeout=3)
                finally:
                    if child.poll() is None:
                        child.kill()
                        child.communicate(timeout=3)

    def test_idle_interrupt_preserves_inherited_disposition(self):
        for disposition in (signal.SIG_DFL, signal.SIG_IGN):
            with self.subTest(disposition=disposition), tempfile.TemporaryDirectory() as directory:
                path = os.path.join(directory, "input")
                os.mkfifo(path)
                previous = signal.signal(signal.SIGINT, disposition)
                try:
                    child = subprocess.Popen([BC, path], stdin=subprocess.DEVNULL,
                                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                finally:
                    signal.signal(signal.SIGINT, previous)
                writer = None
                try:
                    deadline = time.monotonic() + 3
                    while writer is None:
                        try:
                            writer = os.open(path, os.O_WRONLY | os.O_NONBLOCK)
                        except OSError as error:
                            if error.errno != errno.ENXIO:
                                raise
                            self.assertLess(time.monotonic(), deadline, "bc did not open its input")
                            time.sleep(0.005)
                    # Opening the writer proves the CLI has reached its input
                    # reader. With no bytes available, no item can execute.
                    os.kill(child.pid, signal.SIGINT)
                    if disposition == signal.SIG_IGN:
                        os.write(writer, b"6*7\nquit;")
                    out, err = child.communicate(timeout=3)
                    if disposition == signal.SIG_DFL:
                        self.assertEqual((child.returncode, out, err), (-signal.SIGINT, b"", b""))
                    else:
                        self.assertEqual((child.returncode, out, err), (0, b"42\n", b""))
                finally:
                    if writer is not None:
                        os.close(writer)
                    if child.poll() is None:
                        child.kill()
                        child.communicate(timeout=3)

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

    def test_scalar_function_interrupt_preserves_bindings_and_argument_reuse(self):
        master, slave = pty.openpty()
        try:
            tty.setraw(slave)
            with subprocess.Popen([BC], stdin=subprocess.PIPE, stdout=slave,
                                  stderr=subprocess.PIPE, universal_newlines=True,
                                  env=dict(os.environ, LC_ALL="C")) as child:
                os.close(slave)
                slave = None
                try:
                    # Warm eligible calls complete before readiness, including
                    # the recursive word tier's bounded eight-call warmup;
                    # repeated bounded scalar calls then run in one item with
                    # the VM handler installed. This tests recovery around
                    # calls without claiming a particular machine-code PC.
                    child.stdin.write('define f(y) {\nreturn(y+1)\n}\ny=73\n'
                                      '{for(t=0;t<8;t++)v=f(4);"READY\n";while(1) {t=f(t%1000)}}\n')
                    child.stdin.flush()
                    self.assertEqual(self.read_until(master, b"READY\n"), b"READY\n")
                    os.kill(child.pid, signal.SIGINT)
                    child.stdin.write("f(2)\ny\n")
                    child.stdin.flush()
                    self.assertEqual(self.read_until(master, b"73\n"), b"3\n73\n")
                    _, errors = child.communicate("quit\n", timeout=3)
                    self.assertEqual(child.returncode, 0, errors)
                    self.assertEqual(errors, "Execution interrupted.\n")
                    self.assertEqual(self.drain(master), b"")
                finally:
                    if child.poll() is None:
                        child.kill()
                        child.communicate(timeout=3)
        finally:
            os.close(master)
            if slave is not None:
                os.close(slave)

    def test_hot_loop_function_interrupt_restores_parameter_and_auto(self):
        master, slave = pty.openpty()
        try:
            tty.setraw(slave)
            with subprocess.Popen([BC], stdin=subprocess.PIPE, stdout=slave,
                                  stderr=subprocess.PIPE, universal_newlines=True,
                                  env=dict(os.environ, LC_ALL="C")) as child:
                os.close(slave)
                slave = None
                try:
                    # Warm a pure function containing a backedge before the
                    # marker. The interrupted call may be in the word tier or
                    # its generic fallback; no particular PC is asserted.
                    child.stdin.write('define f(x) {\nauto i\nif(x==0)return(0)\n'
                                      'i=x\nwhile(i>0)i+=1\nreturn(i)\n}\n'
                                      'x=73\ni=29\n{for(t=0;t<8;t++)v=f(0);'
                                      '"READY\n";q=f(1)}\n')
                    child.stdin.flush()
                    self.assertEqual(self.read_until(master, b"READY\n"), b"READY\n")
                    os.kill(child.pid, signal.SIGINT)
                    child.stdin.write("f(0)\nx\ni\n")
                    child.stdin.flush()
                    self.assertEqual(self.read_until(master, b"29\n"), b"0\n73\n29\n")
                    _, errors = child.communicate("quit\n", timeout=3)
                    self.assertEqual(child.returncode, 0, errors)
                    self.assertEqual(errors, "Execution interrupted.\n")
                    self.assertEqual(self.drain(master), b"")
                finally:
                    if child.poll() is None:
                        child.kill()
                        child.communicate(timeout=3)
        finally:
            os.close(master)
            if slave is not None:
                os.close(slave)

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
                    for marker, setup in (("READY1", ""), ("READY2", "a=0;" * 64)):
                        # The marker and infinite loop belong to one execution
                        # item: observing it proves the VM handler is installed.
                        # The second item has enough producers for compact
                        # lowering. Assignments keep the readiness output exact.
                        program = '{' + setup + '"' + marker + '\n";while(1) {}}\n'
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
