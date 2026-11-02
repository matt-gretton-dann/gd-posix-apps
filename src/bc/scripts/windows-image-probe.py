#!/usr/bin/env python3
"""Read the main-module address in an isolated, correctness-checked bc launch.

This is a separate diagnostic, not a timed sample or a causal ASLR claim.
Unsupported hosts/APIs retain explicit unavailable results. SPDX-License-Identifier: Apache-2.0
"""
import argparse
import ctypes
import hashlib
import json
import math
import ntpath
import os
from pathlib import Path
import subprocess
import sys
import time

PROGRAM = b'1+1\n'


class ProbeError(RuntimeError):
    pass


class ModuleUnavailable(RuntimeError):
    def __init__(self, reason, code=None, retryable=False):
        super().__init__(reason)
        self.code = code
        self.retryable = retryable


def canonical_windows_path(value):
    value = str(value).replace('/', '\\')
    if value.startswith('\\\\?\\UNC\\'):
        value = '\\\\' + value[8:]
    elif value.startswith('\\\\?\\'):
        value = value[4:]
    return ntpath.normcase(ntpath.normpath(value))


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def last_error():
    return getattr(ctypes, 'get_last_error', lambda: 0)()


class WindowsModules:
    """Use documented PSAPI operations without assuming the first module is bc."""
    capacity = 1024
    path_capacity = 32768

    def __init__(self):
        pointer = ctypes.c_void_p
        dword = ctypes.c_uint32

        class ModuleInfo(ctypes.Structure):
            _fields_ = [('base', pointer), ('image_bytes', dword), ('entry', pointer)]

        self.info_type = ModuleInfo
        try:
            library = ctypes.WinDLL('kernel32', use_last_error=True)
            self.enumerate = library.K32EnumProcessModulesEx
            self.filename = library.K32GetModuleFileNameExW
            self.information = library.K32GetModuleInformation
        except (AttributeError, OSError):
            try:
                library = ctypes.WinDLL('psapi', use_last_error=True)
                self.enumerate = library.EnumProcessModulesEx
                self.filename = library.GetModuleFileNameExW
                self.information = library.GetModuleInformation
            except (AttributeError, OSError):
                raise ModuleUnavailable('native module API unavailable') from None
        self.library = library  # Keep the DLL/function lifetime explicit.
        self.enumerate.argtypes = [pointer, ctypes.POINTER(pointer), dword,
                                   ctypes.POINTER(dword), dword]
        self.enumerate.restype = ctypes.c_int
        self.filename.argtypes = [pointer, pointer, ctypes.c_wchar_p, dword]
        self.filename.restype = dword
        self.information.argtypes = [pointer, pointer, ctypes.POINTER(ModuleInfo), dword]
        self.information.restype = ctypes.c_int

    def snapshot(self, handle, executable):
        modules = (ctypes.c_void_p * self.capacity)()
        needed = ctypes.c_uint32()
        if not self.enumerate(handle, modules, ctypes.sizeof(modules), ctypes.byref(needed), 3):
            code = last_error()
            # PSAPI documents an initialization race. Access denial is a
            # capability result, rather than a reason to retry indefinitely.
            raise ModuleUnavailable('module enumeration failed', code, code in (0, 87, 299))
        unit = ctypes.sizeof(ctypes.c_void_p)
        if needed.value > ctypes.sizeof(modules) or needed.value % unit:
            raise ModuleUnavailable('module list exceeds bounds or has invalid extent')
        count = needed.value // unit
        truncated = False
        for index in range(count):
            if not modules[index]:
                continue
            name = ctypes.create_unicode_buffer(self.path_capacity)
            size = self.filename(handle, modules[index], name, len(name))
            if not size:
                code = last_error()
                if code == 5:
                    raise ModuleUnavailable('module path query denied', code)
                continue
            if size >= len(name) - 1:
                truncated = True
                continue
            if canonical_windows_path(name.value) != canonical_windows_path(executable):
                continue
            info = self.info_type()
            if not self.information(handle, modules[index], ctypes.byref(info), ctypes.sizeof(info)):
                code = last_error()
                raise ModuleUnavailable('main-module information unavailable', code,
                                        code in (0, 87, 299))
            if not info.base or not info.image_bytes:
                raise ModuleUnavailable('main-module information has zero address or size')
            if info.base != modules[index]:
                raise ModuleUnavailable('inconsistent main-module base', retryable=True)
            return dict(status='collected', main_module_path_matches=True,
                        module_base='0x{:016x}'.format(info.base), image_bytes=info.image_bytes,
                        modules_observed=count,
                        method='EnumProcessModulesEx + GetModuleFileNameExW + GetModuleInformation')
        if truncated:
            raise ModuleUnavailable('module path snapshot was truncated')
        raise ModuleUnavailable('main module not present in initialized snapshot', retryable=True)


def native_windows_64():
    return os.name == 'nt' and ctypes.sizeof(ctypes.c_void_p) == 8


def clean_environment():
    value = {k: v for k, v in os.environ.items()
             if not k.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
    value.update(LC_ALL='C', LANG='C')
    return value


def probe(executable, timeout=120, probe_timeout=2):
    for value in (timeout, probe_timeout):
        try:
            valid = not isinstance(value, bool) and math.isfinite(value) and value > 0
        except (TypeError, ValueError, OverflowError):
            valid = False
        if not valid:
            raise ValueError('probe deadlines must be finite and positive')
    if not native_windows_64():
        return dict(status='unavailable', reason='requires native 64-bit Windows Python',
                    checked=False, scope='separate diagnostic; not an elapsed sample')
    executable = Path(executable).resolve(strict=True)
    try:
        api = WindowsModules()
    except ModuleUnavailable as error:
        return dict(status='unavailable', reason=str(error), checked=False,
                    scope='separate diagnostic; not an elapsed sample')
    before = digest(executable)
    child_deadline = time.monotonic() + timeout
    child = None
    attempts = 0
    observation = dict(status='unavailable', reason='module probe did not complete')
    try:
        child = subprocess.Popen([str(executable)], stdin=subprocess.PIPE,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 env=clean_environment())
        deadline = min(child_deadline, time.monotonic() + probe_timeout)
        # Keep stdin open. Once initialized, bc waits for input while its module
        # list can be read; no busy bc program or timed-sample hook is needed.
        while True:
            attempts += 1
            handle = getattr(child, '_handle', None)
            if handle is None:
                observation = dict(status='unavailable', reason='process handle unavailable')
                break
            try:
                observation = api.snapshot(handle, str(executable))
                break
            except ModuleUnavailable as error:
                observation = dict(status='unavailable', reason=str(error), error_code=error.code)
                remaining = deadline - time.monotonic()
                if not error.retryable or remaining <= 0 or child.poll() is not None:
                    break
                time.sleep(min(0.01, remaining))
        remaining = child_deadline - time.monotonic()
        if remaining <= 0:
            raise subprocess.TimeoutExpired(str(executable), timeout)
        stdout, stderr = child.communicate(PROGRAM, timeout=remaining)
        if child.returncode != 0 or stderr or stdout.replace(b'\r\n', b'\n') != b'2\n':
            raise ProbeError('bc diagnostic output/exit check failed')
        if digest(executable) != before:
            raise ProbeError('bc executable changed during diagnostic')
        observation.update(checked=True, binary_sha256=before,
                           program_sha256=hashlib.sha256(PROGRAM).hexdigest(),
                           output_sha256=hashlib.sha256(stdout).hexdigest(), attempts=attempts,
                           scope='separate checked launch; not a timed-child address or a causal claim')
        return observation
    except subprocess.TimeoutExpired:
        raise ProbeError('bc diagnostic child exceeded its deadline') from None
    finally:
        if child is not None and child.poll() is None:
            child.kill()
            child.communicate()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    try:
        result = probe(args.bc)
        args.output.write_text(json.dumps(result, indent=2, sort_keys=True) + '\n', encoding='utf-8')
        return 0
    except (OSError, ValueError, ProbeError) as error:
        print('Image diagnostic failed: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
