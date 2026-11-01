/** \file compiler-hints.hh
 * Optional code-placement hints with a portable semantic fallback.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_COMPILER_HINTS_HH
#define GD_BC_COMPILER_HINTS_HH

// Keep diagnostic formatting out of successful arithmetic/VM execution. These
// hints change neither the checked conditions nor the reported diagnostics.
// Compilers without supported hints retain the ordinary noreturn function.
#if defined(__GNUC__) || defined(__clang__)
#  define GD_BC_COLD_DIAGNOSTIC __attribute__((cold, noinline))
#elif defined(_MSC_VER)
#  define GD_BC_COLD_DIAGNOSTIC __declspec(noinline)
#else
#  define GD_BC_COLD_DIAGNOSTIC
#endif

#endif
