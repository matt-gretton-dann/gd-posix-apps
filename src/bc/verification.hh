/** \file verification.hh
 * Structural and result-availability checks for VM instruction streams.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_VERIFICATION_HH_INCLUDED
#define GD_BC_VERIFICATION_HH_INCLUDED

#include "bc.hh"

namespace GD::Bc::Details {
#if !defined(_WIN32)
// Used only after structural validation, before the full graph verifier.
#  if __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#  endif
auto is_verified_linear_definition(Instructions const& code) -> bool;
#endif

struct VerificationError
{
  Instruction::Index instruction;
  std::string_view reason;
};

// Operand schemas, offset bounds and producer types must be checked first.
// Function bodies are separate execution regions, including when never called.
// Marker states must agree at joins and consume the incoming function pack once.
auto verify_control_flow(Instructions const& code, bool function_body)
  -> std::optional<VerificationError>;
}  // namespace GD::Bc::Details
#endif
