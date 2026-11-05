/** Ordinary Number arithmetic boundary for the optional integer engine.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "integer-function.hh"

namespace GD::Bc::Details {
auto integer_number_binary(Instruction::Opcode opcode, Number left, Number const& right)
  -> std::optional<Number>
{
  using O = Instruction::Opcode;
  switch (opcode) {
  case O::add:
    left.add(right);
    break;
  case O::subtract:
    left.sub(right);
    break;
  case O::multiply:
    left.multiply(right, 0);
    break;
  case O::less_than:
    left = Number(left < right ? 1 : 0);
    break;
  case O::less_than_equals:
    left = Number(left <= right ? 1 : 0);
    break;
  case O::equals:
    left = Number(left == right ? 1 : 0);
    break;
  case O::not_equals:
    left = Number(left != right ? 1 : 0);
    break;
  default:
    return {};
  }
  return left;
}
}  // namespace GD::Bc::Details
