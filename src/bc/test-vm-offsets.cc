/** \file test-vm-offsets.cc
 * Isolated offset-verifier fixtures using valid instruction operand schemas.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <iostream>
#include <limits>
#include <string_view>

#include "bc.hh"

auto main(int argc, char** argv) -> int
{
  if (argc != 2) {
    return 2;
  }
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  std::string_view const mode(argv[1]);
  // Invalid streams must be rejected before this observable prefix executes.
  GD::Bc::Instructions code{I(O::string, "EXECUTED\n"), I(O::print, D(-1), I::Stream::output)};
  if (mode == "op1-end") {
    code.emplace_back(O::print, D(1), I::Stream::output);
  }
  else if (mode == "op1-after") {
    code.emplace_back(O::print, D(2), I::Stream::output);
  }
  else if (mode == "op1-before") {
    code.emplace_back(O::print, D(-3), I::Stream::output);
  }
  else if (mode == "op1-min") {
    code.emplace_back(O::print, std::numeric_limits<D>::min(), I::Stream::output);
  }
  else if (mode == "op2-end" || mode == "op2-after" || mode == "op2-before" || mode == "op2-min") {
    code.emplace_back(O::number, "1");
    D offset = mode == "op2-end" ? 1 : (mode == "op2-after" ? 2 : -4);
    if (mode == "op2-min") {
      offset = std::numeric_limits<D>::min();
    }
    code.emplace_back(O::add, D(-1), offset);
  }
  else if (mode == "array-index-after") {
    code.emplace_back(O::array_element, GD::Bc::Array('a'), D(99));
  }
  else if (mode == "conditional-after") {
    code.emplace_back(O::number, "0");
    code.emplace_back(O::branch_zero, D(-1), D(2));
  }
  else if (mode == "add-string" || mode == "add-empty" || mode == "add-location") {
    if (mode == "add-string") {
      code.emplace_back(O::string, "bad");
    }
    else if (mode == "add-empty") {
      code.emplace_back(O::branch, D(1));
    }
    else {
      code.emplace_back(O::variable, GD::Bc::Variable('a'));
    }
    code.emplace_back(O::number, "1");
    code.emplace_back(O::add, D(-2), D(-1));
  }
  else if (mode == "load-number" || mode == "length-string" || mode == "branch-string" ||
           mode == "index-string" || mode == "push-string" || mode == "negate-string" ||
           mode == "return-string") {
    code.emplace_back(mode == "load-number" ? O::number : O::string, "1");
    if (mode == "branch-string") {
      code.emplace_back(O::branch_zero, D(-1), D(1));
    }
    else if (mode == "index-string") {
      code.emplace_back(O::array_element, GD::Bc::Array('a'), D(-1));
    }
    else {
      O const op = mode == "load-number"     ? O::load
                   : mode == "length-string" ? O::length
                   : mode == "push-string"   ? O::push_param
                   : mode == "return-string" ? O::return_
                                             : O::negate;
      code.emplace_back(op, D(-1));
    }
  }
  else if (mode == "store-number-array" || mode == "store-array-number") {
    if (mode == "store-number-array") {
      code.emplace_back(O::array, GD::Bc::Array('a'));
      code.emplace_back(O::number, "1");
    }
    else {
      code.emplace_back(O::variable, GD::Bc::Variable('a'));
      code.emplace_back(O::pop_param_array);
    }
    code.emplace_back(O::store, D(-2), D(-1));
  }
  else if (mode == "valid-forward-result") {
    // A producer later in layout can execute before its consumer.
    code.emplace_back(O::branch, D(3));
    code.emplace_back(O::print, D(2), I::Stream::output);
    code.emplace_back(O::eof);
    code.emplace_back(O::number, "42");
    code.emplace_back(O::branch, D(-3));
  }
  else if (mode == "valid-location-print") {
    // Print intentionally supports diagnostic representations of locations.
    code.emplace_back(O::variable, GD::Bc::Variable('a'));
    code.emplace_back(O::print, D(-1), I::Stream::output);
    code.emplace_back(O::eof);
  }
  else if (mode == "branch-end") {
    code.emplace_back(O::branch, D(1));
  }
  else if (mode == "conditional-end") {
    code.emplace_back(O::number, "0");
    code.emplace_back(O::branch_zero, D(-1), D(1));
  }
  else if (mode == "last-result") {
    code.emplace_back(O::number, "42");
    code.emplace_back(O::print, D(-1), I::Stream::output);
    code.emplace_back(O::eof);
  }
  else {
    return 2;
  }
  GD::Bc::VM vm(std::cout, std::cerr, false);
  try {
    (void)vm.execute(code);
  }
  catch (std::exception const& error) {
    std::cerr << "Unexpected execution exception: " << error.what() << '\n';
    return 2;
  }
  return 0;
}
