/** \file test-vm-offsets.cc
 * Isolated offset-verifier fixtures using valid instruction operand schemas.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <array>
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
  else if (mode.substr(0, 7) == "binary-") {
    std::array<std::pair<std::string_view, O>, 10> const operations{{
      {"add", O::add},
      {"subtract", O::subtract},
      {"multiply", O::multiply},
      {"divide", O::divide},
      {"modulo", O::modulo},
      {"power", O::power},
      {"equals", O::equals},
      {"not-equals", O::not_equals},
      {"less-than", O::less_than},
      {"less-equal", O::less_than_equals},
    }};
    bool found = false;
    for (auto const& operation : operations) {
      if (mode.substr(7, mode.size() - 9) == operation.first) {
        code.emplace_back(mode.back() == '1' ? O::string : O::number, "1");
        code.emplace_back(mode.back() == '2' ? O::string : O::number, "1");
        code.emplace_back(operation.second, D(-2), D(-1));
        found = true;
        break;
      }
    }
    if (!found) {
      return 2;
    }
  }
  else if (mode == "sqrt-string" || mode == "abs-string" || mode == "scale-string") {
    code.emplace_back(O::string, "bad");
    code.emplace_back(mode == "sqrt-string"  ? O::sqrt
                      : mode == "abs-string" ? O::abs
                                             : O::scale_expr,
                      D(-1));
  }
  else if (mode == "print-empty") {
    code.emplace_back(O::branch, D(1));
    code.emplace_back(O::print, D(-1), I::Stream::output);
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
  else if (mode == "store-number-location") {
    code.emplace_back(O::number, "1");
    code.emplace_back(O::number, "2");
    code.emplace_back(O::store, D(-2), D(-1));
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
  else if (mode == "call-without-mark" || mode == "push-without-mark" ||
           mode == "pop-without-function" || mode == "param-without-function" ||
           mode == "array-param-without-function" || mode == "unmatched-mark" ||
           mode == "join-marker") {
    if (mode == "call-without-mark") {
      code.emplace_back(O::call, GD::Bc::Letter('f'), GD::Bc::Location("fixture", 1, 1));
    }
    else if (mode == "push-without-mark") {
      code.emplace_back(O::number, "1");
      code.emplace_back(O::push_param, D(-1));
    }
    else if (mode == "join-marker") {
      code.emplace_back(O::number, "0");
      code.emplace_back(O::branch_zero, D(-1), D(2));
      code.emplace_back(O::push_param_mark);
    }
    else {
      O const operation = mode == "pop-without-function"           ? O::pop_param_mark
                          : mode == "param-without-function"       ? O::pop_param
                          : mode == "array-param-without-function" ? O::pop_param_array
                                                                   : O::push_param_mark;
      code.emplace_back(operation);
    }
    code.emplace_back(O::eof);
  }
  else if (mode == "function-missing-pop" || mode == "function-duplicate-pop" ||
           mode == "function-bypass-pop" || mode == "function-prepop-loop" ||
           mode == "function-owned-loop" || mode == "function-param-after-pop" ||
           mode == "function-array-param-after-pop" || mode == "function-return-open-mark" ||
           mode == "function-call-without-mark" || mode == "function-pop-owned-mark") {
    auto const begin = code.size();
    code.emplace_back(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("fixture", 1, 1));
    if (mode == "function-bypass-pop") {
      code.emplace_back(O::number, "0");
      code.emplace_back(O::branch_zero, D(-1), D(2));
    }
    if (mode == "function-prepop-loop") {
      code.emplace_back(O::branch, D(0));
    }
    if (mode == "function-pop-owned-mark") {
      code.emplace_back(O::push_param_mark);
    }
    if (mode != "function-missing-pop") {
      code.emplace_back(O::pop_param_mark);
    }
    if (mode == "function-duplicate-pop") {
      code.emplace_back(O::pop_param_mark);
    }
    if (mode == "function-param-after-pop" || mode == "function-array-param-after-pop") {
      code.emplace_back(mode == "function-param-after-pop" ? O::pop_param : O::pop_param_array);
    }
    if (mode == "function-call-without-mark") {
      code.emplace_back(O::call, GD::Bc::Letter('f'), GD::Bc::Location("fixture", 2, 1));
    }
    if (mode == "function-return-open-mark" || mode == "function-owned-loop") {
      code.emplace_back(O::push_param_mark);
      if (mode == "function-owned-loop") {
        code.emplace_back(O::branch, D(-1));
      }
    }
    code.emplace_back(O::number, "0");
    code.emplace_back(O::return_, D(-1));
    code.emplace_back(O::function_end, GD::Bc::Letter('f'), D(begin) - D(code.size()));
    code.emplace_back(O::eof);
  }
  else if (mode == "valid-alternative-calls") {
    code.emplace_back(O::function_begin, GD::Bc::VariableMask(),
                      GD::Bc::Location("fixture", 1, 1));                                // 2
    code.emplace_back(O::pop_param_mark);                                                // 3
    code.emplace_back(O::number, "7");                                                   // 4
    code.emplace_back(O::return_, D(-1));                                                // 5
    code.emplace_back(O::function_end, GD::Bc::Letter('f'), D(-4));                      // 6
    code.emplace_back(O::variable, GD::Bc::Variable('a'));                               // 7
    code.emplace_back(O::push_param_mark);                                               // 8
    code.emplace_back(O::number, "0");                                                   // 9
    code.emplace_back(O::branch_zero, D(-1), D(4));                                      // 10 -> 14
    code.emplace_back(O::call, GD::Bc::Letter('f'), GD::Bc::Location("fixture", 2, 1));  // 11
    code.emplace_back(O::store, D(-5), D(-1));                                           // 12
    code.emplace_back(O::branch, D(3));                                                  // 13 -> 16
    code.emplace_back(O::call, GD::Bc::Letter('f'), GD::Bc::Location("fixture", 3, 1));  // 14
    code.emplace_back(O::store, D(-8), D(-1));                                           // 15
    code.emplace_back(O::load, D(-9));                                                   // 16
    code.emplace_back(O::print, D(-1), I::Stream::output);
    code.emplace_back(O::eof);
  }
  else if (mode == "skipped-producer") {
    code.emplace_back(O::branch, D(2));
    code.emplace_back(O::number, "1");
    code.emplace_back(O::print, D(-1), I::Stream::output);
    code.emplace_back(O::eof);
  }
  else if (mode == "join-producer") {
    code.emplace_back(O::number, "0");
    code.emplace_back(O::branch_zero, D(-1), D(3));
    code.emplace_back(O::number, "7");
    code.emplace_back(O::branch, D(1));
    code.emplace_back(O::print, D(-2), I::Stream::output);
    code.emplace_back(O::eof);
  }
  else if (mode == "self-producer") {
    code.emplace_back(O::negate, D(0));
  }
  else if (mode == "middle-eof") {
    code.emplace_back(O::eof);
    code.emplace_back(O::number, "0");
  }
  else if (mode == "missing-function-end" || mode == "mismatched-function-end" ||
           mode == "branch-into-function" || mode == "branch-out-of-function" ||
           mode == "cross-function-result" || mode == "nested-function" ||
           mode == "orphan-function-end") {
    if (mode == "orphan-function-end") {
      code.emplace_back(O::function_end, GD::Bc::Letter('f'), D(-1));
    }
    else {
      if (mode == "branch-into-function") {
        code.emplace_back(O::branch, D(2));
      }
      if (mode == "cross-function-result") {
        code.emplace_back(O::number, "7");
      }
      auto const begin = code.size();
      code.emplace_back(O::function_begin, GD::Bc::VariableMask(),
                        GD::Bc::Location("fixture", 1, 1));
      if (mode == "nested-function") {
        code.emplace_back(O::function_begin, GD::Bc::VariableMask(),
                          GD::Bc::Location("fixture", 2, 1));
      }
      code.emplace_back(O::pop_param_mark);
      if (mode == "branch-out-of-function") {
        code.emplace_back(O::branch, D(4));
      }
      code.emplace_back(O::number, "0");
      code.emplace_back(O::return_, mode == "cross-function-result" ? D(-4) : D(-1));
      if (mode == "nested-function") {
        code.emplace_back(O::function_end, GD::Bc::Letter('g'), D(begin + 1) - D(code.size()));
      }
      if (mode != "missing-function-end") {
        code.emplace_back(O::function_end, GD::Bc::Letter('f'),
                          D(begin) - D(code.size()) - (mode == "mismatched-function-end" ? 1 : 0));
      }
    }
    code.emplace_back(O::eof);
  }
  else if (mode == "valid-control-join") {
    code.emplace_back(O::variable, GD::Bc::Variable('a'));  // 2
    code.emplace_back(O::number, "0");                      // 3
    code.emplace_back(O::branch_zero, D(-1), D(4));         // 4 -> 8
    code.emplace_back(O::number, "7");                      // 5
    code.emplace_back(O::store, D(-4), D(-1));              // 6
    code.emplace_back(O::branch, D(3));                     // 7 -> 10
    code.emplace_back(O::number, "9");                      // 8
    code.emplace_back(O::store, D(-7), D(-1));              // 9
    code.emplace_back(O::load, D(-8));                      // 10
    code.emplace_back(O::print, D(-1), I::Stream::output);
    code.emplace_back(O::eof);
  }
  else if (mode == "valid-forward-result") {
    // A producer later in layout can execute before its consumer.
    code.emplace_back(O::branch, D(3));
    code.emplace_back(O::print, D(2), I::Stream::output);
    code.emplace_back(O::branch, D(3));
    code.emplace_back(O::number, "42");
    code.emplace_back(O::branch, D(-3));
    code.emplace_back(O::eof);
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
