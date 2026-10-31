/** \file test-vm-execution.cc
 * Exact semantic oracles for direct and parsed VM execution.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <cstdint>
#include <random>
#include <sstream>
#include <string>
#include <tuple>

#include "bc.hh"

namespace {
using I = GD::Bc::Instruction;
using O = I::Opcode;
using D = I::Offset;

auto execute(GD::Bc::Instructions code) -> std::string
{
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false);
  CHECK_FALSE(vm.execute(code));
  CHECK(errors.str().empty());
  return output.str();
}

auto literal(GD::Bc::Instructions& code, int64_t value) -> D
{
  code.emplace_back(O::number, std::to_string(value < 0 ? -value : value));
  if (value < 0) {
    code.emplace_back(O::negate, D(-1));
  }
  return D(code.size() - 1);
}

struct Expression
{
  std::string text;
  int64_t value;
  D result;
};

auto expression(std::mt19937& rng, unsigned depth, GD::Bc::Instructions& code) -> Expression
{
  if (depth == 0 || rng() % 4 == 0) {
    int64_t const value = int64_t(rng() % 19) - 9;
    return {value < 0 ? "(" + std::to_string(value) + ")" : std::to_string(value), value,
            literal(code, value)};
  }
  auto const lhs = expression(rng, depth - 1, code);
  auto const rhs = expression(rng, depth - 1, code);
  unsigned const operation = static_cast<unsigned>(rng() % 3);
  char const symbol = operation == 0 ? '+' : operation == 1 ? '-' : '*';
  O const opcode = operation == 0 ? O::add : operation == 1 ? O::subtract : O::multiply;
  int64_t const value = operation == 0   ? lhs.value + rhs.value
                        : operation == 1 ? lhs.value - rhs.value
                                         : lhs.value * rhs.value;
  D const result = D(code.size());
  code.emplace_back(opcode, lhs.result - result, rhs.result - result);
  return {"(" + lhs.text + symbol + rhs.text + ")", value, result};
}
}  // namespace

TEST_CASE("VM binary opcodes agree with native integer arithmetic", "[bc][vm][execution]")
{
  for (int64_t lhs = -4; lhs <= 4; ++lhs) {
    for (int64_t rhs = -4; rhs <= 4; ++rhs) {
      for (O opcode : {O::add, O::subtract, O::multiply, O::divide, O::modulo, O::power, O::equals,
                       O::not_equals, O::less_than, O::less_than_equals}) {
        if (((opcode == O::divide || opcode == O::modulo) && rhs == 0) ||
            (opcode == O::power && rhs < 0)) {
          continue;  // Runtime-error and fractional reciprocal cases have separate tests.
        }
        int64_t expected = 0;
        switch (opcode) {
        case O::add:
          expected = lhs + rhs;
          break;
        case O::subtract:
          expected = lhs - rhs;
          break;
        case O::multiply:
          expected = lhs * rhs;
          break;
        case O::divide:
          expected = lhs / rhs;
          break;
        case O::modulo:
          expected = lhs % rhs;
          break;
        case O::power:
          expected = 1;
          for (int64_t n = 0; n < rhs; ++n) {
            expected *= lhs;
          }
          break;
        case O::equals:
          expected = lhs == rhs;
          break;
        case O::not_equals:
          expected = lhs != rhs;
          break;
        case O::less_than:
          expected = lhs < rhs;
          break;
        case O::less_than_equals:
          expected = lhs <= rhs;
          break;
        default:
          FAIL("unexpected binary test opcode");
        }
        INFO("lhs = " << lhs << ", rhs = " << rhs << ", opcode = " << unsigned(opcode));
        GD::Bc::Instructions code;
        D const left = literal(code, lhs);
        D const right = literal(code, rhs);
        D const at = D(code.size());
        code.emplace_back(opcode, left - at, right - at);
        code.emplace_back(O::print, D(-1), I::Stream::output);
        code.emplace_back(O::eof);
        CHECK(execute(code) == std::to_string(expected) + "\n");
      }
    }
  }
}

TEST_CASE("Generated expressions agree across grammar bytecode and native oracles",
          "[bc][vm][execution][property]")
{
  constexpr unsigned seed = 197215;
  std::mt19937 rng(seed);
  for (unsigned sample = 0; sample < 200; ++sample) {
    GD::Bc::Instructions code;
    auto const value = expression(rng, 3, code);
    std::string const source = value.text + "\n";
    INFO("seed = " << seed << ", sample = " << sample << ", source = " << source);
    code.emplace_back(O::print, value.result - D(code.size()), I::Stream::output);
    code.emplace_back(O::eof);
    std::string const expected = std::to_string(value.value) + "\n";
    CHECK(execute(code) == expected);
    GD::Bc::Parser parser(
      std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
    auto parsed = parser.parse();
    REQUIRE(parsed != nullptr);
    CHECK(execute(*parsed) == expected);
  }
}

TEST_CASE("VM unary operations have exact observable results", "[bc][vm][execution]")
{
  for (auto const& entry :
       {std::make_tuple(O::negate, "7", "-7"), std::make_tuple(O::abs, "7", "7"),
        std::make_tuple(O::sqrt, "81", "9"), std::make_tuple(O::scale_expr, "12.340", "3"),
        std::make_tuple(O::length, "123", "3")}) {
    GD::Bc::Instructions code{I(O::number, std::get<1>(entry)), I(std::get<0>(entry), D(-1)),
                              I(O::print, D(-1), I::Stream::output), I(O::eof)};
    CHECK(execute(code) == std::string(std::get<2>(entry)) + "\n");
  }
  GD::Bc::Instructions code{I(O::number, "7"), I(O::negate, D(-1)), I(O::abs, D(-1)),
                            I(O::print, D(-1), I::Stream::output), I(O::eof)};
  CHECK(execute(code) == "7\n");
}

TEST_CASE("VM conditional and unconditional branches select one output", "[bc][vm][execution]")
{
  for (auto condition : {"0", "1"}) {
    GD::Bc::Instructions code{I(O::number, condition),
                              I(O::branch_zero, D(-1), D(4)),
                              I(O::string, "nonzero"),
                              I(O::print, D(-1), I::Stream::output),
                              I(O::branch, D(3)),
                              I(O::string, "zero"),
                              I(O::print, D(-1), I::Stream::output),
                              I(O::eof)};
    CHECK(execute(code) == (std::string(condition) == "0" ? "zero" : "nonzero"));
  }
}

TEST_CASE("VM special register stores survive instruction packs", "[bc][vm][execution]")
{
  for (O opcode : {O::scale, O::ibase, O::obase}) {
    std::ostringstream output;
    std::ostringstream errors;
    GD::Bc::VM vm(output, errors, false);
    GD::Bc::Instructions store{I(opcode), I(O::number, "16"), I(O::store, D(-2), D(-1)), I(O::eof)};
    CHECK_FALSE(vm.execute(store));
    GD::Bc::Instructions load{I(opcode), I(O::load, D(-1)), I(O::print, D(-1), I::Stream::output),
                              I(O::eof)};
    CHECK_FALSE(vm.execute(load));
    CHECK(output.str() == (opcode == O::obase ? "10\n" : "16\n"));
    CHECK(errors.str().empty());
  }
}
