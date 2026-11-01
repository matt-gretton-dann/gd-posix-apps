/** \file test-instruction.cc
 * Assert instruction diagnostics and variable-mask independence.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>
#include <catch2/catch.hpp>

#include <sstream>
#include <string>
#include <string_view>

#include "bc.hh"

TEST_CASE("Variable masks compare and iterate independent namespaces", "[bc][instruction][mask]")
{
  using GD::Bc::Array;
  using GD::Bc::Letter;
  using GD::Bc::Variable;
  using GD::Bc::VariableMask;
  VariableMask const reference("az", "by");
  REQUIRE(reference == VariableMask("za", "yb"));
  REQUIRE_FALSE(reference != VariableMask("az", "by"));
  REQUIRE(reference != VariableMask("a", "by"));
  REQUIRE(reference != VariableMask("az", "b"));
  REQUIRE(reference != VariableMask("a", "b"));
  REQUIRE(reference.contains(Variable('a')));
  REQUIRE(reference.contains(Variable('z')));
  REQUIRE_FALSE(reference.contains(Variable('b')));
  REQUIRE(reference.contains(Array('b')));
  REQUIRE(reference.contains(Array('y')));
  REQUIRE_FALSE(reference.contains(Array('a')));

  struct Case
  {
    std::string_view variables;
    std::string_view arrays;
    std::string_view expected;
  };
  Case const cases[] = {
    {"", "", ""},
    {"z", "", "z"},
    {"", "z", "z[]"},
    {"zaa", "ybb", "a, z, b[], y[]"},
    {"abcdefghijklmnopqrstuvwxyz", "",
     "a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z"},
  };
  for (auto const& test : cases) {
    INFO("variables = " << test.variables << ", arrays = " << test.arrays);
    VariableMask mask(test.variables, test.arrays);
    std::string variables;
    std::string arrays;
    mask.for_each_variable([&variables](Letter letter) {
      std::ostringstream text;
      text << letter;
      variables += text.str();
    });
    mask.for_each_array([&arrays](Letter letter) {
      std::ostringstream text;
      text << letter;
      arrays += text.str();
    });
    auto canonical = [](std::string_view input) {
      std::string output;
      for (auto letter : std::string_view("abcdefghijklmnopqrstuvwxyz")) {
        if (input.find(letter) != std::string_view::npos) {
          output += letter;
        }
      }
      return output;
    };
    REQUIRE(variables == canonical(test.variables));
    REQUIRE(arrays == canonical(test.arrays));
    std::ostringstream output;
    output << mask;
    REQUIRE(output.str() == test.expected);
  }
}

TEST_CASE("Instruction diagnostics preserve operands and positions", "[bc][instruction][format]")
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  using GD::Bc::Array;
  using GD::Bc::Letter;
  using GD::Bc::Variable;
  struct Case
  {
    I instruction;
    std::string_view text;
    unsigned count;
  };
  Case const cases[] = {
    {I(O::eof), "eof", 0},
    {I(O::push_param_mark), "push_param_mark", 0},
    {I(O::pop_param_mark), "pop_param_mark", 0},
    {I(O::pop_param), "pop_param", 0},
    {I(O::pop_param_array), "pop_param_array", 0},
    {I(O::scale), "scale", 0},
    {I(O::ibase), "ibase", 0},
    {I(O::obase), "obase", 0},
    {I(O::quit, 1U), "quit 1", 1},
    {I(O::string, "hello"), "string hello", 1},
    {I(O::number, "1.20"), "number 1.20", 1},
    {I(O::variable, Variable('z')), "variable z", 1},
    {I(O::array, Array('a')), "array a", 1},
    {I(O::negate, D(-1)), "negate -1", 1},
    {I(O::load, D(-1)), "load -1", 1},
    {I(O::scale_expr, D(-1)), "scale_expr -1", 1},
    {I(O::sqrt, D(-1)), "sqrt -1", 1},
    {I(O::abs, D(-1)), "abs -1", 1},
    {I(O::length, D(-1)), "length -1", 1},
    {I(O::branch, D(2)), "branch 2", 1},
    {I(O::return_, D(-1)), "return -1", 1},
    {I(O::push_param, D(-1)), "push_param -1", 1},
    {I(O::print, D(-1), I::Stream::output), "print -1, output", 2},
    {I(O::print, D(-1), I::Stream::error), "print -1, error", 2},
    {I(O::array_element, Array('b'), D(-2)), "array_element b, -2", 2},
    {I(O::function_end, Letter('f'), D(-3)), "function_end f, -3", 2},
    {I(O::add, D(-1), D(-2)), "add -1, -2", 2},
    {I(O::subtract, D(-1), D(-2)), "subtract -1, -2", 2},
    {I(O::multiply, D(-1), D(-2)), "multiply -1, -2", 2},
    {I(O::divide, D(-1), D(-2)), "divide -1, -2", 2},
    {I(O::modulo, D(-1), D(-2)), "modulo -1, -2", 2},
    {I(O::power, D(-1), D(-2)), "power -1, -2", 2},
    {I(O::store, D(-1), D(-2)), "store -1, -2", 2},
    {I(O::equals, D(-1), D(-2)), "equals -1, -2", 2},
    {I(O::less_than_equals, D(-1), D(-2)), "less_than_equals -1, -2", 2},
    {I(O::not_equals, D(-1), D(-2)), "not_equals -1, -2", 2},
    {I(O::less_than, D(-1), D(-2)), "less_than -1, -2", 2},
    {I(O::branch_zero, D(2), D(-1)), "branch_zero 2, -1", 2},
    {I(O::call, Letter('f'), GD::Bc::Location("input", 2, 3)), "call f, input:2:3", 2},
    {I(O::function_begin, GD::Bc::VariableMask("a", "b"), GD::Bc::Location("input", 2, 3)),
     "function_begin a, b[], input:2:3", 2},
  };
  for (auto const& test : cases) {
    INFO("instruction = " << test.text);
    REQUIRE(test.instruction.has_op1() == (test.count >= 1));
    REQUIRE(test.instruction.has_op2() == (test.count == 2));
    std::ostringstream text;
    text << test.instruction;
    REQUIRE(text.str() == test.text);
  }
  I eof(O::eof);
  REQUIRE_THROWS_AS(eof.op1(), std::logic_error);
  REQUIRE_THROWS_AS(eof.op2(), std::logic_error);
  I number(O::number, "1");
  REQUIRE_THROWS_AS(number.op2(), std::logic_error);
  number.op1(std::string("2"));
  REQUIRE(std::get<std::string>(number.op1()) == "2");
  I sum(O::add, D(-1), D(-2));
  sum.op1(D(-3));
  sum.op2(D(-4));
  std::ostringstream listing;
  listing << GD::Bc::Instructions{number, sum, eof};
  REQUIRE(listing.str() == "0\tnumber 2\n1\tadd -3, -4\n2\teof\n");
}

TEST_CASE("Decoded literal memos follow instruction copy move and replacement",
          "[bc][instruction][literal]")
{
  using I = GD::Bc::Instruction;
  using GD::Bc::Number;
  I source(I::Opcode::number, std::string("10"));
  auto snapshot = source.decoded_number(10);
  snapshot.add(Number(1));
  CHECK(snapshot == Number(11));
  CHECK(source.decoded_number(10) == Number(10));
  I copy(source);
  source.op1(std::string("11"));
  CHECK(source.decoded_number(2) == Number(3));
  CHECK(copy.decoded_number(2) == Number(2));
  I moved(std::move(copy));
  CHECK(moved.decoded_number(10) == Number(10));
  moved.op1(std::string("100"));
  CHECK(moved.decoded_number(2) == Number(4));
  I assigned(I::Opcode::number, std::string("77"));
  CHECK(assigned.decoded_number(10) == Number(77));
  assigned = moved;
  CHECK(assigned.decoded_number(2) == Number(4));
  moved.op1(std::string("10"));
  CHECK(assigned.decoded_number(2) == Number(4));
  CHECK(moved.decoded_number(2) == Number(2));
  assigned = std::move(moved);
  CHECK(assigned.decoded_number(10) == Number(10));
  auto const* alias = &assigned;
  assigned = *alias;
  CHECK(assigned.decoded_number(10) == Number(10));
}
