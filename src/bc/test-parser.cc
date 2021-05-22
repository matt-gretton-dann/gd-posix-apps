/** \file   test-parser.cc
 *  \brief  Tests for bc's Parser
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <memory>
#include <string>

#include "bc.hh"
#include <string_view>

TEST_CASE("Parser - basic parsing", "[bc][parser]")
{
  using namespace GD::Bc;
  auto [input, expected] = GENERATE(table<std::string_view, Instructions>({
    {"\"Hello\"\n",
     {Instruction(Instruction::Opcode::string, "Hello"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"quit\n", {Instruction(Instruction::Opcode::quit, 0U)}},
    {"", Instructions{}},
    {";;\n", Instructions{}},
    {"\n", Instructions{}},
    {"{\"Hello\";\" World\"\n\"!\"\n}\n",
     {Instruction(Instruction::Opcode::string, "Hello"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::string, " World"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::string, "!"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"10\n",
     {Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a[0]\n",
     {Instruction(Instruction::Opcode::number, "0"),
      Instruction(Instruction::Opcode::array_element, Array('a'), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"scale\n",
     {Instruction(Instruction::Opcode::scale),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"ibase\n",
     {Instruction(Instruction::Opcode::ibase),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),

      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"obase\n",
     {Instruction(Instruction::Opcode::obase),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),

      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"(10)\n",
     {Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"-10\n",
     {Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::negate, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"- -10\n",
     {Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::negate, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::negate, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a + b + c\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-3), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a - b - c\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::subtract, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::subtract, Instruction::Offset(-3), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a * b * c\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::multiply, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::multiply, Instruction::Offset(-3), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a / b / c\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::divide, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::divide, Instruction::Offset(-3), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a % b % c\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::modulo, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::modulo, Instruction::Offset(-3), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a ^ b ^ c\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::power, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-6)),
      Instruction(Instruction::Opcode::power, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"++a\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-2),
                  Instruction::Stream::stdout)}},
    {"--a\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::subtract, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1)),
      {Instruction::Opcode::print, Instruction::Offset(-2), Instruction::Stream::stdout}}},
    {"a++\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-4),
                  Instruction::Stream::stdout)}},
    {"a--\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::subtract, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-4),
                  Instruction::Stream::stdout)}},
    {"a = 10\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-2), Instruction::Offset(-1))}},
    {"a += 10\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1))}},
    {"a -= 10\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::subtract, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1))}},
    {"a *= 10\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::multiply, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1))}},
    {"a /= 10\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::divide, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1))}},
    {"a %= 10\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::modulo, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1))}},
    {"a ^= 10\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::power, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1))}},
    {"a = b += 10\n",  // Assignments bind right.
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-6), Instruction::Offset(-2))}},
    {"a = b * c\n",  // Assignment is low precedence
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::multiply, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-6), Instruction::Offset(-1))}},
    {"length(a)\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::length, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"sqrt(a)\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::sqrt, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"scale(a)\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::scale_expr, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"1 + -c++ * 3 ^ 4 ^ 5 * (6 - 7) % 8 - length(9) + scale(sqrt(b))\n",  // try and catch
                                                                           // precedence issues
     {Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::negate, Instruction::Offset(-4)),
      Instruction(Instruction::Opcode::number, "3"),
      Instruction(Instruction::Opcode::number, "4"),
      Instruction(Instruction::Opcode::number, "5"),
      Instruction(Instruction::Opcode::power, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::power, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::multiply, Instruction::Offset(-6), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "6"),
      Instruction(Instruction::Opcode::number, "7"),
      Instruction(Instruction::Opcode::subtract, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::multiply, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "8"),
      Instruction(Instruction::Opcode::modulo, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-19), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "9"),
      Instruction(Instruction::Opcode::length, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::subtract, Instruction::Offset(-3), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::sqrt, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::scale_expr, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-5), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"while (a < 10) ++a;\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "10"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::less_than, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::branch_zero, Instruction::Offset(-1),
                  Instruction::Offset(8)),
      Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-4), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-2), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(-11))}},
    {"if (a >= 0) a = 0;\n",
     {Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "0"),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::less_than_equals, Instruction::Offset(-2),
                  Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::branch_zero, Instruction::Offset(-1),
                  Instruction::Offset(4)),
      Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::number, "0"),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-2), Instruction::Offset(-1))}},
    {"while (1) break;\n",
     {Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::branch_zero, Instruction::Offset(-1),
                  Instruction::Offset(3)),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(2)),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(-3))}},
    {"1; while(2) { 3; while (4) { 5; break; 6; }; 7; }; 8;\n",
     {Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::number, "2"),
      Instruction(Instruction::Opcode::branch_zero, Instruction::Offset(-1),
                  Instruction::Offset(14)),
      Instruction(Instruction::Opcode::number, "3"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::number, "4"),
      Instruction(Instruction::Opcode::branch_zero, Instruction::Offset(-1),
                  Instruction::Offset(7)),
      Instruction(Instruction::Opcode::number, "5"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(4)),
      Instruction(Instruction::Opcode::number, "6"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(-7)),
      Instruction(Instruction::Opcode::number, "7"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(-14)),
      Instruction(Instruction::Opcode::number, "8"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"for (1;2;3) { 4 }\n",
     {Instruction(Instruction::Opcode::number, "1"), Instruction(Instruction::Opcode::number, "2"),
      Instruction(Instruction::Opcode::branch_zero, Instruction::Offset(-1),
                  Instruction::Offset(7)),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(3)),
      Instruction(Instruction::Opcode::number, "3"),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(-4)),
      Instruction(Instruction::Opcode::number, "4"),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1), Instruction::Stream::stdout),
      Instruction(Instruction::Opcode::branch, Instruction::Offset(-4))}},
    {"a(1)\n",
     {Instruction(Instruction::Opcode::push_param_mark),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::push_param, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::call, Letter('a'), Location("Library", 1, 3)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a(b[])\n",
     {Instruction(Instruction::Opcode::push_param_mark),
      Instruction(Instruction::Opcode::array, Array('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::push_param, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::call, Letter('a'), Location("Library", 1, 3)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"a(1, (2 * 3), b(c), d[])\n",
     {Instruction(Instruction::Opcode::push_param_mark),
      Instruction(Instruction::Opcode::number, "1"),
      Instruction(Instruction::Opcode::push_param, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "2"), Instruction(Instruction::Opcode::number, "3"),
      Instruction(Instruction::Opcode::multiply, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::push_param, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::push_param_mark),
      Instruction(Instruction::Opcode::variable, Variable('c')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::push_param, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::call, Letter('b'), Location("Library", 1, 17)),
      Instruction(Instruction::Opcode::push_param, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::array, Array('d')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::push_param, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::call, Letter('a'), Location("Library", 1, 3)),
      Instruction(Instruction::Opcode::print, Instruction::Offset(-1),
                  Instruction::Stream::stdout)}},
    {"define a() {\n}\n",
     {Instruction(Instruction::Opcode::function_begin, VariableMask(), Location("Library", 1, 7)),
      Instruction(Instruction::Opcode::pop_param_mark),
      Instruction(Instruction::Opcode::number, "0"),
      Instruction(Instruction::Opcode::return_, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::function_end, Letter('a'), Instruction::Offset(-4))}},
    {"define a(b, c[]) {\n}\n",
     {Instruction(Instruction::Opcode::function_begin, VariableMask("b", "c"),
                  Location("Library", 1, 7)),
      Instruction(Instruction::Opcode::pop_param),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::pop_param_array),
      Instruction(Instruction::Opcode::array, Array('c')),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::pop_param_mark),
      Instruction(Instruction::Opcode::number, "0"),
      Instruction(Instruction::Opcode::return_, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::function_end, Letter('a'), Instruction::Offset(-10))}},
    {"define s(a,b) {\nauto r\nr = a + b\nreturn (r)\n}\n",
     {Instruction(Instruction::Opcode::function_begin, VariableMask("abr", ""),
                  Location("Library", 1, 7)),
      Instruction(Instruction::Opcode::pop_param),
      Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::pop_param),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-1), Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::pop_param_mark),
      Instruction(Instruction::Opcode::variable, Variable('r')),
      Instruction(Instruction::Opcode::variable, Variable('a')),
      Instruction(Instruction::Opcode::variable, Variable('b')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-2)),
      Instruction(Instruction::Opcode::add, Instruction::Offset(-2), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::store, Instruction::Offset(-6), Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::variable, Variable('r')),
      Instruction(Instruction::Opcode::load, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::return_, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::number, "0"),
      Instruction(Instruction::Opcode::return_, Instruction::Offset(-1)),
      Instruction(Instruction::Opcode::function_end, Letter('s'), Instruction::Offset(-20))}},
  }));

  auto parser = Parser(std::make_unique<Lexer>(std::make_unique<StringReader>(input)), false);
  auto instructions = parser.parse();
  INFO("Parsing: " << input);
  INFO("Expected:\n" << expected);
  INFO("Actual:\n" << *instructions);
  REQUIRE(instructions != nullptr);
  REQUIRE(instructions->size() == (expected.size() + 1));
  for (Instructions::size_type i = 0; i < expected.size(); ++i) {
    INFO("Expected instruction " << i << ": " << expected[i]);
    INFO("Actual instruction " << i << ": " << instructions->at(i));
    REQUIRE(expected[i].opcode() == instructions->at(i).opcode());
    if (expected[i].has_op1()) {
      REQUIRE(expected[i].op1() == instructions->at(i).op1());
    }
    if (expected[i].has_op2()) {
      REQUIRE(expected[i].op2() == instructions->at(i).op2());
    }
  }

  REQUIRE(instructions->at(expected.size()).opcode() == Instruction::Opcode::eof);
}
