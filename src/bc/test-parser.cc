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

TEST_CASE("GD::Bc::Parser - basic parsing", "[bc][parser]")
{
  auto [input, expected] = GENERATE(table<std::string_view, GD::Bc::Instructions>({
    {"\"Hello\"\n",
     {{GD::Bc::Instruction::Opcode::string, "Hello"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"quit\n", {{GD::Bc::Instruction::Opcode::quit, 0U}}},
    {"", {}},
    {";;\n", {}},
    {"\n", {}},
    {"{\"Hello\";\" World\"\n\"!\"\n}\n",
     {{GD::Bc::Instruction::Opcode::string, "Hello"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::string, " World"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::string, "!"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"10\n",
     {{GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a[0]\n",
     {{GD::Bc::Instruction::Opcode::number, "0"},
      {GD::Bc::Instruction::Opcode::array_element, 'a', -1},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"scale\n",
     {GD::Bc::Instruction{GD::Bc::Instruction::Opcode::scale},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"ibase\n",
     {GD::Bc::Instruction{GD::Bc::Instruction::Opcode::ibase},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"obase\n",
     {GD::Bc::Instruction{GD::Bc::Instruction::Opcode::obase},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"(10)\n",
     {{GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"-10\n",
     {{GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::negate, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"- -10\n",
     {{GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::negate, -1},
      {GD::Bc::Instruction::Opcode::negate, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a + b + c\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::add, -2, -1},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::add, -3, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a - b - c\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::subtract, -2, -1},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::subtract, -3, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a * b * c\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::multiply, -2, -1},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::multiply, -3, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a / b / c\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::divide, -2, -1},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::divide, -3, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a % b % c\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::modulo, -2, -1},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::modulo, -3, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a ^ b ^ c\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::power, -2, -1},
      {GD::Bc::Instruction::Opcode::load, -6},
      {GD::Bc::Instruction::Opcode::power, -1, -2},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"++a\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::add, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -4, -1},
      {GD::Bc::Instruction::Opcode::print, -2, GD::Bc::Instruction::Stream::stdout}}},
    {"--a\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::subtract, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -4, -1},
      {GD::Bc::Instruction::Opcode::print, -2, GD::Bc::Instruction::Stream::stdout}}},
    {"a++\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::add, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -4, -1},
      {GD::Bc::Instruction::Opcode::print, -4, GD::Bc::Instruction::Stream::stdout}}},
    {"a--\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::subtract, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -4, -1},
      {GD::Bc::Instruction::Opcode::print, -4, GD::Bc::Instruction::Stream::stdout}}},
    {"a = 10\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::store, -2, -1}}},
    {"a += 10\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::add, -1, -2},
      {GD::Bc::Instruction::Opcode::store, -4, -1}}},
    {"a -= 10\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::subtract, -1, -2},
      {GD::Bc::Instruction::Opcode::store, -4, -1}}},
    {"a *= 10\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::multiply, -1, -2},
      {GD::Bc::Instruction::Opcode::store, -4, -1}}},
    {"a /= 10\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::divide, -1, -2},
      {GD::Bc::Instruction::Opcode::store, -4, -1}}},
    {"a %= 10\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::modulo, -1, -2},
      {GD::Bc::Instruction::Opcode::store, -4, -1}}},
    {"a ^= 10\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::power, -1, -2},
      {GD::Bc::Instruction::Opcode::store, -4, -1}}},
    {"a = b += 10\n",  // Assignments bind right.
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::add, -1, -2},
      {GD::Bc::Instruction::Opcode::store, -4, -1},
      {GD::Bc::Instruction::Opcode::store, -6, -2}}},
    {"a = b * c\n",  // Assignment is low precedence
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::multiply, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -6, -1}}},
    {"length(a)\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::length, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"sqrt(a)\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::sqrt, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"scale(a)\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::scale_expr, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"1 + -c++ * 3 ^ 4 ^ 5 * (6 - 7) % 8 - length(9) + scale(sqrt(b))\n",  // try and catch
                                                                           // precedence issues
     {{GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::add, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -4, -1},
      {GD::Bc::Instruction::Opcode::negate, -4},
      {GD::Bc::Instruction::Opcode::number, "3"},
      {GD::Bc::Instruction::Opcode::number, "4"},
      {GD::Bc::Instruction::Opcode::number, "5"},
      {GD::Bc::Instruction::Opcode::power, -2, -1},
      {GD::Bc::Instruction::Opcode::power, -4, -1},
      {GD::Bc::Instruction::Opcode::multiply, -6, -1},
      {GD::Bc::Instruction::Opcode::number, "6"},
      {GD::Bc::Instruction::Opcode::number, "7"},
      {GD::Bc::Instruction::Opcode::subtract, -2, -1},
      {GD::Bc::Instruction::Opcode::multiply, -4, -1},
      {GD::Bc::Instruction::Opcode::number, "8"},
      {GD::Bc::Instruction::Opcode::modulo, -2, -1},
      {GD::Bc::Instruction::Opcode::add, -19, -1},
      {GD::Bc::Instruction::Opcode::number, "9"},
      {GD::Bc::Instruction::Opcode::length, -1},
      {GD::Bc::Instruction::Opcode::subtract, -3, -1},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::sqrt, -1},
      {GD::Bc::Instruction::Opcode::scale_expr, -1},
      {GD::Bc::Instruction::Opcode::add, -5, -1},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"while (a < 10) ++a;\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "10"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::less_than, -1, -2},
      {GD::Bc::Instruction::Opcode::branch_zero, -1, 8},
      {GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::add, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -4, -1},
      {GD::Bc::Instruction::Opcode::print, -2, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::branch, -11}}},
    {"if (a >= 0) a = 0;\n",
     {{GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "0"},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::less_than, -2, -1},
      {GD::Bc::Instruction::Opcode::branch_zero, -1, 4},
      {GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::number, "0"},
      {GD::Bc::Instruction::Opcode::store, -2, -1}}},
    {"while (1) break;\n",
     {{GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::branch_zero, -1, 3},
      {GD::Bc::Instruction::Opcode::branch, 2},
      {GD::Bc::Instruction::Opcode::branch, -3}}},
    {"1; while(2) { 3; while (4) { 5; break; 6; }; 7; }; 8;\n",
     {{GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::number, "2"},
      {GD::Bc::Instruction::Opcode::branch_zero, -1, 14},
      {GD::Bc::Instruction::Opcode::number, "3"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::number, "4"},
      {GD::Bc::Instruction::Opcode::branch_zero, -1, 7},
      {GD::Bc::Instruction::Opcode::number, "5"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::branch, 4},
      {GD::Bc::Instruction::Opcode::number, "6"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::branch, -7},
      {GD::Bc::Instruction::Opcode::number, "7"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::branch, -14},
      {GD::Bc::Instruction::Opcode::number, "8"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"for (1;2;3) { 4 }\n",
     {{GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::number, "2"},
      {GD::Bc::Instruction::Opcode::branch_zero, -1, 7},
      {GD::Bc::Instruction::Opcode::branch, 3},
      {GD::Bc::Instruction::Opcode::number, "3"},
      {GD::Bc::Instruction::Opcode::branch, -4},
      {GD::Bc::Instruction::Opcode::number, "4"},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
      {GD::Bc::Instruction::Opcode::branch, -4}}},
    {"a(1)\n",
     {GD::Bc::Instruction{GD::Bc::Instruction::Opcode::push_param_mark},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::push_param, -1},
      {GD::Bc::Instruction::Opcode::call, 'a', GD::Bc::Location("Library", 1, 3)},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a(b[])\n",
     {GD::Bc::Instruction{GD::Bc::Instruction::Opcode::push_param_mark},
      {GD::Bc::Instruction::Opcode::array, 'b'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::push_param, -1},
      {GD::Bc::Instruction::Opcode::call, 'a', GD::Bc::Location("Library", 1, 3)},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"a(1, (2 * 3), b(c), d[])\n",
     {GD::Bc::Instruction{GD::Bc::Instruction::Opcode::push_param_mark},
      {GD::Bc::Instruction::Opcode::number, "1"},
      {GD::Bc::Instruction::Opcode::push_param, -1},
      {GD::Bc::Instruction::Opcode::number, "2"},
      {GD::Bc::Instruction::Opcode::number, "3"},
      {GD::Bc::Instruction::Opcode::multiply, -2, -1},
      {GD::Bc::Instruction::Opcode::push_param, -1},
      GD::Bc::Instruction{GD::Bc::Instruction::Opcode::push_param_mark},
      {GD::Bc::Instruction::Opcode::variable, 'c'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::push_param, -1},
      {GD::Bc::Instruction::Opcode::call, 'b', GD::Bc::Location("Library", 1, 17)},
      {GD::Bc::Instruction::Opcode::push_param, -1},
      {GD::Bc::Instruction::Opcode::array, 'd'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::push_param, -1},
      {GD::Bc::Instruction::Opcode::call, 'a', GD::Bc::Location("Library", 1, 3)},
      {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout}}},
    {"define a() {\n}\n",
     {{GD::Bc::Instruction::Opcode::function_begin, GD::Bc::VariableMask(),
       GD::Bc::Location("Library", 1, 7)},
      {GD::Bc::Instruction::Opcode::number, "0"},
      {GD::Bc::Instruction::Opcode::return_, -1},
      {GD::Bc::Instruction::Opcode::function_end, 'a', -3}}},
    {"define a(b, c[]) {\n}\n",
     {{GD::Bc::Instruction::Opcode::function_begin, GD::Bc::VariableMask("b", "c"),
       GD::Bc::Location("Library", 1, 7)},
      GD::Bc::Instruction{GD::Bc::Instruction::Opcode::pop_param},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::store, -1, -2},
      GD::Bc::Instruction{GD::Bc::Instruction::Opcode::pop_param},
      {GD::Bc::Instruction::Opcode::array, 'c'},
      {GD::Bc::Instruction::Opcode::store, -1, -2},
      {GD::Bc::Instruction::Opcode::number, "0"},
      {GD::Bc::Instruction::Opcode::return_, -1},
      {GD::Bc::Instruction::Opcode::function_end, 'a', -9}}},
    {"define s(a,b) {\nauto r\nr = a + b\nreturn (r)\n}\n",
     {{GD::Bc::Instruction::Opcode::function_begin, GD::Bc::VariableMask("abr", ""),
       GD::Bc::Location("Library", 1, 7)},
      GD::Bc::Instruction{GD::Bc::Instruction::Opcode::pop_param},
      {GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::store, -1, -2},
      GD::Bc::Instruction{GD::Bc::Instruction::Opcode::pop_param},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::store, -1, -2},
      {GD::Bc::Instruction::Opcode::variable, 'r'},
      {GD::Bc::Instruction::Opcode::variable, 'a'},
      {GD::Bc::Instruction::Opcode::variable, 'b'},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::load, -2},
      {GD::Bc::Instruction::Opcode::add, -2, -1},
      {GD::Bc::Instruction::Opcode::store, -6, -1},
      {GD::Bc::Instruction::Opcode::variable, 'r'},
      {GD::Bc::Instruction::Opcode::load, -1},
      {GD::Bc::Instruction::Opcode::return_, -1},
      {GD::Bc::Instruction::Opcode::number, "0"},
      {GD::Bc::Instruction::Opcode::return_, -1},
      {GD::Bc::Instruction::Opcode::function_end, 's', -19}}},
  }));

  auto parser = GD::Bc::Parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(input)), false);
  auto instructions = parser.parse();
  INFO("Parsing: " << input);
  INFO("Expected:\n" << expected);
  INFO("Actual:\n" << *instructions);
  REQUIRE(instructions != nullptr);
  REQUIRE(instructions->size() == (expected.size() + 1));
  for (GD::Bc::Instructions::size_type i = 0; i < expected.size(); ++i) {
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

  REQUIRE(instructions->at(expected.size()).opcode() == GD::Bc::Instruction::Opcode::eof);
}
