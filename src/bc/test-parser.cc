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
     {
       {GD::Bc::Instruction::Opcode::string, "Hello"},
       {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
       {GD::Bc::Instruction::Opcode::string, " World"},
       {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
       {GD::Bc::Instruction::Opcode::string, "!"},
       {GD::Bc::Instruction::Opcode::print, -1, GD::Bc::Instruction::Stream::stdout},
     }},
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
