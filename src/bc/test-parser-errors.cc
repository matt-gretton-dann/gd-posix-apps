/** \file test-parser-errors.cc
 * Parse-time diagnostic placement regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <memory>
#include <string_view>

#include "bc.hh"

TEST_CASE("Parser errors discard partial control flow", "[bc][parser][errors]")
{
  using GD::Bc::Instruction;
  std::string_view const text = GENERATE(
    "sqrt 1\n", "length 1\n", "abs 1\n", "define f() 1\n", "1 2\n", "1", "define f() {\nauto\n}\n",
    "define f() {\nreturn (1\n}\n", "if(0) {\n1+\n}\n", "while(0) {\n1+\n}\n");
  INFO("input = " << text);
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(text)), false);
  auto instructions = parser.parse();
  REQUIRE_FALSE(parser.seen_quit());
  REQUIRE(instructions != nullptr);
  REQUIRE(instructions->size() == 4);
  REQUIRE(instructions->at(0).opcode() == Instruction::Opcode::string);
  REQUIRE(std::get<std::string>(instructions->at(0).op1()).find("Error:") != std::string::npos);
  REQUIRE(instructions->at(1).opcode() == Instruction::Opcode::print);
  REQUIRE(std::get<Instruction::Stream>(instructions->at(1).op2()) == Instruction::Stream::error);
  REQUIRE(instructions->at(2).opcode() == Instruction::Opcode::quit);
  REQUIRE(std::get<unsigned>(instructions->at(2).op1()) == 1);
  REQUIRE(instructions->at(3).opcode() == Instruction::Opcode::eof);
}
