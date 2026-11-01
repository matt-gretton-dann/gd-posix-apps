/** \file test-parser-errors.cc
 * Parse-time diagnostic placement regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>

#include <catch2/catch.hpp>

#include <memory>
#include <sstream>
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

TEST_CASE("Fatal parser reuse preserves the original diagnostic stream",
          "[bc][parser][errors][reuse]")
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  std::string_view const text =
    GENERATE("1+", "1+\n2\n", "while(1) {\n1+\n}\n", "define f() {\nwhile(1) {\n1+\n}\n}\n",
             "1+define f() {\n}\n");
  bool const interactive = GENERATE(false, true);
  INFO("input = " << text << ", interactive = " << interactive);
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(text)), interactive);
  auto first = parser.parse();
  REQUIRE(first != nullptr);
  REQUIRE(first->size() == 4);
  std::string const diagnostic = std::get<std::string>(first->at(0).op1());
  REQUIRE(diagnostic.find("Error:") != std::string::npos);
  for (unsigned repeat = 0; repeat < 3; ++repeat) {
    INFO("repeat = " << repeat);
    auto code = parser.parse();
    REQUIRE(code != nullptr);
    REQUIRE(code->size() == 4);
    REQUIRE(code->at(0).opcode() == O::string);
    REQUIRE(std::get<std::string>(code->at(0).op1()) == diagnostic);
    REQUIRE(code->at(1).opcode() == O::print);
    REQUIRE(std::get<I::Offset>(code->at(1).op1()) == -1);
    REQUIRE(std::get<I::Stream>(code->at(1).op2()) == I::Stream::error);
    REQUIRE(code->at(2).opcode() == O::quit);
    REQUIRE(std::get<unsigned>(code->at(2).op1()) == 1);
    REQUIRE(code->at(3).opcode() == O::eof);
    REQUIRE_FALSE(parser.seen_quit());
    REQUIRE(std::get<std::string>(first->at(0).op1()) == diagnostic);
  }
}

TEST_CASE("Nonfatal interactive parser reuse advances input", "[bc][parser][reuse]")
{
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>("1\n2\n")), true);
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false);
  auto first = parser.parse();
  REQUIRE(vm.execute(*first));
  auto second = parser.parse();
  REQUIRE(vm.execute(*second));
  auto eof = parser.parse();
  REQUIRE_FALSE(vm.execute(*eof));
  REQUIRE(output.str() == "1\n2\n");
  REQUIRE(errors.str().empty());
  REQUIRE_FALSE(parser.seen_quit());
  REQUIRE(std::get<std::string>(first->at(0).op1()) == "1");
}
