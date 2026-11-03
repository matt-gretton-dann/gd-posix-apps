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
             "1+define f() {\n}\n", "1+;2\n", "{1+;2}\n", "f(1,)\n", "f(1+,2)\n",
             "define f(a,,b) {\nreturn(0)\n}\n",
             "define f() {\nauto a,,b;\nreturn(0)\n}\n",
             "define f() {\nreturn(1+)\n}\n", "return(1)\n", "1?2\n");
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

TEST_CASE("Read-time quit suppresses errors in abandoned parser contexts",
          "[bc][parser][errors][quit]")
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  std::string_view const text = GENERATE(
    "quit\n\"unterminated", "if(1) {quit\n\"unterminated",
    "while(1) {quit\n\"unterminated", "define f(a) {\nauto b;\nquit\n\"unterminated");
  bool const interactive = GENERATE(false, true);
  INFO("input = " << text << ", interactive = " << interactive);
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(text)), interactive);
  for (unsigned repeat = 0; repeat != 3; ++repeat) {
    auto code = parser.parse();
    REQUIRE(code != nullptr);
    REQUIRE(parser.seen_quit());
    // The driver discards this partial item. A missing closing delimiter after
    // quit must not become a fatal diagnostic or consume the trailing string.
    for (auto const& instruction : *code) {
      if (instruction.opcode() == O::quit) {
        REQUIRE(std::get<unsigned>(instruction.op1()) == 0);
      }
      if (instruction.opcode() == O::print) {
        REQUIRE(std::get<I::Stream>(instruction.op2()) != I::Stream::error);
      }
    }
  }
}

namespace {
struct InjectedReadFailure
{
  unsigned marker;
};

class ThrowOnceReader final : public GD::Bc::Reader
{
public:
  ThrowOnceReader(std::string_view text, std::size_t fault_at, bool allocation_failure,
                  std::size_t& reads)
      : Reader("reader-fault"), text_(text), fault_at_(fault_at),
        allocation_failure_(allocation_failure), reads_(reads)
  {
  }

  auto peek() -> int override
  {
    ++reads_;
    if (position_ == fault_at_ && !thrown_) {
      thrown_ = true;
      if (allocation_failure_) {
        throw std::bad_alloc();
      }
      throw InjectedReadFailure{197};
    }
    return position_ == text_.size() ? EOF : static_cast<unsigned char>(text_[position_]);
  }

private:
  void do_chew() override { ++position_; }

  std::string_view text_;
  std::size_t fault_at_;
  bool allocation_failure_;
  std::size_t position_ = 0;
  bool thrown_ = false;
  std::size_t& reads_;
};
}  // namespace

TEST_CASE("Reader failures cannot resume a partially parsed item",
          "[bc][parser][errors][reader-fault]")
{
  std::string_view const item = GENERATE("1+2\n99\n", "{\n1+2\n}\n99\n", "f(1+2)\n99\n",
                                         "define f(a) {\nauto b;\nwhile(1) {\n1+2\n}\n}\n99\n");
  bool const allocation_failure = GENERATE(false, true);
  std::string const input = "7\n" + std::string(item);
  std::size_t reads = 0;
  auto reader =
    std::make_unique<ThrowOnceReader>(input, input.find("1+2") + 2, allocation_failure, reads);
  GD::Bc::Parser parser(std::make_unique<GD::Bc::Lexer>(std::move(reader)), true);
  auto original = parser.parse();
  REQUIRE(original != nullptr);
  REQUIRE(std::get<std::string>(original->at(0).op1()) == "7");
  INFO("item = " << item << ", allocation failure = " << allocation_failure);
  auto expect_failure = [&]() {
    if (allocation_failure) {
      REQUIRE_THROWS_AS(parser.parse(), std::bad_alloc);
    }
    else {
      bool caught = false;
      try {
        (void)parser.parse();
      }
      catch (InjectedReadFailure const& failure) {
        caught = true;
        REQUIRE(failure.marker == 197);
      }
      REQUIRE(caught);
    }
  };
  expect_failure();
  auto const reads_after_failure = reads;
  for (unsigned repeat = 0; repeat != 3; ++repeat) {
    expect_failure();
    REQUIRE(reads == reads_after_failure);
    REQUIRE(std::get<std::string>(original->at(0).op1()) == "7");
    REQUIRE_FALSE(parser.seen_quit());
  }
}
