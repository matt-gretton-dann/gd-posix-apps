/** \file benchmark-components.cc
 * Component measurements with explicit setup and copy/reset control costs.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <charconv>
#include <chrono>
#include <iomanip>
#include <stdexcept>
#include <string>
#include <string_view>

#include "bc.hh"

namespace {
using Number = GD::Bc::Number;
auto positive(std::string_view text, std::uint64_t maximum) -> std::uint64_t
{
  std::uint64_t result = 0;
  auto const parsed = std::from_chars(text.data(), text.data() + text.size(), result);
  if (parsed.ec != std::errc{} || parsed.ptr != text.data() + text.size() || result == 0 ||
      result > maximum) {
    throw std::runtime_error("expected a positive bounded integer");
  }
  return result;
}

auto number(std::string_view text, Number::NumType input_base = 10) -> Number
{
  bool const negative = !text.empty() && text.front() == '-';
  if (negative) {
    text.remove_prefix(1);
  }
  bool digit = false;
  bool period = false;
  auto const* message = input_base == 10 ? "expected a signed decimal number"
                                         : "expected signed digits in the requested input base";
  for (auto c : text) {
    if (c == '.' && !period) {
      period = true;
    }
    else if (input_base == 10
               ? (c >= '0' && c <= '9')
               : (std::string_view("0123456789ABCDEF").find(c) < input_base ||
                  (text.size() == 1 && std::string_view("0123456789ABCDEF").find(c) < 16))) {
      digit = true;
    }
    else {
      throw std::runtime_error(message);
    }
  }
  if (!digit) {
    throw std::runtime_error(message);
  }
  Number result(text, input_base);
  if (negative) {
    result.negate();
  }
  return result;
}

auto text(Number const& value, Number::NumType output_base = 10) -> std::string
{
  std::ostringstream output;
  value.output(output, output_base, 0);
  return output.str();
}
}  // namespace

auto main(int argc, char** argv) -> int
{
  try {
    if (argc != 4 && argc != 5) {
      throw std::runtime_error(
        "usage: bc-components COMPONENT ITERATIONS SPLIT_POINT [TEXT_BASE] < INPUT");
    }
    std::string const component(argv[1]);
    if (component != "control" && component != "add" && component != "subtract" &&
        component != "multiply" && component != "square" && component != "divide" &&
        component != "parse" && component != "conversion-in" && component != "conversion-out") {
      throw std::runtime_error("unknown component");
    }
    auto const iterations = positive(argv[2], 10000000);
    auto const split = positive(argv[3], Number::base_);
    auto const text_base = static_cast<Number::NumType>(argc == 5 ? positive(argv[4], 16) : 10);
    if (text_base < 2) {
      throw std::runtime_error("text base must be between two and sixteen");
    }
    if (argc == 5 && component != "conversion-in" && component != "conversion-out") {
      throw std::runtime_error("text base applies only to conversion components");
    }
    Number::multiply_split_point(static_cast<Number::NumType>(split));
    // Read a bounded fixture before measuring; stdin avoids native path encoding
    // differences and matches the process harness's explicit EOF protocol.
    std::string source(1024 * 1024 + 1, '\0');
    std::cin.read(source.data(), static_cast<std::streamsize>(source.size()));
    source.resize(static_cast<std::size_t>(std::cin.gcount()));
    if (std::cin.bad() || source.size() > 1024 * 1024) {
      throw std::runtime_error("unreadable or oversized component input");
    }
    Number lhs;
    Number rhs;
    std::string literal;
    if (component != "parse") {
      std::istringstream lines(source);
      std::string right;
      std::getline(lines, literal);
      std::getline(lines, right);
      if (!literal.empty() && literal.back() == '\r') {
        literal.pop_back();
      }
      if (!right.empty() && right.back() == '\r') {
        right.pop_back();
      }
      if (right.empty() && component != "control" && component != "conversion-in" &&
          component != "conversion-out") {
        throw std::runtime_error("missing right-hand component operand");
      }
      std::string extra;
      while (std::getline(lines, extra)) {
        if (extra.find_first_not_of(" \t\r") != std::string::npos) {
          throw std::runtime_error("unexpected extra component input");
        }
      }
      lhs = number(literal, component == "conversion-in" ? text_base : Number::NumType(10));
      rhs = right.empty() ? Number(1) : number(right);
      if (component == "divide" && rhs.is_zero()) {
        throw std::runtime_error("zero component divisor");
      }
    }
    Number result;
    std::uint64_t checksum = 0;
    std::uint64_t instruction_count = 0;
    std::string rendered;
#if !GD_BC_PROFILE
    auto const started = std::chrono::steady_clock::now();
#endif
    auto repeat = [iterations](auto operation) {
      for (std::uint64_t i = 0; i < iterations; ++i) {
        operation();
      }
    };
    if (component == "parse") {
      repeat([&]() {
        GD::Bc::Parser parser(
          std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), true);
        instruction_count = 0;
        while (auto code = parser.parse()) {
          instruction_count += code->size();
          for (auto const& instruction : *code) {
            if (instruction.opcode() == GD::Bc::Instruction::Opcode::quit &&
                std::get<unsigned>(instruction.op1()) != 0) {
              throw std::runtime_error("invalid parse-only fixture");
            }
          }
          if (!code->empty() && code->back().opcode() == GD::Bc::Instruction::Opcode::eof) {
            break;
          }
        }
        if (parser.seen_quit()) {
          throw std::runtime_error("quit cannot complete a parse-only fixture");
        }
        checksum += instruction_count;
      });
    }
    else if (component == "conversion-out") {
      repeat([&]() {
        rendered = text(lhs, text_base);
        checksum += rendered.size();
      });
    }
    else if (component == "conversion-in") {
      repeat([&]() {
        result = number(literal, text_base);
        checksum += result.length();
      });
    }
    else {
      // Select the operation once. Control includes the same copy/reset and
      // length consumer; no per-iteration string dispatch hides kernel work.
      auto arithmetic = [&](auto operation) {
        repeat([&]() {
          result = lhs;
          operation(result);
          checksum += result.length();
        });
      };
      if (component == "add") {
        arithmetic([&](Number& value) { value.add(rhs); });
      }
      else if (component == "subtract") {
        arithmetic([&](Number& value) { value.sub(rhs); });
      }
      else if (component == "multiply") {
        arithmetic([&](Number& value) { value.multiply(rhs, 0); });
      }
      else if (component == "square") {
        arithmetic([](Number& value) { value.multiply(value, 0); });
      }
      else if (component == "divide") {
        arithmetic([&](Number& value) { value.divide(rhs, 0); });
      }
      else {
        arithmetic([](Number&) {});
      }
    }
#if !GD_BC_PROFILE
    auto const seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - started);
#endif
    // Formatting the final arithmetic result is outside the measured interval.
    if (component != "parse" && component != "conversion-out") {
      rendered = text(result);
    }
    // Joining continuation lines is output validation work, outside timing.
    for (auto position = rendered.find("\\\n"); position != std::string::npos;
         position = rendered.find("\\\n")) {
      rendered.erase(position, 2);
    }
    std::cout << "{\"component\":\"" << component << "\",\"iterations\":" << iterations
              << ",\"checksum_mod_2_64\":" << checksum << ",\"instructions\":" << instruction_count
              << ",\"text_base\":" << text_base << ",\"split_point\":" << split
              << ",\"limb_bytes\":" << sizeof(Number::NumType) << ",\"result\":\"" << rendered
              << "\",\"instrumented\":";
#if GD_BC_PROFILE
    std::cout << "true,\"seconds\":null";
#else
    std::cout << "false,\"seconds\":" << std::setprecision(17) << seconds.count();
#endif
    std::cout << "}\n";
    return 0;
  }
  catch (std::exception const& error) {
    std::cerr << "bc-components: " << error.what() << '\n';
    return 1;
  }
}
