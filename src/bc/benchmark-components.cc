/** \file benchmark-components.cc
 * Component measurements with explicit setup and copy/reset control costs.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <algorithm>
#include <array>
#include <charconv>
#include <chrono>
#include <iomanip>
#include <limits>
#include <map>
#include <sstream>
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

void validate_numeric(std::string_view text, Number::NumType input_base)
{
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
}

auto number(std::string_view text, Number::NumType input_base = 10) -> Number
{
  bool const negative = !text.empty() && text.front() == '-';
  if (negative) {
    text.remove_prefix(1);
  }
  validate_numeric(text, input_base);
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
// Diagnostic-only prototypes. Every conversion owns its power cache: constant
// generation/allocation is included, with no hidden cross-call amortization.
class ConversionPowers
{
public:
  explicit ConversionPowers(Number::NumType base) : base_(base) {}
  auto get(std::size_t digits) -> Number const&
  {
    if (digits > std::numeric_limits<Number::NumType>::max())
      throw std::length_error("Diagnostic conversion exponent exceeds native bounds");
    auto found = powers_.find(digits);
    if (found == powers_.end()) {
      Number power(base_);
      power.power(Number(static_cast<Number::NumType>(digits)), 0);
      found = powers_.emplace(digits, std::move(power)).first;
    }
    return found->second;
  }

private:
  Number::NumType base_;
  std::map<std::size_t, Number> powers_;
};

constexpr std::size_t conversion_leaf = 64;
auto parse_dc(std::string_view input, Number::NumType base, ConversionPowers& powers) -> Number
{
  if (input.size() <= conversion_leaf)
    return Number(input, base);
  auto const middle = input.size() / 2;
  auto left = parse_dc(input.substr(0, middle), base, powers);
  auto right = parse_dc(input.substr(middle), base, powers);
  left.multiply(powers.get(input.size() - middle), 0);
  left.add(right);
  return left;
}

auto input_dc(std::string_view input, Number::NumType base) -> Number
{
  // Direct decimal decoding is already linear; retain its normal entry path.
  if (base == 10 || input.size() <= conversion_leaf)
    return number(input, base);
  validate_numeric(input, base);
  ConversionPowers powers(base);
  return parse_dc(input, base, powers);
}

auto unwrapped(std::string result) -> std::string
{
  for (auto at = result.find("\\\n"); at != std::string::npos; at = result.find("\\\n"))
    result.erase(at, 2);
  return result;
}

auto output_dc_digits(Number const& value, Number::NumType base, ConversionPowers& powers)
  -> std::string
{
  if (value.length() <= conversion_leaf)
    return unwrapped(text(value, base));
  // Approximate log10(base)*1000 only chooses a partition, never result digits.
  // For the non-leaf regime each side shrinks; exact powers and arithmetic
  // determine the quotient/remainder regardless of this integer estimate.
  constexpr std::array<unsigned, 17> logs{0,   0,    301,  477,  602,  699,  778,  845, 903,
                                          954, 1000, 1041, 1079, 1113, 1146, 1176, 1204};
  auto const width = static_cast<std::size_t>(std::uint64_t(value.length()) * 500 / logs[base]);
  auto const& power = powers.get(width);
  auto high = value;
  high.divide(power, 0);
  auto low = high;
  low.multiply(power, 0);
  low.negate();
  low.add(value);
  if (high.length() >= value.length() || low.length() >= value.length())
    throw std::logic_error("Diagnostic conversion partition did not shrink");
  auto left = output_dc_digits(high, base, powers);
  auto right = output_dc_digits(low, base, powers);
  if (right.size() > width)
    throw std::logic_error("Diagnostic remainder exceeds radix power");
  left.append(width - right.size(), '0');
  left += right;
  return left;
}

auto output_dc(Number const& value, Number::NumType base) -> std::string
{
  if (base == 10 || value.length() <= conversion_leaf)
    return text(value, base);
  ConversionPowers powers(base);
  auto const digits = output_dc_digits(value, base, powers);
  std::string result;
  std::size_t position = 0;
  // Match the existing positive-integer, line-position-zero generic layout.
  if (digits.size() > 69) {
    while (digits.size() - position > 68) {
      result.append(digits, position, 68);
      result += "\\\n";
      position += 68;
    }
  }
  result.append(digits, position);
  return result;
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
    bool const conversion_input = component == "conversion-in" || component == "conversion-in-dc";
    bool const conversion_output =
      component == "conversion-out" || component == "conversion-out-dc";
    bool const divide_conquer = component == "conversion-in-dc" || component == "conversion-out-dc";
    if (component != "control" && component != "add" && component != "subtract" &&
        component != "multiply" && component != "square" && component != "divide" &&
        component != "parse" && !conversion_input && !conversion_output) {
      throw std::runtime_error("unknown component");
    }
    auto const iterations = positive(argv[2], 10000000);
    auto const split = positive(argv[3], Number::base_);
    auto const text_base = static_cast<Number::NumType>(argc == 5 ? positive(argv[4], 16) : 10);
    if (text_base < 2) {
      throw std::runtime_error("text base must be between two and sixteen");
    }
    if (argc == 5 && !conversion_input && !conversion_output) {
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
      if (right.empty() && component != "control" && !conversion_input && !conversion_output) {
        throw std::runtime_error("missing right-hand component operand");
      }
      std::string extra;
      while (std::getline(lines, extra)) {
        if (extra.find_first_not_of(" \t\r") != std::string::npos) {
          throw std::runtime_error("unexpected extra component input");
        }
      }
      if (divide_conquer && literal.find_first_of("-.") != std::string::npos)
        throw std::runtime_error("divide-conquer diagnostics require unsigned integer fixtures");
      lhs = number(literal, conversion_input ? text_base : Number::NumType(10));
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
    else if (conversion_output) {
      if (divide_conquer)
        repeat([&]() {
          rendered = output_dc(lhs, text_base);
          checksum += rendered.size();
        });
      else
        repeat([&]() {
          rendered = text(lhs, text_base);
          checksum += rendered.size();
        });
    }
    else if (conversion_input) {
      if (divide_conquer)
        repeat([&]() {
          result = input_dc(literal, text_base);
          checksum += result.length();
        });
      else
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
    if (component != "parse" && !conversion_output) {
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
