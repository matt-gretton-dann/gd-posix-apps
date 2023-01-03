/** \file   instruction.cc
 *  \brief  awk Execution engine
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/format.hh"
#include "gd/span.hh"
#include "gd/stdlib.h"
#include "gd/unistd.h"

#include <cctype>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <regex>
#include <string>
#include <variant>
#include <vector>

#include "awk.hh"

using Msg = GD::Awk::Msg;

namespace {
/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Awk::Messages::get().format(GD::Awk::Set::awk, msg, args...) << '\n';
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
}
}  // namespace

namespace GD::Awk::Details {

using Field = TypeWrapper<Integer::underlying_type, struct FieldTag>;

/// A value - either string, signed integer, or double.
using ExecutionValue = std::variant<std::string, Integer, Floating, std::regex, std::nullopt_t,
                                    VariableName, Field, FileDescriptor, bool>;
}  // namespace GD::Awk::Details

// This needs to be in the global space
template<>
struct fmt::formatter<GD::Awk::Details::ExecutionValue>
{
  static constexpr auto parse(format_parse_context& ctx)
  {
    if (ctx.begin() != ctx.end() && *ctx.begin() != '}') {
      throw format_error("invalid format");
    }
    return ctx.begin();
  }

  template<typename FormatContext>
  auto format(GD::Awk::Details::ExecutionValue const& value, FormatContext& ctx)
  {
    return std::visit(
      GD::Overloaded{
        [&ctx](std::string const& s) {
          return fmt::vformat_to(ctx.out(), "{0}", fmt::make_format_args(s));
        },
        [&ctx](GD::Awk::Integer i) {
          return fmt::vformat_to(ctx.out(), "{0}", fmt::make_format_args(i.get()));
        },
        [&ctx](GD::Awk::Floating f) {
          return fmt::vformat_to(ctx.out(), "{0}", fmt::make_format_args(f));
        },
        [&ctx](std::regex const&) {
          return fmt::vformat_to(ctx.out(), "Regular expression", fmt::make_format_args());
        },
        [&ctx](std::nullopt_t) {
          return fmt::vformat_to(ctx.out(), "unset", fmt::make_format_args());
        },
        [&ctx](GD::Awk::VariableName const& vn) {
          return fmt::vformat_to(ctx.out(), "{0}", fmt::make_format_args(vn.get()));
        },
        [&ctx](GD::Awk::Details::Field f) {
          return fmt::vformat_to(ctx.out(), "${0}", fmt::make_format_args(f.get()));
        },
        [&ctx](GD::Awk::FileDescriptor fd) {
          return fmt::vformat_to(ctx.out(), "fd({0})", fmt::make_format_args(fd.get()));
        },
      },
      value);
  }
};

namespace GD::Awk::Details {
/// Mapping of variable names to values.
using VariableMap = std::map<std::string, ExecutionValue>;

auto do_int_add(Integer::underlying_type a, Integer::underlying_type b)
  -> std::optional<Integer::underlying_type>
{
  if (a == 0) {
    return b;
  }
  if (b == 0) {
    return a;
  }
  if (a < 0 && b < 0 && std::numeric_limits<Integer::underlying_type>::min() - a > b) {
    return std::nullopt;
  }
  if (a > 0 && b > 0 && std::numeric_limits<Integer::underlying_type>::max() - a < b) {
    return std::nullopt;
  }
  return a + b;
}

auto do_int_sub(Integer::underlying_type a, Integer::underlying_type b)
  -> std::optional<Integer::underlying_type>
{
  // Treat INT_MIN equivalent specially - so that we can then degenerate this into a + b.
  if (b == std::numeric_limits<Integer::underlying_type>::min()) {
    if (a >= 0) {
      return std::nullopt;
    }
    return a - b;
  }

  return do_int_add(a, -b);
}

auto do_int_mul(Integer::underlying_type a, Integer::underlying_type b)
  -> std::optional<Integer::underlying_type>
{
  if (a == 0 || b == 0) {
    return 0;
  }
  if (a < 0 && b < 0 && a < std::numeric_limits<Integer::underlying_type>::max() / b) {
    return std::nullopt;
  }
  if (a > 0 && b > 0 && a > std::numeric_limits<Integer::underlying_type>::max() / b) {
    return std::nullopt;
  }
  if (a > 0 && b < 0 && b < -(std::numeric_limits<Integer::underlying_type>::max() / a)) {
    return std::nullopt;
  }
  if (a < 0 && b > 0 && a < -(std::numeric_limits<Integer::underlying_type>::max() / b)) {
    return std::nullopt;
  }

  return a * b;
}

auto do_int_power(Integer::underlying_type base, Integer::underlying_type exp)
  -> std::optional<Integer::underlying_type>
{
  if (base == 0 && exp <= 0) {
    return std::nullopt;
  }
  if (exp < 0) {
    return std::nullopt;
  }

  // Hand-written power, in O(lg exp) time.
  Integer::underlying_type result{1};
  while (exp != 0) {
    if ((exp & 1) != 0) {
      auto tmp{do_int_mul(result, base)};
      if (!tmp.has_value()) {
        return std::nullopt;
      }
      result = *tmp;
    }
    exp >>= 1;
    auto tmp{do_int_mul(base, base)};
    if (!tmp.has_value()) {
      return std::nullopt;
    }
    base = *tmp;
  }

  return result;
}

/** @brief The Execution state. */
class ExecutionState
{
public:
  ExecutionState()
  {
    // We need the global variables on the variable stack.
    variables_stack_.emplace_back();
  }

  /** @brief Parse a VAR=VALUE string and do the assignment
   *
   * @param  assignment String to parse
   * @return            True iff this was valid.
   *
   * This is used for parsing command-line variable assignments.
   */
  auto parse_var(std::string const& assignment) -> bool
  {
    Lexer lexer{std::make_unique<StringReader>(assignment)};
    auto tok{lexer.peek(false)};
    if (tok != Token::Type::name) {
      return false;
    }

    std::string const var_name{tok.name()};
    lexer.chew(false);
    tok = lexer.peek(false);
    if (tok != Token::Type::assign) {
      return false;
    }
    lexer.chew(false);

    std::string const value{lexer.peek_cmdline_string().string()};
    var(var_name, ExecutionValue{value});
    return true;
  }

  static auto interpret_literal(Instruction::Operand const& op) -> ExecutionValue
  {
    return std::visit(GD::Overloaded{[](std::string const& s) { return ExecutionValue{s}; },
                                     [](Integer v) { return ExecutionValue{v}; },
                                     [](Floating v) { return ExecutionValue{v}; },
                                     [](FileDescriptor fd) { return ExecutionValue{fd}; },
                                     [](std::regex const& re) { return ExecutionValue{re}; },
                                     [](auto const&) {
                                       std::abort();
                                       return ExecutionValue{std::nullopt};
                                     }},
                      op);
  }

  static auto read_integer(std::vector<ExecutionValue> const& values,
                           Instruction::Operand const& index) -> Integer::underlying_type
  {
    assert(std::holds_alternative<Instruction::Index>(index));
    ExecutionValue value{values.at(std::get<Instruction::Index>(index))};
    assert(std::holds_alternative<Integer>(value));
    return std::get<Integer>(value).get();
  }

  static auto read_fd(std::vector<ExecutionValue> const& values, Instruction::Operand const& index)
    -> int
  {
    assert(std::holds_alternative<Instruction::Index>(index));
    ExecutionValue value{values.at(std::get<Instruction::Index>(index))};
    assert(std::holds_alternative<FileDescriptor>(value));
    return std::get<FileDescriptor>(value).get();
  }

  [[nodiscard]] auto read_lvalue(std::vector<ExecutionValue> const& values,
                                 Instruction::Operand const& index) const -> ExecutionValue
  {
    assert(std::holds_alternative<Instruction::Index>(index));
    return std::visit(GD::Overloaded{
                        [this](VariableName v) { return var(v.get()); },
                        [this](Field f) { return ExecutionValue{fields_.at(f.get())}; },
                        [](auto const&) {
                          std::abort();
                          return ExecutionValue{std::nullopt};
                        },
                      },
                      values.at(std::get<Instruction::Index>(index)));
  }

  void store_lvalue(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                    Instruction::Operand const& rhs)
  {
    assert(std::holds_alternative<Instruction::Index>(lhs));
    assert(std::holds_alternative<Instruction::Index>(rhs));
    ExecutionValue const& value{values.at(std::get<Instruction::Index>(rhs))};

    std::visit(GD::Overloaded{
                 [&value, this](VariableName v) { var(v.get(), value); },
                 [&value, this](Field f) { fields_.at(f.get()) = std::get<std::string>(value); },
                 [](auto const&) { std::abort(); },
               },
               values.at(std::get<Instruction::Index>(lhs)));
  }

  [[nodiscard]] static auto read_field(std::vector<ExecutionValue> const& values,
                                       Instruction::Operand const& index) -> ExecutionValue
  {
    assert(std::holds_alternative<Instruction::Index>(index));
    ExecutionValue const& value{values.at(std::get<Instruction::Index>(index))};
    assert(std::holds_alternative<Integer>(value));
    Integer const& field{std::get<Integer>(value)};
    return Field{field.get()};
  }

  static auto to_bool(ExecutionValue const& value) -> std::optional<bool>
  {
    return std::visit(GD::Overloaded{
                        [](Integer i) { return std::make_optional(i.get() != 0); },
                        [](Floating f) { return std::make_optional(f != 0.0); },
                        [](std::nullopt_t) { return std::make_optional(false); },
                        [](std::string const& s) { return std::make_optional(!s.empty()); },
                        [](bool b) { return std::make_optional(b); },
                        [](auto const&) { return std::optional<bool>{std::nullopt}; },
                      },
                      value);
  }

  static auto to_integer(std::string const& s) -> std::optional<Integer::underlying_type>
  {
    if (s.empty()) {
      return std::nullopt;
    }
    try {
      std::size_t pos{0};
      Integer::underlying_type num{std::stol(s, &pos)};
      return (pos == s.size()) ? std::make_optional(num)
                               : std::optional<Integer::underlying_type>(std::nullopt);
    }
    catch (...) {
      return std::nullopt;
    }
  }

  static auto to_integer(Floating f) -> std::optional<Integer::underlying_type>
  {
    auto result{static_cast<Integer::underlying_type>(f)};
    if (f == static_cast<Floating>(result)) {
      return result;
    }

    return std::nullopt;
  }

  static auto to_integer(ExecutionValue const& value) -> std::optional<Integer::underlying_type>
  {
    return std::visit(
      GD::Overloaded{
        [](Integer i) { return std::make_optional(i.get()); },
        [](Floating f) { return to_integer(f); },
        [](std::nullopt_t) { return std::make_optional(Integer::underlying_type{0}); },
        [](std::string const& s) { return to_integer(s); },
        [](bool b) { return std::optional<Integer::underlying_type>(b ? 1 : 0); },
        [](auto const&) { return std::optional<Integer::underlying_type>{std::nullopt}; },
      },
      value);
  }

  [[nodiscard]] auto str_to_floating(std::string const& s) const -> std::optional<Floating>
  {
    auto conv_fmt{std::get<std::string>(var("CONVFMT"))};
    std::size_t pos{0};
    Floating f{std::stod(s, &pos)};
    if (pos == s.size() && !s.empty()) {
      return f;
    }

    return std::nullopt;
  }

  auto to_floating(ExecutionValue const& value) -> std::optional<Floating>
  {
    return std::visit(
      GD::Overloaded{
        [](Integer i) { return std::make_optional(static_cast<Floating>(i.get())); },
        [](Floating f) { return std::make_optional(f); },
        [](std::nullopt_t) { return std::make_optional(Floating{0.0}); },
        [](bool b) { return std::optional<Floating>(b ? 1.0 : 0.0); },
        [this](std::string const& s) { return str_to_floating(s); },
        [](auto const&) { return std::optional<Floating>{std::nullopt}; },
      },
      value);
  }

  template<typename IntFn, typename FloatFn>
  auto execute_binary_op(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                         Instruction::Operand const& rhs, IntFn int_op, FloatFn float_op)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Instruction::Index>(lhs));
    assert(std::holds_alternative<Instruction::Index>(rhs));
    ExecutionValue const& lhs_value{values.at(std::get<Instruction::Index>(lhs))};
    ExecutionValue const& rhs_value{values.at(std::get<Instruction::Index>(rhs))};
    auto const lhs_int{to_integer(lhs_value)};
    auto const rhs_int{to_integer(rhs_value)};
    if (lhs_int.has_value() && rhs_int.has_value()) {
      auto const res{int_op(*lhs_int, *rhs_int)};
      if (res.has_value()) {
        Integer::underlying_type const r = *res;
        return static_cast<Integer>(r);
      }
    }

    auto const lhs_float{to_floating(lhs_value)};
    auto const rhs_float{to_floating(rhs_value)};
    if (!lhs_float.has_value()) {
      error(Msg::unable_to_cast_value_to_float, lhs_value);
    }
    if (!rhs_float.has_value()) {
      error(Msg::unable_to_cast_value_to_float, rhs_value);
    }

    return Floating{float_op(*lhs_float, *rhs_float)};
  }

  auto execute_add(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                   Instruction::Operand const& rhs) -> ExecutionValue
  {
    return execute_binary_op(
      values, lhs, rhs,
      [](Integer::underlying_type a, Integer::underlying_type b) { return do_int_add(a, b); },
      [](Floating a, Floating b) { return a + b; });
  }

  auto execute_sub(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                   Instruction::Operand const& rhs) -> ExecutionValue
  {
    return execute_binary_op(
      values, lhs, rhs,
      [](Integer::underlying_type a, Integer::underlying_type b) { return do_int_sub(a, b); },
      [](Floating a, Floating b) { return a - b; });
  }

  auto execute_power(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                     Instruction::Operand const& rhs) -> ExecutionValue
  {
    return execute_binary_op(
      values, lhs, rhs,
      [](Integer::underlying_type a, Integer::underlying_type b) { return do_int_power(a, b); },
      [](Floating a, Floating b) { return std::pow(a, b); });
  }

  auto execute_to_number(std::vector<ExecutionValue> const& values, Instruction::Operand const& op)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Instruction::Index>(op));
    ExecutionValue const& value{values.at(std::get<Instruction::Index>(op))};
    auto integer{to_integer(value)};
    if (integer.has_value()) {
      return Integer{*integer};
    }

    auto floating{to_floating(value)};
    if (floating.has_value()) {
      return Floating{*floating};
    }

    error(Msg::unable_to_cast_value_to_number, value);
  }

  auto execute_negate(std::vector<ExecutionValue> const& values, Instruction::Operand const& op)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Instruction::Index>(op));
    ExecutionValue const& value{values.at(std::get<Instruction::Index>(op))};
    auto integer{to_integer(value)};
    if (integer.has_value()) {
      auto result{-*integer};
      return Integer{result};
    }

    auto floating{to_floating(value)};
    if (floating.has_value()) {
      return Floating{-*floating};
    }

    error(Msg::unable_to_cast_value_to_number, value);
  }

  static auto execute_to_bool(std::vector<ExecutionValue> const& values,
                              Instruction::Operand const& op) -> ExecutionValue
  {
    assert(std::holds_alternative<Instruction::Index>(op));
    ExecutionValue const& value{values.at(std::get<Instruction::Index>(op))};
    auto b{to_bool(value)};
    if (b.has_value()) {
      return Integer{*b};
    }

    error(Msg::unable_to_cast_value_to_bool, value);
  }

  static auto execute_logical_not(std::vector<ExecutionValue> const& values,
                                  Instruction::Operand const& op) -> ExecutionValue
  {
    assert(std::holds_alternative<Instruction::Index>(op));
    ExecutionValue const& value{values.at(std::get<Instruction::Index>(op))};
    auto b{to_bool(value)};
    if (b.has_value()) {
      auto result{!*b};
      return Integer{result};
    }

    error(Msg::unable_to_cast_value_to_bool, value);
  }

  auto format_value(std::vector<ExecutionValue> const& values, Instruction::Operand const& index)
    -> std::string
  {
    assert(std::holds_alternative<Instruction::Index>(index));
    return std::visit(GD::Overloaded{
                        [](Integer v) { return std::to_string(v.get()); },
                        [this](Floating v) {
                          auto ofmt{std::get<std::string>(var("OFMT"))};
                          return fmt::vformat(ofmt, fmt::make_format_args(v));
                        },
                        [](bool b) { return b ? std::string{"1"} : std::string{"0"}; },
                        [](std::string const& s) { return s; },
                        [](std::nullopt_t) { return std::string{}; },
                        [](auto const&) {
                          std::abort();
                          return std::string{};
                        },
                      },
                      values.at(std::get<Instruction::Index>(index)));
  }  // namespace GD::Awk::Details

  void execute([[maybe_unused]] ParsedProgram const& program, Instructions::const_iterator begin,
               Instructions::const_iterator end)
  {
    auto length{std::distance(begin, end)};
    std::vector<ExecutionValue> values(length, std::nullopt);
    Instructions::difference_type pc{0};
    while (pc != length) {
      auto it{begin + pc};
      switch (it->opcode()) {
      case Instruction::Opcode::load_literal:
        values.at(pc) = (interpret_literal(it->op1()));
        break;
      case Instruction::Opcode::load_lvalue:
        values.at(pc) = read_lvalue(values, it->op1());
        break;
      case Instruction::Opcode::store_lvalue:
        store_lvalue(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::variable:
        values.at(pc) = std::get<VariableName>(it->op1());
        break;
      case Instruction::Opcode::field:
        values.at(pc) = read_field(values, it->op1());
        break;
      case Instruction::Opcode::printf:
      case Instruction::Opcode::open_param_pack:
      case Instruction::Opcode::close_param_pack:
      case Instruction::Opcode::push_param:
        std::abort();
        break;
      case Instruction::Opcode::print: {
        auto stream{read_fd(values, it->op2())};
        auto buf{format_value(values, it->op1())};
        write(stream, buf.data(), buf.size());
        break;
      }
      case Instruction::Opcode::add:
        values.at(pc) = execute_add(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::sub:
        values.at(pc) = execute_sub(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::power:
        values.at(pc) = execute_power(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::to_number:
        values.at(pc) = execute_to_number(values, it->op1());
        break;
      case Instruction::Opcode::to_bool:
        values.at(pc) = execute_to_bool(values, it->op1());
        break;
      case Instruction::Opcode::negate:
        values.at(pc) = execute_negate(values, it->op1());
        break;
      case Instruction::Opcode::logical_not:
        values.at(pc) = execute_logical_not(values, it->op1());
      }
      ++pc;
    }
  }

  /** @brief Read the value of the variable \a var.
   *
   * @param  var Variable to read
   * @return     Value of variable, or std::nullopt if uninitialised.
   */
  [[nodiscard]] auto var(std::string const& var) const -> ExecutionValue
  {
    for (auto const& map : variables_stack_) {
      if (auto it{map.find(var)}; it != map.end()) {
        return it->second;
      }
    }
    return std::nullopt;
  }

  /** \brief Assign \a value to \a var.
   *
   * @param var   Variable to set
   * @param value Value
   *
   * If \a var does not exist then a new variable is created at the global
   * state.
   */
  void var(std::string const& var, ExecutionValue const& value)
  {
    for (auto& map : variables_stack_) {
      if (auto it{map.find(var)}; it != map.end()) {
        it->second = value;
        return;
      }
    }

    // If the variable doesn't already exist then it is made a global variable.
    variables_stack_.back().insert_or_assign(var, value);
  }

  auto parse_record(StreamInputFile& input_file) -> bool
  {
    // Determine the record separator
    std::string rs_var(std::get<std::string>(var("RS")));
    if (rs_var.size() != 1) {
      // TODO(mgrettondann): Implement
      // Empty rs changes parsing of newlines, longer than 1 character is undefined behaviour.
      std::abort();
    }
    char const rs{rs_var[0]};

    // Loop round reading a byte at a time until we get to the record separator
    // symbol.
    std::string record;
    int c{input_file.getc()};
    while (c != EOF && c != rs) {
      record += static_cast<char>(c);
      c = input_file.getc();
    }

    // Handle end of file.
    if (c == EOF && record.empty()) {
      return false;
    }

    parse_record(record);
    return true;
  }

  void parse_record(std::string const& record)
  {
    // Clear the current fields, and set $0.
    fields_.clear();
    fields_.push_back(record);

    // Determine the field separator.
    std::string fs_var(std::get<std::string>(var("FS")));
    if (fs_var.empty()) {
      // TODO(mgrettondann): Implement: Undefined behaviour
      std::abort();
    }

    if (fs_var == " ") {
      parse_record_space(record);
    }
    else if (fs_var.size() == 1) {
      parse_record_char(record, fs_var[0]);
    }
    else {
      parse_record_regex(record, std::regex{fs_var, std::regex_constants::awk});
    }

    var("NF", static_cast<Integer>(fields_.size() - 1));
  }

private:
  void parse_record_space(std::string const& record)
  {
    auto it{fields_.insert(fields_.end(), std::string{})};

    for (auto c : record) {
      if (std::isblank(c) != 0 || c == '\n') {
        if (!it->empty()) {
          it = fields_.insert(fields_.end(), std::string{});
        }
        continue;
      }

      *it += c;
    }
  }

  void parse_record_char(std::string const& record, char fs)
  {
    auto it{fields_.insert(fields_.end(), std::string{})};

    for (auto c : record) {
      if (c == fs) {
        if (!it->empty()) {
          it = fields_.insert(fields_.end(), std::string{});
        }
        continue;
      }

      *it += c;
    }
  }

  // NOLINTNEXTLINE
  void parse_record_regex([[maybe_unused]] std::string const& record,
                          [[maybe_unused]] std::regex const& re)
  {
    // TODO(mgrettondann): Implement - handling regex Field separators
    std::abort();
  }

  std::list<VariableMap> variables_stack_;
  std::vector<std::string> fields_;
};

}  // namespace GD::Awk::Details

// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
void GD::Awk::execute(ParsedProgram const& program, std::vector<std::string> const& initial_vars,
                      std::vector<std::string> const& cmd_line)
{
  Details::ExecutionState state;

  // Default values of variables
  state.var("FS", " ");
  state.var("NR", Integer{0});
  state.var("OFS", " ");
  state.var("ORS", "\n");
  state.var("OFMT", "%.6g");
  state.var("RS", "\n");

  // Set command line variables
  for (auto const& var : initial_vars) {
    if (!state.parse_var(var)) {
      error(Msg::not_valid_variable_assignment, var);
    }
  }

  // Run the BEGIN block.
  auto [begin_begin, begin_end] = program.begin_instructions();
  state.execute(program, begin_begin, begin_end);

  // Now parse and execute the command line.
  for (auto const& operand : cmd_line) {
    if (state.parse_var(operand)) {
      continue;
    }

    state.var("FILENAME", operand);
    state.var("NR", Integer{0});
    auto file{GD::StreamInputFile(operand)};
    auto [record_begin, record_end] = program.per_record_instructions();
    while (state.parse_record(file)) {
      Integer const nr{std::get<Integer>(state.var("NR"))};
      Integer::underlying_type const new_nr = nr.get() + 1;
      state.var("NR", Integer{new_nr});
      state.execute(program, record_begin, record_end);
    }
  }

  auto [end_begin, end_end] = program.end_instructions();
  state.execute(program, end_begin, end_end);
}
