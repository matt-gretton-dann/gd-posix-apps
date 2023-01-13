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
#include <cmath>
#include <ctime>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <regex>
#include <string>
#include <variant>
#include <vector>

#include "awk.hh"

extern "C" char** environ;  // NOLINT

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

using ParameterValue =
  std::variant<std::string, Integer, Floating, FileDescriptor, bool, std::nullopt_t>;
using ParameterPack = std::vector<ParameterValue>;

using ArrayElement = std::pair<ArrayName, std::string>;

/// A value - either string, signed integer, or double.
using ExecutionValue =
  std::variant<std::string, Integer, Floating, std::regex, std::nullopt_t, VariableName, Field,
               FileDescriptor, bool, ParameterPack, ArrayName, ArrayElement>;

auto operator<<(std::ostream& os, ExecutionValue const& ev) -> std::ostream&
{
  return std::visit(
    GD::Overloaded{
      [&os](std::string const& s) -> std::ostream& { return os << s; },
      [&os](GD::Awk::Integer i) -> std::ostream& { return os << i.get(); },
      [&os](GD::Awk::Floating f) -> std::ostream& { return os << f; },
      [&os](std::regex const&) -> std::ostream& { return os << "<regex>"; },
      [&os](std::nullopt_t) -> std::ostream& { return os << "<empty>"; },
      [&os](GD::Awk::VariableName const& vn) -> std::ostream& {
        return os << "var(" << vn.get() << ")";
      },
      [&os](GD::Awk::ArrayName const& an) -> std::ostream& {
        return os << "array(" << an.get() << ")";
      },
      [&os](GD::Awk::Details::ArrayElement const& ae) -> std::ostream& {
        return os << ae.first.get() << "[" << ae.second << "]";
      },
      [&os](GD::Awk::Details::Field f) -> std::ostream& { return os << "$" << f.get(); },
      [&os](GD::Awk::FileDescriptor fd) -> std::ostream& { return os << "fd(" << fd.get() << ")"; },
      [&os](GD::Awk::Details::ParameterPack const&) -> std::ostream& {
        return os << "<parameter pack>";
      },
    },
    ev);
}
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
        [&ctx](GD::Awk::ArrayName const& an) {
          return fmt::vformat_to(ctx.out(), "{0}[]", fmt::make_format_args(an.get()));
        },
        [&ctx](GD::Awk::Details::ArrayElement const& ae) {
          return fmt::vformat_to(ctx.out(), "{0}[{1}]",
                                 fmt::make_format_args(ae.first.get(), ae.second));
        },
        [&ctx](GD::Awk::Details::Field f) {
          return fmt::vformat_to(ctx.out(), "${0}", fmt::make_format_args(f.get()));
        },
        [&ctx](GD::Awk::FileDescriptor fd) {
          return fmt::vformat_to(ctx.out(), "fd({0})", fmt::make_format_args(fd.get()));
        },
        [&ctx](GD::Awk::Details::ParameterPack const&) {
          return fmt::vformat_to(ctx.out(), "parameter pack", fmt::make_format_args());
        },
      },
      value);
  }
};

namespace GD::Awk::Details {
/// Mapping of variable names to values.
using VariableMap = std::map<std::string, ExecutionValue>;

class Fields
{
public:
  Fields() noexcept = default;  // NOLINT(bugprone-exception-escape)
  Fields(Fields const&) = delete;
  Fields(Fields&&) noexcept = delete;
  auto operator=(Fields const&) -> Fields& = delete;
  auto operator=(Fields&&) noexcept -> Fields& = delete;
  ~Fields() = default;

  [[nodiscard]] auto size() const noexcept -> std::size_t { return fields_.size() - 1; }
  void resize(std::size_t size, std::string const& ofs)
  {
    if (size == 0) {
      fields_.clear();
      fields_.emplace_back();
      return;
    }

    fields_.resize(size + 1);
    recalculate_field0(ofs);
  }

  [[nodiscard]] auto field(Field f) const -> ExecutionValue
  {
    if (f.get() < 0) {
      error(Msg::cannot_get_negative_field, f.get());
    }

    auto idx{static_cast<std::size_t>(f.get())};

    if (idx < fields_.size()) {
      return fields_[idx];
    }

    return std::nullopt;
  }

  void field(Field f, std::string const& str, std::string const& fs, std::string const& ofs)
  {
    if (f.get() < 0) {
      error(Msg::cannot_set_negative_field, f.get());
    }

    auto idx{static_cast<std::size_t>(f.get())};

    if (idx == 0) {
      parse_fields(str, fs);
      return;
    }

    if (idx < fields_.size()) {
      fields_[idx] = str;
      recalculate_field0(ofs);
      return;
    }

    // Appending to the end.
    fields_.resize(idx + 1);
    fields_[idx] = str;
    recalculate_field0(ofs);
  }

private:
  void recalculate_field0(std::string const& ofs)
  {
    auto it{fields_.begin() + 1};
    fields_[0].clear();
    while (it != fields_.end()) {
      if (it != fields_.begin() + 1) {
        fields_[0] += ofs;
      }
      fields_[0] += *it;
      ++it;
    }
  }

  void parse_fields(std::string const& field, std::string const& fs)  // NOLINT
  {
    // Clear the current fields, and set $0.
    fields_.clear();
    fields_.emplace_back(field);

    if (fs.empty()) {
      error(Msg::empty_fs_undefined_behaviour);
    }

    if (fs == " ") {
      parse_fs_space();
    }
    else if (fs.size() == 1) {
      parse_fs_char(fs[0]);
    }
    else {
      parse_fs_regex(std::regex{fs, std::regex_constants::awk});
    }
  }

  void parse_fs_space()
  {
    std::size_t offset{0};
    while (true) {
      while (offset < fields_[0].size() &&
             (std::isblank(fields_[0][offset]) != 0 || fields_[0][offset] == '\n')) {
        ++offset;
      }
      if (offset == fields_[0].size()) {
        return;
      }
      std::size_t const space{fields_[0].find_first_of(" \t\n", offset)};
      if (space == std::string::npos) {
        // NOLINTNEXTLINE
        fields_.emplace_back(fields_[0].data() + offset, fields_[0].size() - offset);
        return;
      }

      // NOLINTNEXTLINE
      fields_.emplace_back(fields_[0].data() + offset, space - offset);
      offset = space + 1;
    }
  }

  void parse_fs_char(char fs)
  {
    std::size_t offset{0};

    while (offset < fields_[0].size()) {
      std::size_t const fs_appearance(fields_[0].find(fs, offset));
      if (fs_appearance == std::string::npos) {
        // NOLINTNEXTLINE
        fields_.emplace_back(fields_[0].data() + offset, fields_[0].size() - offset);
        return;
      }

      // NOLINTNEXTLINE
      fields_.emplace_back(fields_[0].data() + offset, fs_appearance - offset);
      offset = fs_appearance + 1;
    }
  }

  // NOLINTNEXTLINE
  void parse_fs_regex(std::regex const& re)
  {
    std::smatch sm;

    for (auto it{std::sregex_iterator(fields_[0].begin(), fields_[0].end(), re)};
         it != std::sregex_iterator(); ++it) {
      sm = *it;
      fields_.emplace_back(sm.prefix());
    }
    fields_.emplace_back(sm.suffix());
  }

  std::vector<std::string> fields_{};
};  // namespace GD::Awk::Details

auto cos(double x) -> double { return std::cos(x); }
auto sin(double x) -> double { return std::sin(x); }
auto exp(double x) -> double { return std::exp(x); }
auto log(double x) -> double { return std::log(x); }
auto sqrt(double x) -> double { return std::sqrt(x); }

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

auto do_int_divide(Integer::underlying_type a, Integer::underlying_type b)
  -> std::optional<Integer::underlying_type>
{
  if (b == 0) {
    return std::nullopt;
  }
  if (a % b != 0) {
    return std::nullopt;
  }
  return a / b;
}

auto do_int_modulo(Integer::underlying_type a, Integer::underlying_type b)
  -> std::optional<Integer::underlying_type>
{
  if (b == 0) {
    return std::nullopt;
  }
  return a % b;
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

  static auto read_fd(std::vector<ExecutionValue> const& values, Instruction::Operand const& index)
    -> int
  {
    assert(std::holds_alternative<Index>(index));
    ExecutionValue value{values.at(std::get<Index>(index))};
    assert(std::holds_alternative<FileDescriptor>(value));
    return std::get<FileDescriptor>(value).get();
  }

  [[nodiscard]] auto read_lvalue(std::vector<ExecutionValue> const& values,
                                 Instruction::Operand const& index) const -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(index));
    return std::visit(
      GD::Overloaded{
        [this](VariableName v) { return var(v.get()); },
        [this](Field f) { return ExecutionValue{fields_.field(f)}; },
        [this](ArrayElement const& elt) { return array_element(elt.first.get(), elt.second); },
        [](auto const&) {
          std::abort();
          return ExecutionValue{std::nullopt};
        },
      },
      values.at(std::get<Index>(index)));
  }

  void store_lvalue(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                    Instruction::Operand const& rhs, std::string const& conv_fmt)
  {
    assert(std::holds_alternative<Index>(lhs));
    assert(std::holds_alternative<Index>(rhs));
    store_lvalue(values, lhs, values.at(std::get<Index>(rhs)), conv_fmt);
  }

  void store_lvalue(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                    ExecutionValue const& value, std::string const& conv_fmt)
  {
    assert(std::holds_alternative<Index>(lhs));

    std::visit(GD::Overloaded{
                 [&value, this](VariableName const& v) {
                   // Updating NF requires us to reset the fields_ variable.
                   if (v.get() == "NF") {
                     auto size{to_integer(value)};
                     if (!size.has_value()) {
                       error(Msg::unable_to_cast_value_to_integer, value);
                     }
                     fields_.resize(static_cast<std::size_t>(*size),
                                    std::get<std::string>(var("OFS")));
                   }

                   var(v.get(), value);
                 },
                 [&value, &conv_fmt, this](Field f) {
                   fields_.field(f, *to_string(value, conv_fmt), std::get<std::string>(var("FS")),
                                 std::get<std::string>(var("OFS")));
                   var("NF", Integer{fields_.size()});
                 },
                 [&value, this](ArrayElement const& elt) {
                   return array_element(elt.first.get(), elt.second, value);
                 },
                 [](auto const&) { std::abort(); },
               },
               values.at(std::get<Index>(lhs)));
  }

  [[nodiscard]] static auto read_field(std::vector<ExecutionValue> const& values,
                                       Instruction::Operand const& index) -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(index));
    ExecutionValue const& value{values.at(std::get<Index>(index))};
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

  static auto to_integer(ParameterValue const& value) -> std::optional<Integer::underlying_type>
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

  [[nodiscard]] static auto str_to_floating(std::string const& s) -> std::optional<Floating>
  {
    std::size_t pos{0};
    try {
      Floating f{std::stod(s, &pos)};
      if (pos == s.size() && !s.empty()) {
        return f;
      }
    }
    catch (...) {
      return std::nullopt;
    }

    return std::nullopt;
  }

  [[nodiscard]] static auto str_to_floating_force(std::string const& s) -> Floating
  {
    std::size_t pos{0};
    try {
      return std::stod(s, &pos);
    }
    catch (...) {
      return 0.0;
    }
  }

  static auto to_floating_force(ExecutionValue const& value) -> Floating
  {
    return std::visit(GD::Overloaded{
                        [](Integer i) { return static_cast<Floating>(i.get()); },
                        [](Floating f) { return f; },
                        [](std::nullopt_t) { return 0.0; },
                        [](bool b) { return b ? 1.0 : 0.0; },
                        [](std::string const& s) { return str_to_floating_force(s); },
                        [](auto const&) { return 0.0; },
                      },
                      value);
  }

  static auto to_floating_force(ParameterValue const& value) -> Floating
  {
    return std::visit(GD::Overloaded{
                        [](Integer i) { return static_cast<Floating>(i.get()); },
                        [](Floating f) { return f; },
                        [](std::nullopt_t) { return 0.0; },
                        [](bool b) { return b ? 1.0 : 0.0; },
                        [](std::string const& s) { return str_to_floating_force(s); },
                        [](auto const&) { return 0.0; },
                      },
                      value);
  }

  static auto to_floating(ExecutionValue const& value) -> std::optional<Floating>
  {
    return std::visit(
      GD::Overloaded{
        [](Integer i) { return std::make_optional(static_cast<Floating>(i.get())); },
        [](Floating f) { return std::make_optional(f); },
        [](std::nullopt_t) { return std::make_optional(Floating{0.0}); },
        [](bool b) { return std::optional<Floating>(b ? 1.0 : 0.0); },
        [](std::string const& s) { return str_to_floating(s); },
        [](auto const&) { return std::optional<Floating>{std::nullopt}; },
      },
      value);
  }

  static auto to_floating(ParameterValue const& value) -> std::optional<Floating>
  {
    return std::visit(
      GD::Overloaded{
        [](Integer i) { return std::make_optional(static_cast<Floating>(i.get())); },
        [](Floating f) { return std::make_optional(f); },
        [](std::nullopt_t) { return std::make_optional(Floating{0.0}); },
        [](bool b) { return std::optional<Floating>(b ? 1.0 : 0.0); },
        [](std::string const& s) { return str_to_floating(s); },
        [](auto const&) { return std::optional<Floating>{std::nullopt}; },
      },
      value);
  }

  template<typename IntFn, typename FloatFn>
  auto execute_binary_op(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                         Instruction::Operand const& rhs, IntFn int_op, FloatFn float_op)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(lhs));
    assert(std::holds_alternative<Index>(rhs));
    ExecutionValue const& lhs_value{values.at(std::get<Index>(lhs))};
    ExecutionValue const& rhs_value{values.at(std::get<Index>(rhs))};
    auto const lhs_int{to_integer(lhs_value)};
    auto const rhs_int{to_integer(rhs_value)};
    if (lhs_int.has_value() && rhs_int.has_value()) {
      auto const res{int_op(*lhs_int, *rhs_int)};
      if (res.has_value()) {
        Integer::underlying_type const r = *res;
        return static_cast<Integer>(r);
      }
    }

    auto const lhs_float{to_floating_force(lhs_value)};
    auto const rhs_float{to_floating_force(rhs_value)};

    return Floating{float_op(lhs_float, rhs_float)};
  }

  static auto execute_atan2(std::vector<ExecutionValue> const& values,
                            Instruction::Operand const& op1, Instruction::Operand const& op2)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(op1));
    assert(std::holds_alternative<Index>(op2));
    ExecutionValue const& op1_value{values.at(std::get<Index>(op1))};
    ExecutionValue const& op2_value{values.at(std::get<Index>(op2))};

    auto const op1_float{to_floating(op1_value)};
    auto const op2_float{to_floating(op2_value)};
    if (!op1_float.has_value()) {
      error(Msg::unable_to_cast_value_to_float, op1_value);
    }
    if (!op2_float.has_value()) {
      error(Msg::unable_to_cast_value_to_float, op2_value);
    }

    return std::atan2(*op1_float, *op2_float);
  }

  static auto execute_math(std::vector<ExecutionValue> const& values,
                           Instruction::Operand const& op, std::function<double(double)> const& fn)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(op));
    ExecutionValue const& op_value{values.at(std::get<Index>(op))};

    auto const op_float{to_floating(op_value)};
    if (!op_float.has_value()) {
      error(Msg::unable_to_cast_value_to_float, op_value);
    }

    return Floating{fn(*op_float)};
  }

  static auto execute_int(std::vector<ExecutionValue> const& values, Instruction::Operand const& op)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(op));
    ExecutionValue const& value{values.at(std::get<Index>(op))};

    // See if this is already an integer.
    auto const integer{to_integer(value)};
    if (integer.has_value()) {
      return Integer{*integer};
    }

    auto const floating{to_floating(value)};
    if (!floating.has_value()) {
      error(Msg::unable_to_cast_value_to_float, value);
    }

    Floating const fresult{std::trunc(*floating)};
    auto const iresult{static_cast<Integer::underlying_type>(fresult)};
    if (static_cast<Floating>(iresult) == fresult) {
      return Integer{iresult};
    }
    return Floating{fresult};
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

  auto execute_multiply(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                        Instruction::Operand const& rhs) -> ExecutionValue
  {
    return execute_binary_op(
      values, lhs, rhs,
      [](Integer::underlying_type a, Integer::underlying_type b) { return do_int_mul(a, b); },
      [](Floating a, Floating b) { return a * b; });
  }

  auto execute_divide(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                      Instruction::Operand const& rhs) -> ExecutionValue
  {
    return execute_binary_op(
      values, lhs, rhs,
      [](Integer::underlying_type a, Integer::underlying_type b) { return do_int_divide(a, b); },
      [](Floating a, Floating b) { return a / b; });
  }

  auto execute_modulo(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                      Instruction::Operand const& rhs) -> ExecutionValue
  {
    return execute_binary_op(
      values, lhs, rhs,
      [](Integer::underlying_type a, Integer::underlying_type b) { return do_int_modulo(a, b); },
      [](Floating a, Floating b) { return fmod(a, b); });
  }

  static auto execute_to_number(std::vector<ExecutionValue> const& values,
                                Instruction::Operand const& op) -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(op));
    ExecutionValue const& value{values.at(std::get<Index>(op))};
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

  static auto execute_negate(std::vector<ExecutionValue> const& values,
                             Instruction::Operand const& op) -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(op));
    ExecutionValue const& value{values.at(std::get<Index>(op))};
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
    assert(std::holds_alternative<Index>(op));
    ExecutionValue const& value{values.at(std::get<Index>(op))};
    auto b{to_bool(value)};
    if (b.has_value()) {
      return bool{*b};
    }

    error(Msg::unable_to_cast_value_to_bool, value);
  }

  static auto execute_logical_not(std::vector<ExecutionValue> const& values,
                                  Instruction::Operand const& op) -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(op));
    ExecutionValue const& value{values.at(std::get<Index>(op))};
    auto b{to_bool(value)};
    if (b.has_value()) {
      auto result{!*b};
      return bool{result};
    }

    error(Msg::unable_to_cast_value_to_bool, value);
  }

  static auto execute_logical_and(std::vector<ExecutionValue> const& values,
                                  Instruction::Operand const& lhs, Instruction::Operand const& rhs)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(lhs));
    assert(std::holds_alternative<Index>(rhs));
    ExecutionValue const& lhs_value{values.at(std::get<Index>(lhs))};
    ExecutionValue const& rhs_value{values.at(std::get<Index>(rhs))};
    auto b1{to_bool(lhs_value)};
    auto b2{to_bool(rhs_value)};
    if (b1.has_value() && b2.has_value()) {
      auto result{*b1 && *b2};
      return bool{result};
    }

    if (!b1.has_value()) {
      error(Msg::unable_to_cast_value_to_bool, lhs_value);
    }
    if (!b2.has_value()) {
      error(Msg::unable_to_cast_value_to_bool, rhs_value);
    }

    std::abort();
  }

  static auto execute_logical_or(std::vector<ExecutionValue> const& values,
                                 Instruction::Operand const& lhs, Instruction::Operand const& rhs)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(lhs));
    assert(std::holds_alternative<Index>(rhs));
    ExecutionValue const& lhs_value{values.at(std::get<Index>(lhs))};
    ExecutionValue const& rhs_value{values.at(std::get<Index>(rhs))};
    auto b1{to_bool(lhs_value)};
    auto b2{to_bool(rhs_value)};
    if (b1.has_value() && b2.has_value()) {
      auto result{*b1 || *b2};
      return bool{result};
    }

    if (!b1.has_value()) {
      error(Msg::unable_to_cast_value_to_bool, lhs_value);
    }
    if (!b2.has_value()) {
      error(Msg::unable_to_cast_value_to_bool, rhs_value);
    }

    std::abort();
  }

  static auto to_fmt(std::string const& s)
  {
    // TODO(mgrettondann): Make this robust
    std::string result{"{:"};
    result += s.substr(1);
    result += "}";
    return result;
  }

  static auto to_string(ExecutionValue const& index, std::string const& fmt)
    -> std::optional<std::string>
  {
    return std::visit(GD::Overloaded{
                        [](Integer v) { return std::make_optional(std::to_string(v.get())); },
                        [&fmt](Floating v) {
                          return std::make_optional(
                            fmt::vformat(to_fmt(fmt), fmt::make_format_args(v)));
                        },
                        [](bool b) { return std::make_optional(std::string{b ? "1" : "0"}); },
                        [](std::string const& s) { return std::make_optional(s); },
                        [](std::nullopt_t) { return std::make_optional(std::string{}); },
                        [](auto const&) { return std::optional<std::string>{}; },
                      },
                      index);
  }

  static auto to_string(ParameterValue const& index, std::string const& fmt)
    -> std::optional<std::string>
  {
    return std::visit(GD::Overloaded{
                        [](Integer v) { return std::make_optional(std::to_string(v.get())); },
                        [&fmt](Floating v) {
                          return std::make_optional(
                            fmt::vformat(to_fmt(fmt), fmt::make_format_args(v)));
                        },
                        [](bool b) { return std::make_optional(std::string{b ? "1" : "0"}); },
                        [](std::string const& s) { return std::make_optional(s); },
                        [](std::nullopt_t) { return std::make_optional(std::string{}); },
                        [](auto const&) { return std::optional<std::string>{}; },
                      },
                      index);
  }

  static auto to_re(ExecutionValue const& index, std::string const& fmt)
    -> std::optional<std::regex>
  {
    return std::visit(
      GD::Overloaded{
        [](std::regex const& re) { return std::make_optional(re); },
        [](Integer v) { return std::make_optional(std::regex{std::to_string(v.get())}); },
        [&fmt](Floating v) {
          return std::make_optional(
            std::regex{fmt::vformat(to_fmt(fmt), fmt::make_format_args(v))});
        },
        [](bool b) { return std::make_optional(std::regex{b ? "1" : "0"}); },
        [](std::string const& s) { return std::make_optional(std::regex{s}); },
        [](std::nullopt_t) { return std::make_optional(std::regex{}); },
        [](auto const&) { return std::optional<std::regex>{}; },
      },
      index);
  }

  static auto execute_concat(std::vector<ExecutionValue> const& values,
                             Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                             std::string const& conv_fmt) -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(lhs));
    assert(std::holds_alternative<Index>(rhs));
    ExecutionValue const& lhs_value{values.at(std::get<Index>(lhs))};
    ExecutionValue const& rhs_value{values.at(std::get<Index>(rhs))};
    auto const lhs_str{to_string(lhs_value, conv_fmt)};
    auto const rhs_str{to_string(rhs_value, conv_fmt)};
    if (lhs_str.has_value() && rhs_str.has_value()) {
      return *lhs_str + *rhs_str;
    }

    if (lhs_str.has_value()) {
      error(Msg::unable_to_cast_value_to_string, rhs_value);
    }

    error(Msg::unable_to_cast_value_to_string, lhs_value);
  }

  static auto execute_match(std::vector<ExecutionValue> const& values,
                            Instruction::Operand const& op_s, Instruction::Operand const& op_re,
                            std::string const& conv_fmt)
    -> std::pair<ExecutionValue, ExecutionValue>
  {
    auto str{to_string(values.at(std::get<Index>(op_s)), conv_fmt)};
    auto re{to_re(values.at(std::get<Index>(op_re)), conv_fmt)};

    if (!re.has_value()) {
      error(Msg::unable_to_cast_value_to_re, values.at(std::get<Index>(op_re)));
    }
    if (!str.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_s)));
    }

    std::smatch sm;
    if (!regex_search(*str, sm, *re)) {
      return {Integer{0}, Integer{-1}};
    }

    Integer::underlying_type const pos{sm.position() + 1};
    Integer::underlying_type const len{sm.length()};
    return {Integer{pos}, Integer{len}};
  }

  static auto execute_re_match(std::vector<ExecutionValue> const& values,
                               Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                               std::string const& conv_fmt) -> ExecutionValue
  {
    auto [pos, len] = execute_match(values, lhs, rhs, conv_fmt);
    return bool{std::get<Integer>(pos).get() != 0};
  }

  template<typename NumFn, typename StringFn>
  auto execute_comparison_op(std::vector<ExecutionValue> const& values,
                             Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                             NumFn num_op, StringFn string_op, std::string const& conv_fmt)
    -> ExecutionValue
  {
    assert(std::holds_alternative<Index>(lhs));
    assert(std::holds_alternative<Index>(rhs));
    ExecutionValue const& lhs_value{values.at(std::get<Index>(lhs))};
    ExecutionValue const& rhs_value{values.at(std::get<Index>(rhs))};
    auto const lhs_int{to_integer(lhs_value)};
    auto const rhs_int{to_integer(rhs_value)};
    if (lhs_int.has_value() && rhs_int.has_value()) {
      return num_op(*lhs_int, *rhs_int);
    }

    auto const lhs_float{to_floating(lhs_value)};
    auto const rhs_float{to_floating(rhs_value)};
    if (lhs_float.has_value() && rhs_float.has_value()) {
      return num_op(*lhs_float, *rhs_float);
    }

    auto const lhs_str{to_string(lhs_value, conv_fmt)};
    auto const rhs_str{to_string(rhs_value, conv_fmt)};

    if (!lhs_str.has_value()) {
      error(Msg::unable_to_cast_value_to_string, lhs_value);
    }
    if (!rhs_str.has_value()) {
      error(Msg::unable_to_cast_value_to_string, rhs_value);
    }

    return string_op(*lhs_str, *rhs_str);
  }

  auto execute_is_equal(std::vector<ExecutionValue> const& values, Instruction::Operand const& lhs,
                        Instruction::Operand const& rhs, std::string const& conv_fmt)
    -> ExecutionValue
  {
    return execute_comparison_op(
      values, lhs, rhs, [](auto l, auto r) { return l == r; },
      [](std::string const& l, std::string const& r) {
        return std::strcoll(l.data(), r.data()) == 0;
      },
      conv_fmt);
  }

  auto execute_is_not_equal(std::vector<ExecutionValue> const& values,
                            Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                            std::string const& conv_fmt) -> ExecutionValue
  {
    return execute_comparison_op(
      values, lhs, rhs, [](auto l, auto r) { return l != r; },
      [](std::string const& l, std::string const& r) {
        return std::strcoll(l.data(), r.data()) != 0;
      },
      conv_fmt);
  }

  auto execute_is_less_than(std::vector<ExecutionValue> const& values,
                            Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                            std::string const& conv_fmt) -> ExecutionValue
  {
    return execute_comparison_op(
      values, lhs, rhs, [](auto l, auto r) { return l < r; },
      [](std::string const& l, std::string const& r) {
        return std::strcoll(l.data(), r.data()) < 0;
      },
      conv_fmt);
  }

  auto execute_is_less_than_equal(std::vector<ExecutionValue> const& values,
                                  Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                                  std::string const& conv_fmt) -> ExecutionValue
  {
    return execute_comparison_op(
      values, lhs, rhs, [](auto l, auto r) { return l <= r; },
      [](std::string const& l, std::string const& r) {
        return std::strcoll(l.data(), r.data()) <= 0;
      },
      conv_fmt);
  }

  auto execute_is_greater_than(std::vector<ExecutionValue> const& values,
                               Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                               std::string const& conv_fmt) -> ExecutionValue
  {
    return execute_comparison_op(
      values, lhs, rhs, [](auto l, auto r) { return l > r; },
      [](std::string const& l, std::string const& r) {
        return std::strcoll(l.data(), r.data()) > 0;
      },
      conv_fmt);
  }

  auto execute_greater_than_equal(std::vector<ExecutionValue> const& values,
                                  Instruction::Operand const& lhs, Instruction::Operand const& rhs,
                                  std::string const& conv_fmt) -> ExecutionValue
  {
    return execute_comparison_op(
      values, lhs, rhs, [](auto l, auto r) { return l >= r; },
      [](std::string const& l, std::string const& r) {
        return std::strcoll(l.data(), r.data()) >= 0;
      },
      conv_fmt);
  }

  static auto execute_branch_if_false(std::vector<ExecutionValue> const& values,
                                      Instruction::Operand const& expr,
                                      Instruction::Operand const& false_dest, Index true_dest)
    -> Index
  {
    assert(std::holds_alternative<Index>(expr));
    assert(std::holds_alternative<Index>(false_dest));
    auto value{to_bool(values.at(std::get<Index>(expr)))};
    if (!value.has_value()) {
      error(Msg::unable_to_cast_value_to_bool, values.at(std::get<Index>(expr)));
    }
    return *value ? true_dest : std::get<Index>(false_dest);
  }

  static auto execute_sprintf(std::vector<ExecutionValue> const& values,
                              Instruction::Operand const& format_op,
                              Instruction::Operand const& parameter_pack,
                              std::string const& conv_fmt) -> ExecutionValue
  {
    auto format{to_string(values.at(std::get<Index>(format_op)), conv_fmt)};
    if (!format.has_value()) {
      error(Msg::unable_to_cast_value_to_string);
    }
    auto pp{std::get<ParameterPack>(values.at(std::get<Index>(parameter_pack)))};
    auto it{pp.begin()};
    auto fmt_string{*format};  // NOLINT
    std::size_t pos{0};
    std::string result;
    while (pos != fmt_string.size()) {
      auto next_pos{fmt_string.find('%', pos)};
      if (next_pos == std::string::npos) {
        next_pos = fmt_string.size();
      }
      if (next_pos != pos) {
        // NOLINTNEXTLINE
        result.insert(result.end(), fmt_string.begin() + pos, fmt_string.begin() + next_pos);
      }
      if (next_pos == fmt_string.size()) {
        break;
      }
      pos = next_pos + 1;
      if (pos < fmt_string.size() && fmt_string[pos] == '%') {
        result += '%';
        continue;
      }
      next_pos = fmt_string.find_first_of("csdioxXufFeEaAgG", pos);
      bool is_string{false};
      bool is_floating{false};
      bool is_left_aligned{false};
      if (next_pos == std::string::npos) {
        next_pos = fmt_string.size();
      }
      else {
        auto fmtc{fmt_string[next_pos]};
        is_string = (fmtc == 's');  // NOLINT
        is_floating = (fmtc == 'f' || fmtc == 'g' || fmtc == 'e');
        ++next_pos;
      }
      std::string fmt_portion{fmt_string.substr(pos, next_pos - pos)};
      if (auto mp{fmt_portion.find('-')}; mp != std::string::npos) {
        is_left_aligned = true;
        fmt_portion.erase(mp, 1);
      }
      std::string new_fmt{"{:"};
      new_fmt += is_left_aligned ? '<' : '>';
      new_fmt += fmt_portion;
      new_fmt += '}';
      if (auto mp{new_fmt.find('-')}; mp != std::string::npos) {
        new_fmt[mp] = '<';  // NOLINT
      }

      if (is_string) {
        auto str{it == pp.end() ? std::string{} : *to_string(*it, conv_fmt)};  // NOLINT
        result += fmt::vformat(new_fmt,
                               fmt::make_format_args(str));  // NOLINT
      }
      else if (it == pp.end()) {
        result += fmt::vformat(new_fmt, fmt::make_format_args(0));
      }
      else if (auto integer{to_integer(*it)}; !is_floating) {
        if (integer.has_value()) {
          result += fmt::vformat(new_fmt, fmt::make_format_args(*integer));
        }
        else {
          result += fmt::vformat(
            new_fmt,
            fmt::make_format_args(static_cast<Integer::underlying_type>(to_floating_force(*it))));
        }
      }
      else {
        auto floating{to_floating_force(*it)};
        result += fmt::vformat(new_fmt, fmt::make_format_args(floating));
      }

      if (it != pp.end()) {
        ++it;
      }
      pos = next_pos;
    }

    return result;
  }

  auto format_value(std::vector<ExecutionValue> const& values, Instruction::Operand const& index)
    -> std::string
  {
    assert(std::holds_alternative<Index>(index));
    return std::visit(GD::Overloaded{
                        [](Integer v) { return std::to_string(v.get()); },
                        [this](Floating v) {
                          auto ofmt{std::get<std::string>(var("OFMT"))};
                          return fmt::vformat(to_fmt(ofmt), fmt::make_format_args(v));
                        },
                        [](bool b) { return b ? std::string{"1"} : std::string{"0"}; },
                        [](std::string const& s) { return s; },
                        [](std::nullopt_t) { return std::string{}; },
                        [](auto const&) {
                          std::abort();
                          return std::string{};
                        },
                      },
                      values.at(std::get<Index>(index)));
  }

  static auto execute_push_parameter_value(std::vector<ExecutionValue>& values,
                                           Instruction::Operand const& parameter_pack,
                                           Instruction::Operand const& value)
  {
    assert(std::holds_alternative<Index>(parameter_pack));
    assert(std::holds_alternative<Index>(value));
    ParameterPack& pp{std::get<ParameterPack>(values.at(std::get<Index>(parameter_pack)))};
    ExecutionValue v{values.at(std::get<Index>(value))};

    pp.push_back(std::visit(GD::Overloaded{
                              [](Integer v) { return ParameterValue{v}; },
                              [](Floating f) { return ParameterValue{f}; },
                              [](bool b) { return ParameterValue{b}; },
                              [](std::string const& s) { return ParameterValue{s}; },
                              [](std::nullopt_t) { return ParameterValue{std::nullopt}; },
                              [](FileDescriptor fd) { return ParameterValue{fd}; },
                              [](auto const&) {
                                std::abort();
                                return ParameterValue{};
                              },
                            },
                            v));
  }

  static auto execute_length(std::vector<ExecutionValue>& values, Instruction::Operand const& expr,
                             std::string const& fmt) -> ExecutionValue
  {
    ExecutionValue const v{values.at(std::get<Index>(expr))};
    std::optional<std::string> s{to_string(v, fmt)};
    if (s.has_value()) {
      return Integer{s->size()};
    }

    error(Msg::unable_to_cast_value_to_string, v);
  }

  static auto execute_array_element(std::vector<ExecutionValue>& values,
                                    Instruction::Operand const& array,  // NOLINT
                                    Instruction::Operand const& subscript, std::string const& fmt)
    -> ExecutionValue
  {
    ArrayName const an{std::get<ArrayName>(array)};
    std::optional<std::string> s{to_string(values.at(std::get<Index>(subscript)), fmt)};
    if (!s.has_value()) {
      std::abort();
    }
    return ArrayElement{an, *s};
  }

  static auto execute_srand(std::vector<ExecutionValue>& values, Instruction::Operand const& expr)
    -> Integer ::underlying_type
  {
    ExecutionValue const v{values.at(std::get<Index>(expr))};
    auto oseed{to_integer(v)};
    if (!oseed.has_value()) {
      error(Msg::unable_to_cast_value_to_integer);
    }
    auto seed{*oseed};

    // NOLINTNEXTLINE - API specifies unsigned short.
    std::array<unsigned short, 3> seeda{
      static_cast<unsigned short>(seed & 0xffff),           // NOLINT
      static_cast<unsigned short>((seed >> 16) & 0xffff),   // NOLINT
      static_cast<unsigned short>((seed >> 24) & 0xffff)};  // NOLINT

    (void)seed48(seeda.data());  // NOLINT
    return seed;
  }

  auto execute_subst(std::vector<ExecutionValue>& values, Instruction::Operand const& op_re,
                     Instruction::Operand const& op_repl, Instruction::Operand const& op_in,
                     bool global, std::string const& conv_fmt) -> ExecutionValue
  {
    auto re{to_re(values.at(std::get<Index>(op_re)), conv_fmt)};
    auto repl{to_string(values.at(std::get<Index>(op_repl)), conv_fmt)};
    auto str{to_string(read_lvalue(values, std::get<Index>(op_in)), conv_fmt)};

    if (!re.has_value()) {
      error(Msg::unable_to_cast_value_to_re, values.at(std::get<Index>(op_re)));
    }
    if (!repl.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_repl)));
    }
    if (!str.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_in)));
    }

    auto flags{std::regex_constants::format_sed};

    std::string result;
    Integer::underlying_type matches{0};
    std::smatch sm;
    for (auto it{std::sregex_iterator(str->begin(), str->end(), *re)}; it != std::sregex_iterator();
         ++it) {
      ++matches;
      sm = *it;
      result += sm.prefix();
      if (global || matches == 1) {
        result += sm.format(*repl, flags);
      }
      else {
        result += sm.str();
      }
    }
    result += sm.suffix();
    if (!result.empty()) {
      store_lvalue(values, std::get<Index>(op_in), ExecutionValue{result}, conv_fmt);
    }
    if (!global && matches > 1) {
      matches = 1;
    }
    return Integer{matches};
  }

  static auto execute_index(std::vector<ExecutionValue>& values, Instruction::Operand const& sop,
                            Instruction::Operand const& top, std::string const& conv_fmt) -> Integer
  {
    auto const& s{to_string(values.at(std::get<Index>(sop)), conv_fmt)};
    auto const& t{to_string(values.at(std::get<Index>(top)), conv_fmt)};

    if (!s.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(sop)));
    }
    if (!t.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(sop)));
    }

    auto result{s->find(*t)};
    if (result == std::string::npos) {
      result = 0;
    }
    else {
      ++result;
    }
    return Integer{result};
  }

  static auto execute_substr(std::vector<ExecutionValue>& values, Instruction::Operand const& op_s,
                             Instruction::Operand const& op_pos, Instruction::Operand const& op_len,
                             std::string const& conv_fmt) -> ExecutionValue
  {
    auto s{to_string(values.at(std::get<Index>(op_s)), conv_fmt)};
    auto pos{to_integer(values.at(std::get<Index>(op_pos)))};
    auto len{to_integer(values.at(std::get<Index>(op_len)))};

    if (!s.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_s)));
    }
    if (!pos.has_value()) {
      error(Msg::unable_to_cast_value_to_integer, values.at(std::get<Index>(op_pos)));
    }
    if (!len.has_value()) {
      error(Msg::unable_to_cast_value_to_integer, values.at(std::get<Index>(op_len)));
    }

    return s->substr(*pos - 1, *len);
  }

  static auto execute_tolower(std::vector<ExecutionValue>& values, Instruction::Operand const& op_s,
                              std::string const& conv_fmt) -> ExecutionValue
  {
    auto const& s{to_string(values.at(std::get<Index>(op_s)), conv_fmt)};

    if (!s.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_s)));
    }

    std::string result;
    std::transform(s->begin(), s->end(), std::back_inserter(result),
                   [](char c) { return static_cast<char>(std::tolower(c)); });
    return result;
  }

  static auto execute_toupper(std::vector<ExecutionValue> const& values,
                              Instruction::Operand const& op_s, std::string const& conv_fmt)
    -> ExecutionValue
  {
    auto const& s{to_string(values.at(std::get<Index>(op_s)), conv_fmt)};

    if (!s.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_s)));
    }

    std::string result;
    std::transform(s->begin(), s->end(), std::back_inserter(result),
                   [](char c) { return static_cast<char>(std::toupper(c)); });
    return result;
  }

  static auto execute_split(std::string const& str, VariableMap& array, std::string const& fs)
    -> Integer::underlying_type
  {
    array.clear();

    if (fs.empty()) {
      error(Msg::empty_fs_undefined_behaviour);
    }

    if (fs == " ") {
      execute_split_space(str, array);
    }
    else if (fs.size() == 1) {
      execute_split_char(str, array, fs[0]);
    }
    else {
      execute_split_re(str, array, std::regex{fs, std::regex_constants::awk});
    }

    return static_cast<Integer::underlying_type>(array.size());
  }

  static void execute_split_space(std::string const& str, VariableMap& array)
  {
    std::size_t offset{0};
    std::size_t index{1};
    while (true) {
      while (offset < str.size() && (std::isblank(str[offset]) != 0 || str[offset] == '\n')) {
        ++offset;
      }
      if (offset == str.size()) {
        return;
      }
      std::size_t const space{str.find_first_of(" \t\n", offset)};
      if (space == std::string::npos) {
        array.insert({std::to_string(index++), str.substr(offset)});
        return;
      }

      array.insert({std::to_string(index++), str.substr(offset, space - offset)});
      offset = space + 1;
    }
  }

  static void execute_split_char(std::string const& str, VariableMap& array, char fs)
  {
    std::size_t offset{0};
    std::size_t index{1};

    while (offset < str.size()) {
      std::size_t const fs_appearance(str.find(fs, offset));
      if (fs_appearance == std::string::npos) {
        array.insert({std::to_string(index++), str.substr(offset)});
        return;
      }

      array.insert({std::to_string(index++), str.substr(offset, fs_appearance - offset)});
      offset = fs_appearance + 1;
    }
  }

  static auto execute_split_re(std::string const& str, VariableMap& array, std::regex const& re)
    -> Integer::underlying_type
  {
    std::smatch sm;
    std::size_t index{1};

    for (auto it{std::sregex_iterator(str.begin(), str.end(), re)}; it != std::sregex_iterator();
         ++it) {
      sm = *it;
      array.insert({std::to_string(index++), sm.prefix()});
    }
    array.insert({std::to_string(index++), sm.suffix()});
    return static_cast<Integer::underlying_type>(array.size());
  }

  auto execute_split(std::vector<ExecutionValue> const& values, Instruction::Operand const& op_str,
                     Instruction::Operand const& array_op, std::string const& conv_fmt)
    -> ExecutionValue
  {
    auto const& str{to_string(values.at(std::get<Index>(op_str)), conv_fmt)};
    if (!str.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_str)));
    }

    // TODO(mgrettondann): FIX!
    auto const& array_name{std::get<VariableName>(values.at(std::get<Index>(array_op)))};

    auto [array, flag] = arrays_.insert_or_assign(array_name.get(), VariableMap{});
    auto result{execute_split(*str, array->second, std::get<std::string>(var("FS")))};
    return Integer{result};
  }

  auto execute_split(std::vector<ExecutionValue> const& values, Instruction::Operand const& op_str,
                     Instruction::Operand const& array_op, Instruction::Operand const& fs_op,
                     std::string const& conv_fmt) -> ExecutionValue
  {
    auto const& str{to_string(values.at(std::get<Index>(op_str)), conv_fmt)};
    if (!str.has_value()) {
      error(Msg::unable_to_cast_value_to_string, values.at(std::get<Index>(op_str)));
    }

    auto const& re{to_re(values.at(std::get<Index>(fs_op)), conv_fmt)};
    if (!re.has_value()) {
      error(Msg::unable_to_cast_value_to_re, values.at(std::get<Index>(fs_op)));
    }

    // TODO(mgrettondann): FIX!
    auto const& array_name{std::get<VariableName>(values.at(std::get<Index>(array_op))).get()};

    auto [array, flag] = arrays_.insert_or_assign(array_name, VariableMap{});
    auto result{execute_split_re(*str, array->second, *re)};
    return Integer{result};
  }

  auto execute([[maybe_unused]] ParsedProgram const& program, Instructions::const_iterator begin,
               Instructions::const_iterator end) -> std::optional<Integer::underlying_type>
  {
    constexpr bool debug = false;
    auto length{std::distance(begin, end)};
    assert(length >= 0);
    std::vector<ExecutionValue> values;
    Index pc{0};
    while (pc != static_cast<Index>(length)) {
      auto it{begin + static_cast<Instructions::difference_type>(pc)};
      if constexpr (debug) {
        std::cout << std::setw(10) << pc << ": " << *it;  // NOLINT
      }
      switch (it->opcode()) {
      case Instruction::Opcode::reserve_regs:
        values.resize(std::get<Index>(it->op1()), std::nullopt);
        break;
      case Instruction::Opcode::load_literal:
        values.at(it->reg()) = (interpret_literal(it->op1()));
        break;
      case Instruction::Opcode::load_lvalue:
        values.at(it->reg()) = read_lvalue(values, it->op1());
        break;
      case Instruction::Opcode::store_lvalue:
        store_lvalue(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::variable:
        values.at(it->reg()) = std::get<VariableName>(it->op1());
        break;
      case Instruction::Opcode::array:
        values.at(it->reg()) = std::get<ArrayName>(it->op1());
        break;
      case Instruction::Opcode::array_element:
        values.at(it->reg()) = execute_array_element(values, it->op1(), it->op2(),
                                                     std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::field:
        values.at(it->reg()) = read_field(values, it->op1());
        break;
      case Instruction::Opcode::open_param_pack:
        values.at(it->reg()) = ParameterPack{};
        break;
      case Instruction::Opcode::close_param_pack:
        break;
      case Instruction::Opcode::push_param:
        execute_push_parameter_value(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::sprintf: {
        auto ofmt{var("OFMT")};
        auto ofmts{std::get<std::string>(ofmt)};
        values.at(it->reg()) = execute_sprintf(values, it->op1(), it->op2(), ofmts);
        break;
      }
      case Instruction::Opcode::open:
      case Instruction::Opcode::popen:
        std::abort();
        break;
      case Instruction::Opcode::print: {
        auto stream{read_fd(values, it->op2())};
        auto buf{format_value(values, it->op1())};
        write(stream, buf.data(), buf.size());
        break;
      }
      case Instruction::Opcode::add:
        values.at(it->reg()) = execute_add(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::sub:
        values.at(it->reg()) = execute_sub(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::power:
        values.at(it->reg()) = execute_power(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::multiply:
        values.at(it->reg()) = execute_multiply(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::divide:
        values.at(it->reg()) = execute_divide(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::modulo:
        values.at(it->reg()) = execute_modulo(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::concat:
        values.at(it->reg()) =
          execute_concat(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::is_equal:
        values.at(it->reg()) =
          execute_is_equal(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::is_not_equal:
        values.at(it->reg()) =
          execute_is_not_equal(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::is_less_than:
        values.at(it->reg()) =
          execute_is_less_than(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::is_less_than_equal:
        values.at(it->reg()) = execute_is_less_than_equal(values, it->op1(), it->op2(),
                                                          std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::is_greater_than:
        values.at(it->reg()) = execute_is_greater_than(values, it->op1(), it->op2(),
                                                       std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::is_greater_than_equal:
        values.at(it->reg()) = execute_greater_than_equal(values, it->op1(), it->op2(),
                                                          std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::to_number:
        values.at(it->reg()) = execute_to_number(values, it->op1());
        break;
      case Instruction::Opcode::to_bool:
        values.at(it->reg()) = execute_to_bool(values, it->op1());
        break;
      case Instruction::Opcode::negate:
        values.at(it->reg()) = execute_negate(values, it->op1());
        break;
      case Instruction::Opcode::logical_not:
        values.at(it->reg()) = execute_logical_not(values, it->op1());
        break;
      case Instruction::Opcode::branch_if_false:
        pc = execute_branch_if_false(values, it->op1(), it->op2(), pc + 1) - 1;
        break;
      case Instruction::Opcode::re_match:
        values.at(it->reg()) =
          execute_re_match(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::logical_and:
        values.at(it->reg()) = execute_logical_and(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::logical_or:
        values.at(it->reg()) = execute_logical_or(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::branch:
        pc = std::get<Index>(it->op1()) - 1;
        break;
      case Instruction::Opcode::copy:
        values.at(it->reg()) = values.at(std::get<Index>(it->op1()));
        break;
      case Instruction::Opcode::length:
        values.at(it->reg()) =
          execute_length(values, it->op1(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::atan2:
        values.at(it->reg()) = execute_atan2(values, it->op1(), it->op2());
        break;
      case Instruction::Opcode::cos:
        values.at(it->reg()) = execute_math(values, it->op1(), cos);
        break;
      case Instruction::Opcode::sin:
        values.at(it->reg()) = execute_math(values, it->op1(), sin);
        break;
      case Instruction::Opcode::exp:
        values.at(it->reg()) = execute_math(values, it->op1(), exp);
        break;
      case Instruction::Opcode::log:
        values.at(it->reg()) = execute_math(values, it->op1(), log);
        break;
      case Instruction::Opcode::sqrt:
        values.at(it->reg()) = execute_math(values, it->op1(), sqrt);
        break;
      case Instruction::Opcode::int_:
        values.at(it->reg()) = execute_int(values, it->op1());
        break;
      case Instruction::Opcode::rand:
        values.at(it->reg()) = Floating{drand48()};  // NOLINT
        break;
      case Instruction::Opcode::srand:
        values.at(it->reg()) = Integer{rand_seed};
        rand_seed = execute_srand(values, it->op1());
        break;
      case Instruction::Opcode::current_time: {
        auto const now{static_cast<Integer::underlying_type>(std::time(nullptr))};
        values.at(it->reg()) = Integer{now};
        break;
      }
      case Instruction::Opcode::subst:
        values.at(it->reg()) = execute_subst(values, it->op1(), it->op2(), it->op3(), false,
                                             std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::gsubst:
        values.at(it->reg()) = execute_subst(values, it->op1(), it->op2(), it->op3(), true,
                                             std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::index:
        values.at(it->reg()) =
          execute_index(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::match: {
        auto [pos, len] =
          execute_match(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        values.at(it->reg()) = pos;
        var("RSTART", pos);
        var("RLENGTH", len);
        break;
      }
      case Instruction::Opcode::substr:
        values.at(it->reg()) = execute_substr(values, it->op1(), it->op2(), it->op3(),
                                              std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::tolower:
        values.at(it->reg()) =
          execute_tolower(values, it->op1(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::toupper:
        values.at(it->reg()) =
          execute_toupper(values, it->op1(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::split_fs:
        values.at(it->reg()) =
          execute_split(values, it->op1(), it->op2(), std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::split_re:
        values.at(it->reg()) = execute_split(values, it->op1(), it->op2(), it->op3(),
                                             std::get<std::string>(var("CONVFMT")));
        break;
      case Instruction::Opcode::exit: {
        auto result{to_integer(values.at(std::get<Index>(it->op1())))};
        if (!result.has_value()) {
          error(Msg::unable_to_cast_value_to_integer, values.at(std::get<Index>(it->op1())));
        }
        return *result;
      }
      }
      if constexpr (debug) {
        if (it->has_reg()) {
          std::cout << "[" << values.at(it->reg()) << "]";
        }
        std::cout << '\n';
      }
      ++pc;
    }

    return std::nullopt;
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
   * @param name   Variable to set
   * @param value Value
   *
   * If \a var does not exist then a new variable is created at the global
   * state.
   */
  void var(std::string const& name, ExecutionValue const& value)
  {
    for (auto& map : variables_stack_) {
      if (auto it{map.find(name)}; it != map.end()) {
        it->second = value;
        return;
      }
    }

    // If the variable doesn't already exist then it is made a global variable.
    variables_stack_.back().insert_or_assign(name, value);
  }

  // NOLINTNEXTLINE
  [[nodiscard]] auto array_element(std::string const& name, std::string const& idx) const
    -> ExecutionValue
  {
    if (auto array{arrays_.find(name)}; array != arrays_.end()) {
      if (auto elt{array->second.find(idx)}; elt != array->second.end()) {
        return elt->second;
      }
    }

    return ExecutionValue{std::nullopt};
  }

  // NOLINTNEXTLINE
  void array_element(std::string const& name, std::string const& idx, ExecutionValue const& value)
  {
    auto [array, success] = arrays_.insert({name, {}});
    array->second.insert_or_assign(idx, value);
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

    fields_.field(Field{0}, record, std::get<std::string>(var("FS")),
                  std::get<std::string>(var("OFS")));
    var("NF", Integer{fields_.size()});
    return true;
  }

private:
  std::list<VariableMap> variables_stack_;
  Fields fields_;
  std::map<std::string, std::map<std::string, ExecutionValue>> arrays_;
  Integer::underlying_type rand_seed{0};
};
}  // namespace GD::Awk::Details

// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
auto GD::Awk::execute(ParsedProgram const& program, std::vector<std::string> const& initial_vars,
                      std::vector<std::string> const& cmd_line) -> Integer::underlying_type
{
  Details::ExecutionState state;

  // Default values of variables
  state.var("CONVFMT", "%.6g");
  state.var("FNR", Integer{0});
  state.var("FS", " ");
  state.var("NR", Integer{0});
  state.var("OFS", " ");
  state.var("ORS", "\n");
  state.var("OFMT", "%.6g");
  state.var("RS", "\n");
  state.var("SUBSEP", ";");

  Integer::underlying_type idx{0};
  for (auto const& operand : cmd_line) {
    state.array_element("ARGV", std::to_string(idx++), Details::ExecutionValue{operand});
  }
  state.var("ARGC", Integer{idx});

  for (char** env{environ}; *env != nullptr; ++env) {  // NOLINT
    std::string const e{*env};
    auto eq{e.find('=')};
    if (eq == std::string::npos) {
      eq = e.size();
    }
    std::size_t const vstart{eq == e.size() ? e.size() : eq + 1};
    state.array_element("ENVIRON", e.substr(0, eq), e.substr(vstart));
  }

  // Set command line variables
  for (auto const& var : initial_vars) {
    if (!state.parse_var(var)) {
      error(Msg::not_valid_variable_assignment, var);
    }
  }

  // Run the BEGIN block.
  auto [begin_begin, begin_end] = program.begin_instructions();
  auto exit_code{state.execute(program, begin_begin, begin_end)};

  // Now parse and execute the command line.
  for (Integer::underlying_type i{0};
       i < std::get<Integer>(state.var("ARGC")).get() && !exit_code.has_value(); ++i) {
    Details::ExecutionValue const& operand_value{state.array_element("ARGV", std::to_string(i))};
    auto operand_stro{Details::ExecutionState::to_string(
      operand_value, std::get<std::string>(state.var("CONVFMT")))};
    if (!operand_stro.has_value()) {
      continue;
    }
    std::string const& operand = *operand_stro;

    if (state.parse_var(operand)) {
      continue;
    }

    state.var("FILENAME", operand);
    state.var("FNR", Integer{0});
    auto file{GD::StreamInputFile(operand)};
    auto [record_begin, record_end] = program.per_record_instructions();
    while (state.parse_record(file)) {
      Integer const nr{std::get<Integer>(state.var("NR"))};
      Integer::underlying_type const new_nr = nr.get() + 1;
      state.var("NR", Integer{new_nr});

      Integer const fnr{std::get<Integer>(state.var("FNR"))};
      Integer::underlying_type const new_fnr = fnr.get() + 1;
      state.var("FNR", Integer{new_fnr});

      exit_code = state.execute(program, record_begin, record_end);
    }
  }

  auto [end_begin, end_end] = program.end_instructions();
  auto end_exit_code{state.execute(program, end_begin, end_end)};

  if (end_exit_code.has_value()) {
    return *end_exit_code;
  }
  if (exit_code.has_value()) {
    return *exit_code;
  }
  return 0;
}
