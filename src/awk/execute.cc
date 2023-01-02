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
                                    VariableName, Field, FileDescriptor>;

/// Mapping of variable names to values.
using VariableMap = std::map<std::string, ExecutionValue>;

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
                           Instructions::difference_type pc, Instruction::Operand const& delta)
    -> Integer::underlying_type
  {
    ExecutionValue value{values.at(pc + std::get<Instruction::Offset>(delta))};
    if (std::holds_alternative<Integer>(value)) {
      return std::get<Integer>(value).get();
    }

    std::abort();
    return Integer::underlying_type{0};
  }

  static auto read_fd(std::vector<ExecutionValue> const& values, Instructions::difference_type pc,
                      Instruction::Operand const& delta) -> int
  {
    ExecutionValue value{values.at(pc + std::get<Instruction::Offset>(delta))};
    if (std::holds_alternative<FileDescriptor>(value)) {
      return std::get<FileDescriptor>(value).get();
    }

    std::abort();
    return -1;
  }

  [[nodiscard]] auto read_lvalue(std::vector<ExecutionValue> const& values,
                                 Instructions::difference_type pc,
                                 Instruction::Operand const& delta) const -> ExecutionValue
  {
    return std::visit(GD::Overloaded{
                        [this](VariableName v) { return var(v.get()); },
                        [this](Field f) { return ExecutionValue{fields_.at(f.get())}; },
                        [](auto const&) {
                          std::abort();
                          return ExecutionValue{std::nullopt};
                        },
                      },
                      values.at(pc + std::get<Instruction::Offset>(delta)));
  }

  [[nodiscard]] static auto read_field(std::vector<ExecutionValue> const& values,
                                       Instructions::difference_type pc,
                                       Instruction::Operand const& delta) -> ExecutionValue
  {
    ExecutionValue const& value{values.at(pc + std::get<Instruction::Offset>(delta))};
    Integer const& field{std::get<Integer>(value)};
    return Field{field.get()};
  }

  auto format_value(std::vector<ExecutionValue> const& values, Instructions::difference_type pc,
                    Instruction::Operand const& delta) -> std::string
  {
    return std::visit(GD::Overloaded{
                        [](Integer v) { return std::to_string(v.get()); },
                        [this](Floating v) {
                          auto ofmt{std::get<std::string>(var("OFMT"))};
                          return fmt::vformat(ofmt, fmt::make_format_args(v));
                        },
                        [](std::string const& s) { return s; },
                        [](auto const&) {
                          std::abort();
                          return std::string{};
                        },
                      },
                      values.at(pc + std::get<Instruction::Offset>(delta)));
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
        values.at(pc) = read_lvalue(values, pc, it->op1());
        break;
      case Instruction::Opcode::variable:
        values.at(pc) = std::get<VariableName>(it->op1());
        break;
      case Instruction::Opcode::field:
        values.at(pc) = read_field(values, pc, it->op1());
        break;
      case Instruction::Opcode::printf:
      case Instruction::Opcode::open_param_pack:
      case Instruction::Opcode::close_param_pack:
      case Instruction::Opcode::push_param:
        std::abort();
        break;
      case Instruction::Opcode::print: {
        auto stream{read_fd(values, pc, it->op2())};
        auto buf{format_value(values, pc, it->op1())};
        write(stream, buf.data(), buf.size());
        break;
      }
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

    // Loop round reading a byte at a time until we get to the record separator symbol.
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
