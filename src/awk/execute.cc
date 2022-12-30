/** \file   instruction.cc
 *  \brief  awk Execution engine
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/stdlib.h"

#include <map>
#include <memory>
#include <optional>
#include <ostream>
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

/// A value - either string, signed integer, or double.
using ExecutionValue = std::variant<std::string, std::int64_t, double, std::nullopt_t>;

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

  // NOLINTNEXTLINE
  void execute([[maybe_unused]] ParsedProgram const& program, Instructions::const_iterator begin,
               Instructions::const_iterator end)
  {
    auto length{std::distance(begin, end)};
    std::vector<ExecutionValue> values(length, std::nullopt);
    Instructions::difference_type pc{0};
    while (pc != length) {
      auto it{begin + pc};
      switch (it->opcode()) {
      case Instruction::Opcode::load_literal_int:
        values.emplace_back(static_cast<std::int64_t>(std::get<std::uint64_t>(it->op1())));
        break;
      case Instruction::Opcode::load_literal_float:
        values.emplace_back(std::get<double>(it->op1()));
        break;
      case Instruction::Opcode::load_literal_str:
        values.emplace_back(std::get<std::string>(it->op1()));
        break;
      case Instruction::Opcode::load_field_str:
      case Instruction::Opcode::load_rec_str:
      case Instruction::Opcode::load_variable_str:
      case Instruction::Opcode::load_variable_int:
        abort();
        break;
      case Instruction::Opcode::print_str:
        std::cout << std::get<std::string>(
          values.at(pc + std::get<Instruction::Offset>(it->op1())));
        break;
      }
      ++pc;
    }
  }

private:
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
   * If \a var does not exist then a new variable is created at the global state.
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

  std::list<VariableMap> variables_stack_;
};

}  // namespace GD::Awk::Details

// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
void GD::Awk::execute(const ParsedProgram& program, std::vector<std::string> const& initial_vars,
                      std::vector<std::string> const& cmd_line)
{
  Details::ExecutionState state;

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

    auto [record_begin, record_end] = program.per_record_instructions();
    state.execute(program, record_begin, record_end);
  }

  auto [end_begin, end_end] = program.end_instructions();
  state.execute(program, end_begin, end_end);
}
