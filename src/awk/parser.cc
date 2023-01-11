/** \file   parser.cc
 *  \brief  awk Parser program
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "awk-messages.hh"

#include <iostream>
#include <memory>
#include <regex>
#include <variant>

#include "awk.hh"

using Msg = GD::Awk::Msg;
namespace {
/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, GD::Awk::Location const& loc, Ts... args)
{
  std::cerr << GD::Awk::Messages::get().format(GD::Awk::Set::awk, Msg::error_label,
                                               GD::program_name(), loc.file_name(), loc.line(),
                                               loc.column())
            << GD::Awk::Messages::get().format(GD::Awk::Set::awk, msg, args...) << '\n';
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
}
}  // namespace

namespace GD::Awk::Details {

/** Types of expression.
 *
 * There are two types of expression: print_expr, and plain expr.
 *
 * There are parsing ambiguities in print and printf which mean we do not know what type of expr we
 * are necessarily in when we start.  However, we can assume that we are in a print_expr for print
 * and printf, and just treat receiving a multiple_expr_list result as a special case.
 */
enum class ExprType {
  print_expr,  ///< Definitely a print_expr.
  expr,        ///< Definitely an expr.
};

/** Types of expression list.
 *
 * There are three types of expression list:
 *  * print_expr_list - used in print & printf statements
 *  * expr_list - A list of expressions that may have only one entry.
 *  * multiple_expr_list - A list of expressions that must have at least two entries.
 */
enum class ExprListType {
  print_expr_list,     ///< Definitely a print_expr_list
  expr_list,           ///< Definitely an expr_list
  multiple_expr_list,  ///< Definitely a multiple_expr_list
};

static auto to_expr_type(ExprListType t) -> ExprType
{
  switch (t) {
  case ExprListType::print_expr_list:
    return ExprType::print_expr;
  case ExprListType::expr_list:
  case ExprListType::multiple_expr_list:
    return ExprType::expr;
  }
}

/** Whether an expression is unary or non-unary. */
enum class UnaryType {
  unary,      ///< Unary expression
  non_unary,  ///< Non-unary expression
  both        ///< Expression could be unary or non-unary
};

struct ExprResult
{
  enum class Flags { none = 0, lvalue = 0x1, regex = 0x2 };

  struct OneResult
  {
    Index index{illegal_index};
    Flags flags{Flags::none};
  };

  using Indices = std::vector<OneResult>;

  ExprResult() = default;
  explicit ExprResult(OneResult const& r) : indices_(1, r) {}
  explicit ExprResult(Index index) : indices_(1, {index, Flags::none}) {}
  ExprResult(Index index, Flags flags) : indices_(1, {index, flags}) {}

  ~ExprResult() = default;
  ExprResult(ExprResult const&) = default;
  ExprResult(ExprResult&&) noexcept = default;
  auto operator=(ExprResult const&) -> ExprResult& = default;
  auto operator=(ExprResult&&) noexcept -> ExprResult& = default;

  [[nodiscard]] auto has_one_value() const noexcept -> bool { return indices_.size() == 1; }
  [[nodiscard]] auto has_many_values() const noexcept -> bool { return indices_.size() > 1; }
  [[nodiscard]] auto has_no_values() const noexcept -> bool { return indices_.empty(); }

  [[nodiscard]] auto flags() const noexcept -> Flags
  {
    assert(has_one_value());
    return indices_.front().flags;
  }

  [[nodiscard]] auto is_lvalue() const noexcept -> bool
  {
    return !indices_.empty() && indices_.front().flags == Flags::lvalue;
  }
  [[nodiscard]] auto is_regex() const noexcept -> bool
  {
    return !indices_.empty() && indices_.front().flags == Flags::regex;
  }

  [[nodiscard]] auto index() const noexcept -> Index
  {
    assert(has_one_value());
    return indices_.front().index;
  }

  [[nodiscard]] auto begin() const -> Indices::const_iterator { return indices_.begin(); }
  [[nodiscard]] auto end() const -> Indices::const_iterator { return indices_.end(); }

  [[nodiscard]] auto back_inserter() { return std::back_inserter(indices_); }

private:
  Indices indices_{};
};

constexpr auto is_unary_prefix_op(Token::Type type) -> bool
{
  return type == Token::Type::not_ || type == Token::Type::add || type == Token::Type::subtract;
}

constexpr auto is_additive_op(Token::Type type) -> bool
{
  return type == Token::Type::add || type == Token::Type::subtract;
}

constexpr auto is_multiplicative_op(Token::Type type) -> bool
{
  return type == Token::Type::multiply || type == Token::Type::divide ||
         type == Token::Type::modulo;
}

constexpr auto is_comparison_op(Token::Type type) -> bool
{
  return type == Token::Type::eq || type == Token::Type::ne || type == Token::Type::less_than ||
         type == Token::Type::le || type == Token::Type::greater_than || type == Token::Type::ge;
}

constexpr auto is_re_match_op(Token::Type type) -> bool
{
  return type == Token::Type::tilde || type == Token::Type::no_match;
}

constexpr auto is_assignment_op(Token::Type type) -> bool
{
  return type == Token::Type::assign || type == Token::Type::pow_assign ||
         type == Token::Type::mod_assign || type == Token::Type::mul_assign ||
         type == Token::Type::div_assign || type == Token::Type::add_assign ||
         type == Token::Type::sub_assign;
}

class InstructionEmitter
{
public:
  explicit InstructionEmitter(Instructions& instrs) : instrs_(std::addressof(instrs))
  {
    if (!instrs_->empty()) {
      auto& setup_instr{instrs_->front()};
      assert(setup_instr.opcode() == Instruction::Opcode::reserve_regs);
      reg_ = std::get<Index>(setup_instr.op1());
    }
    else {
      emit_statement(Instruction::Opcode::reserve_regs, illegal_index);
    }
  }

  InstructionEmitter(InstructionEmitter const&) = delete;
  InstructionEmitter(InstructionEmitter&&) noexcept = delete;
  auto operator=(InstructionEmitter const&) -> InstructionEmitter& = delete;
  auto operator=(InstructionEmitter&&) noexcept -> InstructionEmitter& = delete;

  ~InstructionEmitter() noexcept
  {
    // Update the reserve_regs instruction with the number of registers we used.
    auto& setup_instr{instrs_->front()};
    assert(setup_instr.opcode() == Instruction::Opcode::reserve_regs);
    setup_instr.op1(reg_);
  }

  [[nodiscard]] auto next_instruction_index() const noexcept -> Index { return instrs_->size(); }

  [[nodiscard]] auto at(Index n) const -> Instruction const& { return instrs_->at(n); }
  [[nodiscard]] auto at(Index n) -> Instruction& { return instrs_->at(n); }

  auto emit_expr(Instruction::Opcode opcode) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_);
    return ExprResult{reg_++, ExprResult::Flags::none};
  }

  auto emit_expr(Instruction::Opcode opcode, Integer integer) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_, integer);
    return ExprResult{reg_++};
  }

  auto emit_expr(Instruction::Opcode opcode, Floating floating) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_, floating);
    return ExprResult{reg_++};
  }

  auto emit_expr(Instruction::Opcode opcode, std::string const& str) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_, str);
    return ExprResult{reg_++};
  }

  auto emit_expr(Instruction::Opcode opcode, std::regex const& regex) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_, regex);
    return ExprResult{reg_++, ExprResult::Flags::regex};
  }

  auto emit_expr(Instruction::Opcode opcode, FileDescriptor fd) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_, fd);
    return ExprResult{reg_++};
  }

  auto emit_expr(Instruction::Opcode opcode, ExprResult const& expr) -> ExprResult
  {
    assert(expr.has_one_value());
    instrs_->emplace_back(opcode, reg_, dereference_lvalue_int(expr));
    return ExprResult{reg_++, opcode == Instruction::Opcode::field ? ExprResult::Flags::lvalue
                                                                   : ExprResult::Flags::none};
  }

  auto emit_expr(Instruction::Opcode opcode, VariableName const& var) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_, var);
    return ExprResult{reg_++, ExprResult::Flags::lvalue};
  }

  auto emit_expr(Instruction::Opcode opcode, ArrayName const& array) -> ExprResult
  {
    instrs_->emplace_back(opcode, reg_, array);
    return ExprResult{reg_++, ExprResult::Flags::lvalue};
  }

  auto emit_expr(Instruction::Opcode opcode, ArrayName const& array, ExprResult const& expr)
    -> ExprResult
  {
    assert(expr.has_one_value());
    instrs_->emplace_back(opcode, reg_, array, dereference_lvalue_int(expr));
    return ExprResult{reg_++, ExprResult::Flags::lvalue};
  }

  auto emit_expr(Instruction::Opcode opcode, ExprResult const& expr, Integer const& op2)
    -> ExprResult
  {
    assert(expr.has_one_value());
    instrs_->emplace_back(opcode, reg_, dereference_lvalue_int(expr), op2);
    return ExprResult{reg_++};
  }

  auto emit_expr(Instruction::Opcode opcode, ExprResult const& expr1, ExprResult const& expr2)
    -> ExprResult
  {
    // Index is allowed to be illegal_index.
    assert(expr1.has_one_value());
    assert(expr2.has_one_value());
    bool const preserve_regex{opcode == Instruction::Opcode::re_match ||
                              opcode == Instruction::Opcode::match};
    instrs_->emplace_back(opcode, reg_, dereference_lvalue_int(expr1),
                          dereference_lvalue_int(expr2, preserve_regex));
    return ExprResult{reg_++};
  }

  auto emit_expr(Instruction::Opcode opcode, ExprResult const& expr1, ExprResult const& expr2,
                 ExprResult const& expr3) -> ExprResult
  {
    // Index is allowed to be illegal_index.
    assert(expr1.has_one_value());
    assert(expr2.has_one_value());
    assert(expr3.has_one_value());
    bool const preserve_regex{opcode == Instruction::Opcode::subst ||
                              opcode == Instruction::Opcode::gsubst};
    Instruction::Operand const op3{
      (opcode == Instruction::Opcode::subst || opcode == Instruction::Opcode::gsubst)
        ? expr3.index()
        : dereference_lvalue_int(expr3)};
    instrs_->emplace_back(opcode, reg_, dereference_lvalue_int(expr1, preserve_regex),
                          dereference_lvalue_int(expr2), op3);
    return ExprResult{reg_++};
  }

  auto emit_copy(ExprResult const& dest, ExprResult const& src) -> ExprResult
  {
    assert(dest.has_one_value());
    assert(src.has_one_value());
    instrs_->emplace_back(Instruction::Opcode::copy, dest.index(), dereference_lvalue_int(src));
    return dest;
  }

  auto emit_store_lvalue(ExprResult const& lvalue, ExprResult const& expr)
  {
    assert(lvalue.has_one_value());
    assert(lvalue.is_lvalue());
    assert(expr.has_one_value());

    instrs_->emplace_back(Instruction::Opcode::store_lvalue, Instruction::Operand{lvalue.index()},
                          dereference_lvalue_int(expr));
  }

  auto emit_statement(Instruction::Opcode opcode, Index index) -> Index
  {
    // Index is allowed to be illegal_index.
    // Need to make sure Instruction's constructor treats the index as an operand, and not the
    // result locaiton.
    instrs_->emplace_back(opcode, Instruction::Operand{index});
    return instrs_->size() - 1;
  }

  auto emit_statement(Instruction::Opcode opcode, ExprResult const& expr) -> Index
  {
    assert(expr.has_one_value());
    instrs_->emplace_back(opcode, dereference_lvalue_int(expr));
    return instrs_->size() - 1;
  }

  auto emit_statement(Instruction::Opcode opcode, ExprResult const& expr1, Index index2) -> Index
  {
    assert(expr1.has_one_value());
    // index2 can be illegal_index
    instrs_->emplace_back(opcode, dereference_lvalue_int(expr1), Instruction::Operand{index2});
    return instrs_->size() - 1;
  }

  auto emit_statement(Instruction::Opcode opcode, ExprResult const& expr1, ExprResult const& expr2)
    -> Index
  {
    assert(expr1.has_one_value());
    assert(expr2.has_one_value());
    instrs_->emplace_back(opcode, dereference_lvalue_int(expr1), dereference_lvalue_int(expr2));
    return instrs_->size() - 1;
  }

  auto dereference_lvalue(ExprResult const& expr) -> ExprResult
  {
    assert(expr.is_lvalue());
    instrs_->emplace_back(Instruction::Opcode::load_lvalue, reg_, expr.index());
    return ExprResult{reg_++};
  }

private:
  auto emit_match_field_0(ExprResult const& re) -> ExprResult
  {
    assert(re.is_regex());
    // Bare regexes need to be converted into `$0 ~ re`
    auto lit_0{emit_expr(Instruction::Opcode::load_literal, Integer{0})};
    auto field{emit_expr(Instruction::Opcode::field, lit_0)};
    return emit_expr(Instruction::Opcode::re_match, field, re);
  }

  auto dereference_lvalue_int(ExprResult const& expr, bool preserve_regex = false)
    -> Instruction::Operand
  {
    if (expr.is_lvalue()) {
      return Instruction::Operand{dereference_lvalue(expr).index()};
    }
    if (expr.is_regex() && !preserve_regex) {
      return Instruction::Operand{emit_match_field_0(expr).index()};
    }
    return Instruction::Operand{expr.index()};
  }

  Instructions* instrs_{nullptr};
  Index reg_{0};
};

auto to_binary_op_opcode(Token::Type type) noexcept -> Instruction::Opcode
{
  switch (type) {
  case Token::Type::add:
  case Token::Type::add_assign:
    return Instruction::Opcode::add;
  case Token::Type::subtract:
  case Token::Type::sub_assign:
    return Instruction::Opcode::sub;
  case Token::Type::multiply:
  case Token::Type::mul_assign:
    return Instruction::Opcode::multiply;
  case Token::Type::divide:
  case Token::Type::div_assign:
    return Instruction::Opcode::divide;
  case Token::Type::power:
  case Token::Type::pow_assign:
    return Instruction::Opcode::power;
  case Token::Type::modulo:
  case Token::Type::mod_assign:
    return Instruction::Opcode::modulo;
  case Token::Type::and_:
    return Instruction::Opcode::logical_and;
  case Token::Type::or_:
    return Instruction::Opcode::logical_or;
  case Token::Type::eq:
    return Instruction::Opcode::is_equal;
  case Token::Type::ne:
    return Instruction::Opcode::is_not_equal;
  case Token::Type::less_than:
    return Instruction::Opcode::is_less_than;
  case Token::Type::le:
    return Instruction::Opcode::is_less_than_equal;
  case Token::Type::greater_than:
    return Instruction::Opcode::is_greater_than;
  case Token::Type::ge:
    return Instruction::Opcode::is_greater_than_equal;
  default:
    std::abort();
  }
}

auto to_builtin_func_opcode(Token::BuiltinFunc func) -> Instruction::Opcode
{
  switch (func) {
  case Token::BuiltinFunc::length:
    return Instruction::Opcode::length;
  case Token::BuiltinFunc::atan2:
    return Instruction::Opcode::atan2;
  case Token::BuiltinFunc::cos:
    return Instruction::Opcode::cos;
  case Token::BuiltinFunc::sin:
    return Instruction::Opcode::sin;
  case Token::BuiltinFunc::exp:
    return Instruction::Opcode::exp;
  case Token::BuiltinFunc::log:
    return Instruction::Opcode::log;
  case Token::BuiltinFunc::sqrt:
    return Instruction::Opcode::sqrt;
  case Token::BuiltinFunc::int_:
    return Instruction::Opcode::int_;
  case Token::BuiltinFunc::rand:
    return Instruction::Opcode::rand;
  case Token::BuiltinFunc::srand:
    return Instruction::Opcode::srand;
  default:
    std::abort();
  }
}

class ParseState
{
public:
  explicit ParseState(std::unique_ptr<Lexer>&& lexer) : lexer_(std::move(lexer)) {}

  /** \brief Parse optional newlines.
   *
   * newline_opt : NEWLINE newline_opt
   *             | empty
   */
  auto parse_newline_opt() -> void
  {
    while (lexer_->peek(false) == Token::Type::newline) {
      lexer_->chew(false);
    }
  }

  /** @brief Results from statement parsing */
  enum class ParseStatementResult {
    none,          ///< Nothing parsed
    unterminated,  ///< Unterminated statement parsed
    terminated     ///< Terminated statement parsed
  };

  /** @parse the expression part of length call.
   *
   */
  auto parse_builtin_length_expr(InstructionEmitter& emitter) -> ExprResult
  {
    bool expect_rparens{false};
    if (lexer_->peek(false) == Token::Type::lparens) {
      expect_rparens = true;
      lexer_->chew(false);
    }

    ExprResult expr{parse_expr_opt(emitter, ExprType::expr)};

    if (expect_rparens) {
      if (lexer_->peek(false) != Token::Type::rparens) {
        error(Msg::expected_rparens_after_length_parameter, lexer_->location(),
              lexer_->peek(false));
      }
      lexer_->chew(false);
    }

    if (expr.has_many_values()) {
      error(Msg::length_builtin_only_takes_one_parameter, lexer_->location());
    }

    if (expr.has_no_values()) {
      expr = emitter.emit_expr(Instruction::Opcode::load_literal, Integer{0});
      expr = emitter.emit_expr(Instruction::Opcode::field, expr);
    }

    return emitter.emit_expr(Instruction::Opcode::length, expr);
  }

  auto parse_bultin_func_one_parm_expr(InstructionEmitter& emitter, Token::BuiltinFunc func)
    -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_builtin_func, lexer_->location(), func,
            lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const expr{parse_expr(emitter, ExprType::expr, Msg::expected_expr_in_builtin_func)};

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_builtin_func_parameter, lexer_->location(), func,
            lexer_->peek(false));
    }
    lexer_->chew(false);

    if (expr.has_many_values()) {
      error(Msg::builtin_only_takes_one_parameter, lexer_->location(), func);
    }

    return emitter.emit_expr(to_builtin_func_opcode(func), expr);
  }

  auto parse_builtin_func_atan2_expr(InstructionEmitter& emitter) -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_builtin_atan2, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const yexpr{
      parse_expr(emitter, ExprType::expr, Msg::expected_expr_in_builtin_atan2)};

    if (lexer_->peek(false) != Token::Type::comma) {
      error(Msg::expected_comma_after_builtin_atan2_parameter, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const xexpr{
      parse_expr(emitter, ExprType::expr, Msg::expected_second_expr_in_builtin_atan2)};

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_builtin_atan2_parameters, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    if (yexpr.has_many_values()) {
      error(Msg::builtin_only_takes_one_parameter, lexer_->location());
    }

    return emitter.emit_expr(Instruction::Opcode::atan2, yexpr, xexpr);
  }

  auto parse_builtin_func_index_expr(InstructionEmitter& emitter) -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_builtin_index, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const sexpr{
      parse_expr(emitter, ExprType::expr, Msg::expected_expr_in_builtin_index)};

    if (lexer_->peek(false) != Token::Type::comma) {
      error(Msg::expected_comma_after_builtin_index_parameter, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const texpr{
      parse_expr(emitter, ExprType::expr, Msg::expected_second_expr_in_builtin_index)};

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_builtin_index_parameters, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    return emitter.emit_expr(Instruction::Opcode::index, sexpr, texpr);
  }

  auto parse_builtin_func_match_expr(InstructionEmitter& emitter) -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_builtin_match, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const sexpr{
      parse_expr(emitter, ExprType::expr, Msg::expected_expr_in_builtin_match)};

    if (lexer_->peek(false) != Token::Type::comma) {
      error(Msg::expected_comma_after_builtin_match_parameter, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const texpr{
      parse_expr(emitter, ExprType::expr, Msg::expected_second_expr_in_builtin_match)};

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_builtin_match_parameters, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    return emitter.emit_expr(Instruction::Opcode::match, sexpr, texpr);
  }

  auto parse_builtin_func_rand_expr(InstructionEmitter& emitter) -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_builtin_rand, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_builtin_rand, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    return emitter.emit_expr(Instruction::Opcode::rand);
  }

  auto parse_builtin_func_srand_expr(InstructionEmitter& emitter) -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_builtin_srand, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult expr{parse_expr_opt(emitter, ExprType::expr)};
    if (expr.has_no_values()) {
      expr = emitter.emit_expr(Instruction::Opcode::current_time);
    }

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_builtin_srand, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    if (expr.has_many_values()) {
      error(Msg::builtin_srand_only_takes_one_parameter, lexer_->location());
    }

    return emitter.emit_expr(Instruction::Opcode::srand, expr);
  }

  auto parse_builtin_func_subst_expr(InstructionEmitter& emitter, bool global) -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(global ? Msg::expected_lparens_after_builtin_gsub
                   : Msg::expected_lparens_after_builtin_sub,
            lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const re{
      parse_expr(emitter, ExprType::expr,
                 global ? Msg::expected_expr_in_builtin_gsub : Msg::expected_expr_in_builtin_sub)};

    if (lexer_->peek(false) != Token::Type::comma) {
      error(global ? Msg::expected_comma_after_builtin_gsub_parameter
                   : Msg::expected_comma_after_builtin_sub_parameter,
            lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const repl{parse_expr(emitter, ExprType::expr,
                                     global ? Msg::expected_second_expr_in_builtin_gsub
                                            : Msg::expected_second_expr_in_builtin_sub)};

    ExprResult in;
    if (lexer_->peek(false) == Token::Type::comma) {
      lexer_->chew(false);
      in = parse_expr(emitter, ExprType::expr,
                      global ? Msg::expected_third_expr_in_builtin_gsub
                             : Msg::expected_third_expr_in_builtin_sub);
    }
    else {
      in = emitter.emit_expr(Instruction::Opcode::load_literal, Integer{0});
      in = emitter.emit_expr(Instruction::Opcode::field, in);
    }

    if (!in.is_lvalue()) {
      error(global ? Msg::in_parameter_of_gsub_must_be_lvalue
                   : Msg::in_parameter_of_sub_must_be_lvalue,
            lexer_->location());
    }

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(global ? Msg::expected_rparens_after_builtin_gsub
                   : Msg::expected_rparens_after_builtin_sub,
            lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    return emitter.emit_expr(global ? Instruction::Opcode::gsubst : Instruction::Opcode::subst, re,
                             repl, in);
  }

  auto parse_builtin_func_substr_expr(InstructionEmitter& emitter) -> ExprResult
  {
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_builtin_substr, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const str{parse_expr(emitter, ExprType::expr, Msg::expected_expr_in_builtin_substr)};

    if (lexer_->peek(false) != Token::Type::comma) {
      error(Msg::expected_comma_after_builtin_substr_parameter, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const pos{
      parse_expr(emitter, ExprType::expr, Msg::expected_second_expr_in_builtin_substr)};

    ExprResult len;
    if (lexer_->peek(false) == Token::Type::comma) {
      lexer_->chew(false);
      len = parse_expr(emitter, ExprType::expr, Msg::expected_third_expr_in_builtin_substr);
    }
    else {
      auto max{std::numeric_limits<Integer ::underlying_type>::max()};
      len = emitter.emit_expr(Instruction::Opcode::load_literal, Integer{max});
    }

    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_builtin_substr, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    return emitter.emit_expr(Instruction::Opcode::substr, str, pos, len);
  }

  /** @brief Parse builtin functions.
   *
   * @param  instrs     Where to emit code to
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * builtin_func_expr : ATAN2 LPARENS expr COMMA expr RPARENS
   *                   | COS LPARENS expr RPARENS
   *                   | SIN LPARENS expr RPARENS
   *                   | EXP LPARENS expr RPARENS
   *                   | LOG LPARENS expr RPARENS
   *                   | SQRT LPARENS expr RPARENS
   *                   | INT LPARENS expr RPARENS
   *                   | RAND LPARENS expr RPARENS
   *                   | SRAND LPARENS expr_opt RPARENS
   *                   | GSUB LPARENS expr COMMA expr RPARENS
   *                   | GSUB LPARENS expr COMMA expr COMMA expr RPARENS
   *                   | INDEX LPARENS expr COMMA expr RPARENS
   *                   | LENGTH expr_opt
   *                   | SPLIT LPARENS expr COMMA expr RPARENS
   *                   | MATCH LPARENS expr COMMA expr COMMA expr RPARENS
   *                   | MATCH LPARENS expr COMMA expr RPARENS
   *                   | SPRINTF LPARENS expr_list RPARENS
   *                   | SUB LPARENS expr COMMA expr RPARENS
   *                   | SUB LPARENS expr COMMA expr COMMA expr RPARENS
   *                   | SUBSTR LPARENS expr COMMA expr RPARENS
   *                   | SUBSTR LPARENS expr COMMA expr COMMA expr RPARENS
   *                   | TOLOWER LPARENS expr RPARENS
   *                   | TOUPPER LPARENS expr RPARENS
   *                   | CLOSE LPARENS expr RPARENS
   *                   | SYSTEM LPARENS expr RPARENS
   *
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | atan2 ( expr , expr )               | Both                 | Non-unary            |
   * | cos ( expr )                        | Both                 | Non-unary            |
   * | sin ( expr )                        | Both                 | Non-unary            |
   * | exp ( expr )                        | Both                 | Non-unary            |
   * | log ( expr )                        | Both                 | Non-unary            |
   * | sqrt ( expr )                       | Both                 | Non-unary            |
   * | int ( expr )                        | Both                 | Non-unary            |
   * | rand (  )                           | Both                 | Non-unary            |
   * | srand ( expr_opt )                  | Both                 | Non-unary            |
   * | gsub ( expr , expr )                | Both                 | Non-unary            |
   * | gsub ( expr , expr , expr )         | Both                 | Non-unary            |
   * | index ( expr , expr )               | Both                 | Non-unary            |
   * | length expr_opt                     | Both                 | Non-unary            |
   * | match ( expr, expr )                | Both                 | Non-unary            |
   * | split ( expr , expr )               | Both                 | Non-unary            |
   * | split ( expr , expr , expr )        | Both                 | Non-unary            |
   * | sprintf ( expr_list )               | Both                 | Non-unary            |
   * | sub ( expr , expr )                 | Both                 | Non-unary            |
   * | sub ( expr , expr , expr )          | Both                 | Non-unary            |
   * | substr ( expr , expr )              | Both                 | Non-unary            |
   * | substr ( expr , expr , expr )       | Both                 | Non-unary            |
   * | tolower ( expr )                    | Both                 | Non-unary            |
   * | toupper ( expr )                    | Both                 | Non-unary            |
   * | close ( expr )                      | Both                 | Non-unary            |
   * | system ( expr )                     | Both                 | Non-unary            |
   */
  auto parse_builtin_func_expr(InstructionEmitter& emitter) -> ExprResult
  {
    assert(lexer_->peek(false) == Token::Type::builtin_func_name);

    auto func{lexer_->peek(false).builtin_func_name()};
    lexer_->chew(false);
    switch (func) {
    case Token::BuiltinFunc::length:
      return parse_builtin_length_expr(emitter);
    case Token::BuiltinFunc::atan2:
      return parse_builtin_func_atan2_expr(emitter);
      break;
    case Token::BuiltinFunc::cos:
    case Token::BuiltinFunc::sin:
    case Token::BuiltinFunc::exp:
    case Token::BuiltinFunc::log:
    case Token::BuiltinFunc::sqrt:
    case Token::BuiltinFunc::int_:
      return parse_bultin_func_one_parm_expr(emitter, func);
      break;
    case Token::BuiltinFunc::rand:
      return parse_builtin_func_rand_expr(emitter);
      break;
    case Token::BuiltinFunc::srand:
      return parse_builtin_func_srand_expr(emitter);
      break;
    case Token::BuiltinFunc::gsub:
      return parse_builtin_func_subst_expr(emitter, true);
      break;
    case Token::BuiltinFunc::sub:
      return parse_builtin_func_subst_expr(emitter, false);
      break;
    case Token::BuiltinFunc::index:
      return parse_builtin_func_index_expr(emitter);
      break;
    case Token::BuiltinFunc::match:
      return parse_builtin_func_match_expr(emitter);
      break;
    case Token::BuiltinFunc::substr:
      return parse_builtin_func_substr_expr(emitter);
      break;
    case Token::BuiltinFunc::split:
    case Token::BuiltinFunc::sprintf:
    case Token::BuiltinFunc::tolower:
    case Token::BuiltinFunc::toupper:
    case Token::BuiltinFunc::close:
    case Token::BuiltinFunc::system:
      std::abort();
    }
  }

  /** @brief Parse primary expressions, () and lvalues.
   *
   * @param  instrs     Where to emit code to
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * primary_expr : STRING
   *              | NUMBER
   *              | ERE
   *              | LPARENS expr RPARENS
   *              | NAME // LVALUE
   *              | NAME LSQUARE expr_list RSQUARE // rvalue
   *              | builtin_func
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | string                              | Both                 | Non-unary            |
   * | number                              | Both                 | Non-unary            |
   * | ere                                 | Both                 | Non-unary            |
   * | ( expr )                            | Both                 | Non-unary            |
   * | name                                | Both                 | Non-unary            |
   * | name [ expr_list ]                  | Both                 | Non-unary            |
   */
  auto parse_primary_expr_opt(InstructionEmitter& emitter, UnaryType unary_type) -> ExprResult
  {
    if (unary_type == UnaryType::unary) {
      return ExprResult{};
    }

    auto const& tok{lexer_->peek(false)};
    ExprResult result;
    switch (tok.type()) {
    case Token::Type::integer:
      result = emitter.emit_expr(Instruction::Opcode::load_literal, tok.integer());
      lexer_->chew(false);
      break;
    case Token::Type::floating:
      result = emitter.emit_expr(Instruction::Opcode::load_literal, tok.floating());
      lexer_->chew(false);
      break;
    case Token::Type::string:
      result = emitter.emit_expr(Instruction::Opcode::load_literal, tok.string());
      lexer_->chew(false);
      break;
    case Token::Type::ere:
      result = emitter.emit_expr(Instruction::Opcode::load_literal,
                                 std::regex{tok.ere(), std::regex_constants::awk});
      lexer_->chew(false);
      break;
    case Token::Type::lparens: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(emitter, ExprType::expr)};
      auto const& close_tok{lexer_->peek(false)};
      result = expr;

      if (close_tok == Token::Type::comma) {
        // We assume that we are in a multiple_expr_list here, hand over sorting the result to
        // parse_multiple_expr_list_rest here.
        lexer_->chew(false);
        parse_multiple_expr_list_rest(emitter, result.back_inserter());
      }

      if (close_tok == Token::Type::rparens) {
        lexer_->chew(false);
        break;
      }

      error(Msg::expected_rparens_at_end_of_expression, lexer_->location(), lexer_->peek(false));
    }
    case Token::Type::name: {
      std::string const var_name{tok.name()};
      lexer_->chew(false);

      auto const& tok2{lexer_->peek(true)};
      if (tok2 != Token::Type::lsquare) {
        result = emitter.emit_expr(Instruction::Opcode::variable, VariableName{var_name});
        break;
      }

      lexer_->chew(true);

      ExprResult subscripts;
      parse_expr_list_opt(emitter, subscripts.back_inserter(), ExprListType::expr_list);
      if (subscripts.has_no_values()) {
        error(Msg::expected_exprs_in_array_element_access, lexer_->location(), lexer_->peek(false));
      }

      if (lexer_->peek(false) != Token::Type::rsquare) {
        error(Msg::expected_rsquare_after_array_subscripts, lexer_->location(),
              lexer_->peek(false));
      }
      lexer_->chew(false);

      ExprResult subsep{};
      if (subscripts.has_many_values()) {
        subsep = emitter.emit_expr(Instruction::Opcode::variable, VariableName{"SUBSEP"});
      }
      ExprResult join{};
      for (auto const& subscript : subscripts) {
        if (join.has_no_values()) {
          join = ExprResult{subscript};
        }
        else {
          join = emitter.emit_expr(Instruction::Opcode::concat, join, subsep);
          join = emitter.emit_expr(Instruction::Opcode::concat, join, ExprResult{subscript});
        }
      }

      result = emitter.emit_expr(Instruction::Opcode::array_element, ArrayName{var_name}, join);
      break;
    }
    case Token::Type::builtin_func_name:
      return parse_builtin_func_expr(emitter);
    default:
      break;
    }

    return result;
  }

  /** @brief Parse a field expression
   *
   * @param  instrs     Where to emit code to
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * field_opt : DOLLAR expr
   *           | primary_expr
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | $ primary_expr                      | Both                 | Non-unary            |
   */
  auto parse_field_expr_opt(InstructionEmitter& emitter, UnaryType unary_type) -> ExprResult
  {
    auto const& tok{lexer_->peek(false)};
    if (unary_type == UnaryType::unary || tok != Token::Type::dollar) {
      return parse_primary_expr_opt(emitter, unary_type);
    }

    lexer_->chew(false);
    ExprResult const field_id{parse_primary_expr_opt(emitter, unary_type)};
    if (!field_id.has_one_value()) {
      error(Msg::expected_expr_after_dollar, lexer_->location(), lexer_->peek(false));
    }

    assert(field_id.has_one_value());
    return emitter.emit_expr(Instruction::Opcode::field, field_id);
  }

  /** @brief Parse a post- increment/decrement expression
   *
   * @param  instrs     Where to emit code to
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * post_incr_decr_expr : lvalue INCR
   *                     | lvalue DECR
   *                     | field_expr
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | lvalue ++                           | Both                 | Non-unary            |
   * | lvalue --                           | Both                 | Non-unary            |
   */
  auto parse_post_incr_decr_expr_opt(InstructionEmitter& emitter, UnaryType unary_type)
    -> ExprResult
  {
    // Parse the primary expression
    ExprResult lvalue{parse_field_expr_opt(emitter, unary_type)};

    if (!lvalue.is_lvalue() || unary_type == UnaryType::unary || !lvalue.has_one_value()) {
      return lvalue;
    }

    auto type{lexer_->peek(true).type()};
    if (type != Token::Type::incr && type != Token::Type::decr) {
      return lvalue;
    }

    bool const is_incr{type == Token::Type::incr};
    lexer_->chew(true);
    // Code sequence:
    //  x: load_lvalue result.index
    //  x + 1: load_lt 1
    //  x + 2: add x, x + 1 (or sub)
    //  x + 3: store_lvalue result.index
    //  result.index = x
    auto result{emitter.dereference_lvalue(lvalue)};
    ExprResult const lit1_index{emitter.emit_expr(Instruction::Opcode::load_literal, Integer{1})};
    auto mod_index{emitter.emit_expr(is_incr ? Instruction::Opcode::add : Instruction::Opcode::sub,
                                     result, lit1_index)};
    emitter.emit_store_lvalue(lvalue, mod_index);

    return result;
  }

  /** @brief Parse a pre- increment/decrement expression
   *
   * @param  instrs     Where to emit code to
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * post_incr_decr_expr : INCR lvalue
   *                     | DECR lvalue
   *                     | primary_expr
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | ++ lvalue                           | Both                 | Non-unary            |
   * | -- rvalue                           | Both                 | Non-unary            |
   */
  auto parse_pre_incr_decr_expr_opt(InstructionEmitter& emitter, UnaryType unary_type) -> ExprResult
  {
    auto token{lexer_->peek(false)};
    if (unary_type == UnaryType::unary ||
        (token != Token::Type::incr && token != Token::Type::decr)) {
      return parse_post_incr_decr_expr_opt(emitter, unary_type);
    }

    bool const is_incr{token == Token::Type::incr};
    lexer_->chew(false);

    // Parse the primary expression
    ExprResult const lvalue{parse_post_incr_decr_expr_opt(emitter, unary_type)};

    if (!lvalue.is_lvalue() || !lvalue.has_one_value()) {
      error(Msg::expected_lvalue_after_pre_incr_decr, lexer_->location(), token,
            lexer_->peek(false));
    }

    // Code sequence:
    //  x: load_lvalue result.index // Delegated to add instruction
    //  x + 1: load_lt 1
    //  x + 2: add x, x + 1 (or sub)
    //  x + 3: store_lvalue result.index
    //  result.index = x + 2
    ExprResult const lit1{emitter.emit_expr(Instruction::Opcode::load_literal, Integer{1})};
    auto mod{emitter.emit_expr(is_incr ? Instruction::Opcode::add : Instruction::Opcode::sub,
                               lvalue, lit1)};
    emitter.emit_store_lvalue(lvalue, mod);
    return mod;
  }

  /** @brief Parse a power expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * power_expr : post_incr_decr_expr ^ power_expr
   *            | post_incr_decr_expr
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | post_incr_decr_expr ^ power_expr    | Both                 | Both                 |
   */
  auto parse_power_expr_opt(InstructionEmitter& emitter, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_pre_incr_decr_expr_opt(emitter, unary_type)};

    if (!lhs.has_one_value() || lexer_->peek(true) != Token::Type::power) {
      return lhs;
    }
    lexer_->chew(true);

    ExprResult const rhs{parse_power_expr_opt(emitter, expr_type, unary_type)};
    if (!rhs.has_one_value()) {
      error(Msg::expected_expr_after_power, lexer_->location(), lexer_->peek(false));
    }

    return emitter.emit_expr(Instruction::Opcode::power, lhs, rhs);
  }

  /** @brief Parse unary prefix expressions (!, +, and -)
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * Not quite the right name as ! expr is actually a non-unary expr!
   *
   * unary_prefix_expr : NOT unary_prefix_expr
   *                   | PLUS unary_prefix_expr
   *                   | SUB unary_prefix_expr
   *                   | power_expr
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | ! unary_prefix_expr                 | Both                 | Non-unary            |
   * | + unary_prefix_expr                 | Both                 | Unary                |
   * | - unary_prefix_expr                 | Both                 | Unary                |
   */
  auto parse_unary_prefix_expr_opt(InstructionEmitter& emitter, ExprType expr_type,
                                   UnaryType unary_type) -> ExprResult
  {
    bool is_not{false};
    bool do_not{false};
    bool is_sign_prefix{false};
    bool negate{false};

    while (is_unary_prefix_op(lexer_->peek(false).type())) {
      if (lexer_->peek(false) == Token::Type::not_ && unary_type != UnaryType::unary) {
        unary_type = UnaryType::non_unary;
        is_not = true;
        do_not = !do_not;
      }
      else if (lexer_->peek(false) == Token::Type::add && unary_type != UnaryType::non_unary) {
        unary_type = UnaryType::unary;
        is_sign_prefix = true;
      }
      else if (lexer_->peek(false) == Token::Type::subtract && unary_type != UnaryType::non_unary) {
        unary_type = UnaryType::unary;
        is_sign_prefix = true;
        negate = !negate;
      }

      lexer_->chew(false);
    }

    if (!is_not && !is_sign_prefix) {
      return parse_power_expr_opt(emitter, expr_type, unary_type);
    }

    auto expr{parse_unary_prefix_expr_opt(emitter, expr_type, UnaryType::both)};
    if (!is_not && !is_sign_prefix) {
      return expr;
    }

    if (!expr.has_one_value()) {
      error(Msg::expected_expr_after_unary_prefix, lexer_->location(),
            is_not ? "!" : (negate ? "-" : "+"), lexer_->peek(false));  // NOLINT
    }

    if (is_not) {
      if (do_not) {
        return emitter.emit_expr(Instruction::Opcode::logical_not, expr);
      }

      return emitter.emit_expr(Instruction::Opcode::to_bool, expr);
    }

    assert(is_sign_prefix);
    if (negate) {
      return emitter.emit_expr(Instruction::Opcode::negate, expr);
    }

    return emitter.emit_expr(Instruction::Opcode::to_number, expr);
  }

  /** @brief Parse a multiplicative expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * multiplicative_expr : multiplicative_expr * unary_prefix_expr
   *                     | multiplicative_expr / unary_prefix_expr
   *                     | multiplicative_expr + unary_prefix_expr
   *                     | unary_prefix_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | multiplicative_expr * unary_prefix_expr | Both                 | Both                 |
   * | multiplicative_expr / unary_prefix_expr | Both                 | Both                 |
   * | multiplicative_expr % unary_prefix_expr | Both                 | Both                 |
   */
  auto parse_multiplicative_expr_opt(InstructionEmitter& emitter, ExprType expr_type,
                                     UnaryType unary_type) -> ExprResult
  {
    ExprResult lhs{parse_unary_prefix_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }

    while (is_multiplicative_op(lexer_->peek(true).type())) {
      auto type{lexer_->peek(true).type()};
      lexer_->chew(true);
      ExprResult const rhs{parse_unary_prefix_expr_opt(emitter, expr_type, UnaryType::both)};
      lhs = emitter.emit_expr(to_binary_op_opcode(type), lhs, rhs);
    }

    return lhs;
  }

  /** @brief Parse an additive expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * additive_expr : additive_expr + multiplicative_expr
   *                     | additive_expr - multiplicative_expr
   *                     | multiplicative_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | additive_expr + multiplicative_expr     | Both                 | Both                 |
   * | additive_expr - multiplicative_expr     | Both                 | Both                 |
   */
  auto parse_additive_expr_opt(InstructionEmitter& emitter, ExprType expr_type,
                               UnaryType unary_type) -> ExprResult
  {
    ExprResult lhs{parse_multiplicative_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }

    while (is_additive_op(lexer_->peek(true).type())) {
      auto type{lexer_->peek(true).type()};
      lexer_->chew(true);
      ExprResult const rhs{parse_multiplicative_expr_opt(emitter, expr_type, UnaryType::both)};
      lhs = emitter.emit_expr(to_binary_op_opcode(type), lhs, rhs);
    }

    return lhs;
  }

  /** @brief Parse a concat expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * concat_expr : concat_expr additive_expr(Non-unary)
   *             | additive_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | concat_expr additive_expr(Non-unary)    | Both                 | Both                 |
   */
  auto parse_concat_expr_opt(InstructionEmitter& emitter, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_additive_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }

    bool cont{true};
    while (cont) {
      // TODO(mgrettondann): make this hack more robust/reasonable!
      // We might want to parse the next token as a '/' so let's peek(true) now to populate the
      // next token.
      (void)lexer_->peek(true);
      auto rhs{parse_additive_expr_opt(emitter, expr_type, UnaryType::non_unary)};
      cont = rhs.has_one_value();
      if (cont) {
        lhs = emitter.emit_expr(Instruction::Opcode::concat, lhs, rhs);
      }
    }

    return lhs;
  }

  /** @brief Parse a comparison expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * comparison_expr : concat_expr EQ concat_expr
   *                 | concat_expr NE concat_expr
   *                 | concat_expr LESS_THAN concat_expr
   *                 | concat_expr LE concat_expr
   *                 | concat_expr GREATER_THAN concat_expr
   *                 | concat_expr GE concat_expr
   *                 | concat_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | concat_expr == concat_expr              | expr                 | Both                 |
   * | concat_expr != concat_expr              | expr                 | Both                 |
   * | concat_expr <  concat_expr              | expr                 | Both                 |
   * | concat_expr <= concat_expr              | expr                 | Both                 |
   * | concat_expr >  concat_expr              | expr                 | Both                 |
   * | concat_expr >= concat_expr              | expr                 | Both                 |
   */
  auto parse_comparison_expr_opt(InstructionEmitter& emitter, ExprType expr_type,
                                 UnaryType unary_type) -> ExprResult
  {
    ExprResult lhs{parse_concat_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value() || expr_type == ExprType::print_expr) {
      return lhs;
    }
    if (auto tok{lexer_->peek(true)}; is_comparison_op(tok.type())) {
      lexer_->chew(true);
      ExprResult const rhs{parse_concat_expr_opt(emitter, expr_type, UnaryType::both)};
      if (!rhs.has_one_value()) {
        error(Msg::expected_expr_after_comparison_op, lexer_->location(), tok, lexer_->peek(false));
      }
      return emitter.emit_expr(to_binary_op_opcode(tok.type()), lhs, rhs);
    }

    return lhs;
  }

  /** @brief Parse a re-match expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * re_match_expr : comparison_expr TILDE comparison_expr
   *               | comparison_expr NO_MATCH comparison_expr
   *               | comparison_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | comparison_expr ~ comparison_expr       | Both                 | Both                 |
   * | comparison_expr !~ comparison_expr      | Both                 | Both                 |
   */
  auto parse_re_match_expr_opt(InstructionEmitter& emitter, ExprType expr_type,
                               UnaryType unary_type) -> ExprResult
  {
    ExprResult lhs{parse_comparison_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }
    if (auto tok{lexer_->peek(true)}; is_re_match_op(tok.type())) {
      lexer_->chew(true);
      ExprResult const rhs{parse_comparison_expr_opt(emitter, expr_type, UnaryType::both)};
      if (!rhs.has_one_value()) {
        error(Msg::expected_expr_after_re_match_op, lexer_->location(), tok, lexer_->peek(false));
      }
      auto re_match{emitter.emit_expr(Instruction::Opcode::re_match, lhs, rhs)};
      if (tok.type() == Token::Type::no_match) {
        re_match = emitter.emit_expr(Instruction::Opcode::logical_not, ExprResult{re_match});
      }

      return ExprResult{re_match};
    }

    return lhs;
  }

  /** @brief Parse an in-array expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * in_array_expr : re_match_expr IN NAME
   *               | LPARENS multiple_expr_list RPARENS IN NAME
   *               | re_match_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | re_match_expr in name                   | Both                 | Both                 |
   * | ( multiple_expr_list ) in name          | Both                 | Non-unary            |
   */
  auto parse_in_array_expr_opt(InstructionEmitter& emitter, ExprType expr_type,
                               UnaryType unary_type) -> ExprResult
  {
    ExprResult lhs{parse_re_match_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }

    if (auto type{lexer_->peek(true).type()}; type == Token::Type::in) {
      lexer_->chew(true);
      auto name{lexer_->peek(false)};
      if (name != Token::Type::name) {
        error(Msg::expected_name_after_in, lexer_->location(), name);
      }

      lexer_->chew(false);
      std::abort();
    }

    return lhs;
  }

  /** @brief Parse an and expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * and_expr : and_expr AND newline_opt array_in_expr
   *          | array_in_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | and_expr && array_in_expr               | Both                 | Both                 |
   */
  auto parse_and_expr_opt(InstructionEmitter& emitter, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_in_array_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }

    while (lexer_->peek(true) == Token::Type::and_) {
      lexer_->chew(true);
      parse_newline_opt();
      auto rhs{parse_in_array_expr_opt(emitter, expr_type, unary_type)};
      if (!rhs.has_one_value()) {
        error(Msg::error_expected_expr_after_and, lexer_->location(), lexer_->peek(false));
      }
      lhs = emitter.emit_expr(Instruction::Opcode::logical_and, lhs, rhs);
    }

    return lhs;
  }

  /** @brief Parse an or expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * or_expr : or_expr AND newline_opt and_expr
   *         | array_in_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | or_expr && and_expr                     | Both                 | Both                 |
   */
  auto parse_or_expr_opt(InstructionEmitter& emitter, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_and_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }

    while (lexer_->peek(true) == Token::Type::or_) {
      lexer_->chew(true);
      parse_newline_opt();
      auto rhs{parse_in_array_expr_opt(emitter, expr_type, unary_type)};
      if (!rhs.has_one_value()) {
        error(Msg::error_expected_expr_after_or, lexer_->location(), lexer_->peek(false));
      }
      lhs = emitter.emit_expr(Instruction::Opcode::logical_or, lhs, rhs);
    }

    return lhs;
  }

  /** @brief Parse a ternary (? :) expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * or_expr : or_expr QUERY ternary_expr COLON ternary_expr
   *         | or_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | or_expr ? ternary_expr : ternary_expr   | Both                 | Both                 |
   */
  auto parse_ternary_expr_opt(InstructionEmitter& emitter, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    /* Generated code:
     *   w: LHS expr
     *      branch if false w, y
     *   x: true_branch
     *      branch z
     *   y: false_branch
     *      copy y, x
     *   z: ....
     */
    ExprResult lhs{parse_or_expr_opt(emitter, expr_type, unary_type)};
    if (!lhs.has_one_value() || lexer_->peek(true) != Token::Type::query) {
      return lhs;
    }

    lexer_->chew(true);

    Index const branch_if_false{
      emitter.emit_statement(Instruction::Opcode::branch_if_false, lhs, illegal_index)};

    ExprResult true_expr{parse_ternary_expr_opt(emitter, expr_type, unary_type)};
    if (!true_expr.has_one_value()) {
      error(Msg::expected_expr_after_query, lexer_->location(), lexer_->peek(false));
    }

    Index const branch_over_false{
      emitter.emit_statement(Instruction::Opcode::branch, illegal_index)};

    if (lexer_->peek(false) != Token::Type::colon) {
      error(Msg::expected_colon_after_truth_expr, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    emitter.at(branch_if_false).op2(emitter.next_instruction_index());

    ExprResult const false_expr{parse_ternary_expr_opt(emitter, expr_type, unary_type)};
    if (!true_expr.has_one_value()) {
      error(Msg::expected_expr_after_colon, lexer_->location(), lexer_->peek(false));
    }

    emitter.emit_copy(true_expr, false_expr);
    emitter.at(branch_over_false).op1(emitter.next_instruction_index());
    return true_expr;
  }

  /** @brief Parse an assignment expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * or_expr :  ternary_expr ASSIGN assignment_expr
   *         |  ternary_expr POW_ASSIGN assignment_expr
   *         |  ternary_expr MOD_ASSIGN assignment_expr
   *         |  ternary_expr MUL_ASSIGN assignment_expr
   *         |  ternary_expr DIV_ASSIGN assignment_expr
   *         |  ternary_expr ADD_ASSIGN assignment_expr
   *         |  ternary_expr SUB_ASSIGN assignment_expr
   *         | ternary_expr
   *
   * | Pattern                                 | Expr or print_expr?  | Unary or non-unary?  |
   * | :-------------------------------------- | :------------------- | :------------------- |
   * | ternary_expr = assignment_expr          | Both                 | Non-unary            |
   * | ternary_expr ^= assignment_expr         | Both                 | Non-unary            |
   * | ternary_expr %= assignment_expr         | Both                 | Non-unary            |
   * | ternary_expr *= assignment_expr         | Both                 | Non-unary            |
   * | ternary_expr /= assignment_expr         | Both                 | Non-unary            |
   * | ternary_expr += assignment_expr         | Both                 | Non-unary            |
   * | ternary_expr -= assignment_expr         | Both                 | Non-unary            |
   */
  auto parse_assignment_expr_opt(InstructionEmitter& emitter, ExprType expr_type,
                                 UnaryType unary_type) -> ExprResult
  {
    ExprResult lvalue{parse_ternary_expr_opt(emitter, expr_type, unary_type)};
    if (!lvalue.has_one_value() || !lvalue.is_lvalue()) {
      return lvalue;
    }

    Token const op{lexer_->peek(true)};
    if (!is_assignment_op(op.type())) {
      return lvalue;
    }
    lexer_->chew(true);

    ExprResult const rhs{parse_assignment_expr_opt(emitter, expr_type, unary_type)};
    if (!rhs.has_one_value()) {
      error(Msg::expected_expr_after_assignment_op, lexer_->location(), op, lexer_->peek(false));
    }

    auto lhs{emitter.dereference_lvalue(lvalue)};
    auto result{op == Token::Type::assign
                  ? rhs
                  : emitter.emit_expr(to_binary_op_opcode(op.type()), lhs, rhs)};
    emitter.emit_store_lvalue(lvalue, result);
    return result;
  }

  /** @brief Parse an expression (of any type)
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @return            Index of emitted expression, and updated expression type
   *
   * The official grammar for awk classifies expressions into two main classes: print_expr or
   * expr. The print_expr class is a subset of expr - it basically excludes getline calls, and
   * comparison operators.
   *
   * Each of these two classes is further subdivided into unary ond non-unary.  The purpose of
   * this seems to be to exclude unary '+' and '-' from the right hand side of string
   * concatenation.
   *
   * In the grammar comments before functions ignore these differences for simplicity of
   * exposition. The following table details the classifications.
   *
   * | Pattern                             | Expr or print_expr?  | Unary or non-unary?  |
   * | :---------------------------------- | :------------------- | :------------------- |
   * | + expr                              | Both                 | Unary                |
   * | - expr                              | Both                 | Unary                |
   * | ( expr )                            | Both                 | Non-unary            |
   * | ! expr                              | Both                 | Non-unary            |
   * | expr * expr                         | Both                 | Both                 |
   * | expr / expr                         | Both                 | Both                 |
   * | expr % expr                         | Both                 | Both                 |
   * | expr ^ expr                         | Both                 | Both                 |
   * | expr + expr                         | Both                 | Both                 |
   * | expr - expr                         | Both                 | Both                 |
   * | expr non_unary_expr                 | Both                 | Both                 |
   * | expr < expr                         | Expr                 | Both                 |
   * | expr <= expr                        | Expr                 | Both                 |
   * | expr != expr                        | Expr                 | Both                 |
   * | expr == expr                        | Expr                 | Both                 |
   * | expr >= expr                        | Expr                 | Both                 |
   * | expr > expr                         | Expr                 | Both                 |
   * | expr ~ expr                         | Both                 | Both                 |
   * | expr !~ expr                        | Both                 | Both                 |
   * | expr in name                        | Both                 | Both                 |
   * | ( multiple_expr_list ) in name      | Both                 | Non-unary            |
   * | expr && expr                        | Both                 | Both                 |
   * | expr || expr                        | Both                 | Both                 |
   * | expr ? expr : expr                  | Both                 | Both                 |
   * | string                              | Both                 | Non-unary            |
   * | lvalue                              | Both                 | Non-unary            |
   * | number                              | Both                 | Non-unary            |
   * | ere                                 | Both                 | Non-unary            |
   * | lvalue ++                           | Both                 | Non-unary            |
   * | lvalue --                           | Both                 | Non-unary            |
   * | ++ lvalue                           | Both                 | Non-unary            |
   * | -- lvalue                           | Both                 | Non-unary            |
   * | lvalue ^= expr                      | Both                 | Non-unary            |
   * | lvalue *= expr                      | Both                 | Non-unary            |
   * | lvalue /= expr                      | Both                 | Non-unary            |
   * | lvalue %= expr                      | Both                 | Non-unary            |
   * | lvalue += expr                      | Both                 | Non-unary            |
   * | lvalue -= expr                      | Both                 | Non-unary            |
   * | lvalue  = expr                      | Both                 | Non-unary            |
   * | func_name( expr_list_opt )          | Both                 | Non-unary            |
   * | builtin_func_name ( expr_list_opt ) | Both                 | Non-unary            |
   * | builtin_func_name                   | Both                 | Non-unary            |
   * | expr `|` simple_get                 | Expr                 | Both                 |
   * | simple_get `<` expr                 | Expr                 | Non-unary            |
   */
  auto parse_expr_opt(InstructionEmitter& emitter, ExprType expr_type) -> ExprResult
  {
    ExprResult result{parse_assignment_expr_opt(emitter, expr_type, UnaryType::both)};
    return result;
  }

  template<typename... Args>
  auto parse_expr(InstructionEmitter& emitter, ExprType expr_type, Msg error_msg) -> ExprResult
  {
    ExprResult result{parse_assignment_expr_opt(emitter, expr_type, UnaryType::both)};
    if (result.has_no_values()) {
      error(error_msg, lexer_->location(), lexer_->peek(false));
    }
    return result;
  }

  /** @brief Parse the second & subsequent elements of a multiple_expr_list.
   *
   * @param instrs      Instructions to emit into
   * @param inserter_it Iterator to use to insert the expression indices into a location
   */
  void parse_multiple_expr_list_rest(InstructionEmitter& emitter, auto inserter_it)
  {
    while (true) {
      // Parse the expression
      auto result = parse_expr_opt(emitter, ExprType::expr);
      if (result.has_many_values()) {
        error(Msg::cannot_nest_multiple_expr_lists, lexer_->location());
      }

      if (result.has_no_values()) {
        error(Msg::expected_expr_after_comma, lexer_->location(), lexer_->peek(false));
      }

      // Insert index into list of expressions.
      *inserter_it++ = {result.index(), result.flags()};

      // Chew the comma, and newline separators.
      if (lexer_->peek(false) != Token::Type::comma) {
        break;
      }
      lexer_->chew(false);
      parse_newline_opt();
    }
  }

  /** @brief Parse an expression list (of any type)
   *
   * @param instrs      Instructions to emit into
   * @param inserter_it Iterator to use to insert the expression indices into a location
   * @param list_type   What type of list are we processing?
   */
  void parse_expr_list_opt(InstructionEmitter& emitter, auto inserter_it, ExprListType list_type)
  {
    std::size_t element_count{0};  // Number of elements
    while (true) {
      // Parse the expression
      auto result = parse_expr_opt(emitter, to_expr_type(list_type));
      if (result.has_many_values()) {
        // This turned out to be a multiple_expr_list - so we just lift the results up a level,
        // and return.
        for (auto const& r : result) {
          *inserter_it++ = r;
        }
        break;
      }

      if (result.has_no_values()) {
        if (element_count != 0) {
          error(Msg::expected_expr_after_comma, lexer_->location(), lexer_->peek(false));
        }
        break;
      }

      // Insert index into list of expressions.
      *inserter_it++ = {result.index(), result.flags()};
      ++element_count;

      // Chew the comma, and newline separators.
      if (lexer_->peek(false) != Token::Type::comma) {
        break;
      }
      lexer_->chew(false);
      parse_newline_opt();
    }

    if (list_type == ExprListType::multiple_expr_list && element_count == 1) {
      error(Msg::expected_two_exprs_in_list, lexer_->location(), lexer_->peek(false));
    }
  }

  // NOLINTNEXTLINE
  void parse_do_while([[maybe_unused]] InstructionEmitter& emitter)
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  /** @brief Parse a delete statement.
   *
   * @param  instrs Instructions to append to
   *
   * delete_statement : DELETE NAME LSQUARE expr_list RSQUARE
   */
  void parse_delete_statement([[maybe_unused]] InstructionEmitter& emitter)  // NOLINT
  {
    assert(lexer_->peek(false) == Token::Type::delete_);
    std::abort();
  }

  /** @brief Parse a redirection expression.
   *
   * @param  instrs Instructions to append to
   *
   * redirection : GREATER_THAN expr
   *             | APPEND expr
   *             | PIPE expr
   */
  auto parse_redirection_opt(InstructionEmitter& emitter) -> ExprResult
  {
    auto tok{lexer_->peek(false)};

    bool const is_open{tok == Token::Type::greater_than};
    bool const is_append{tok == Token::Type::append};
    bool const is_popen{tok == Token::Type::pipe};

    if (!is_open && !is_append && !is_popen) {
      return ExprResult{};
    }

    lexer_->chew(false);

    auto out_expr{parse_expr_opt(emitter, ExprType::expr)};
    if (!out_expr.has_one_value()) {
      error(Msg::expected_expr_after_redirection, lexer_->location(), tok, lexer_->peek(false));
    }

    if (is_popen) {
      return emitter.emit_expr(Instruction::Opcode::popen, out_expr);
    }

    Integer const opt{is_append ? INT64_C(1) : INT64_C(0)};
    return emitter.emit_expr(Instruction::Opcode::open, out_expr, opt);
  }

  /** @brief Parse a print statement.
   *
   * @param  instrs Instructions to append to
   *
   * simple_print_statement : PRINT print_expr_list_opt
   *                        | PRINT LPARENS multiple_expr_list RPARENS
   *                        | PRINTF print_expr_list
   *                        | PRINTF LPARENS multiple_expr_list RPARENS
   *
   * redirection : GREATER_THAN expr
   *             | APPEND expr
   *             | PIPE expr
   *
   * print_statement : simple_print_statement
   *                 | simple_print_statement redirection
   */
  void parse_print_statement(InstructionEmitter& emitter)
  {
    auto const& tok = lexer_->peek(false);
    assert(tok == Token::Type::print || tok == Token::Type::printf);

    bool const is_printf{tok == Token::Type::printf};

    lexer_->chew(false);

    // So the problem we have here is that the token '(' is valid is the first token in
    // print_expr_list_opt.  So if that is the next token we have no idea whether we are parsing a
    // print_expr_list or a multiple_expr_list.  Hence the maybe_multiple_expr_list where we leave
    // it to the expression parser to work it out.
    ExprResult indices;
    parse_expr_list_opt(emitter, indices.back_inserter(), ExprListType::print_expr_list);

    if (is_printf && indices.has_no_values()) {
      error(Msg::expected_list_to_printf, lexer_->location(), lexer_->peek(false));
    }

    if (!is_printf && indices.has_no_values()) {
      // We have a plain print.  We want to print $0.
      auto const lit_expr{emitter.emit_expr(Instruction::Opcode::load_literal, Integer{0})};
      auto field{emitter.emit_expr(Instruction::Opcode::field, lit_expr)};
      *indices.back_inserter()++ = {field.index(), field.flags()};
    }

    // Now get the redirection.  If we don't have a redirection we will output to standard out.
    ExprResult redir{parse_redirection_opt(emitter)};
    if (!redir.has_one_value()) {
      int const fd{STDOUT_FILENO};
      redir = emitter.emit_expr(Instruction::Opcode::load_literal, FileDescriptor{fd});
    }

    ExprResult fs;
    if (!is_printf && indices.has_many_values()) {
      fs = emitter.emit_expr(Instruction::Opcode::variable, VariableName{"OFS"});
    }

    if (!is_printf) {
      bool first{true};
      for (auto const idx : indices) {
        if (first) {
          first = false;
        }
        else {
          emitter.emit_statement(Instruction::Opcode::print, fs, redir);
        }
        emitter.emit_statement(Instruction::Opcode::print, ExprResult{idx}, redir);
      }
      emitter.emit_statement(Instruction::Opcode::print,
                             emitter.emit_expr(Instruction::Opcode::variable, VariableName{"ORS"}),
                             redir);
    }
    else {
      auto const pp{emitter.emit_expr(Instruction::Opcode::open_param_pack)};
      for (auto const& idx : indices) {
        emitter.emit_statement(Instruction::Opcode::push_param, pp, ExprResult{idx});
      }
      emitter.emit_statement(Instruction::Opcode::close_param_pack, pp);
      emitter.emit_statement(Instruction::Opcode::printf, pp, redir);
    }
  }

  /** @brief Parse a simple statement.
   *
   * @param  instrs Instructions to append to
   * @return        Did we parse anything?
   *
   * simple_statement : DELETE NAME LSQUARE expr_list RSQUARE
   *                  | expr
   *                  | print_statement
   *                  | empty
   */
  auto parse_simple_statement_opt(InstructionEmitter& emitter) -> bool
  {
    auto const& tok{lexer_->peek(false)};

    if (tok == Token::Type::delete_) {
      parse_delete_statement(emitter);
      return true;
    }

    if (tok == Token::Type::print || tok == Token::Type::printf) {
      parse_print_statement(emitter);
      return true;
    }

    return parse_expr_opt(emitter, ExprType::expr).has_one_value();
  }

  auto parse_terminatable_statement_opt(InstructionEmitter& emitter) -> ParseStatementResult
  {
    auto const& tok{lexer_->peek(false)};
    switch (tok.type()) {
    case Token::Type::break_:
    case Token::Type::continue_:
    case Token::Type::next:
      // TODO(mgrettondann): Implement.
      std::abort();
      lexer_->chew(false);
      break;
    case Token::Type::exit: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(emitter, ExprType::expr)};
      if (!expr.has_one_value()) {
        expr = emitter.emit_expr(Instruction::Opcode::load_literal, Integer{INT64_C(0)});
      }

      // TODO(mgrettondann): Implement.
      std::abort();
      break;
    }
    case Token::Type::return_: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(emitter, ExprType::expr)};
      if (!expr.has_one_value()) {
        expr = emitter.emit_expr(Instruction::Opcode::load_literal, Integer{INT64_C(0)});
      }
      // TODO(mgrettondann): Implement.
      std::abort();
      break;
    }
    case Token::Type::do_:
      parse_do_while(emitter);
      break;
    default:
      if (!parse_simple_statement_opt(emitter)) {
        return ParseStatementResult::none;
      }
      break;
    }

    if (lexer_->peek(false) == Token::Type::semicolon ||
        lexer_->peek(false) == Token::Type::newline) {
      lexer_->chew(false);
      parse_newline_opt();
      return ParseStatementResult::terminated;
    }

    return ParseStatementResult::unterminated;
  }

  /** @brief Parse an if statement
   *
   * @param emitter Instruction emitter class
   * @return Whether the statement was terminated or not.
   *
   * if_stmt : IF LPARENS expr RPARENS newline_opt statement
   *         | IF LPARENS expr RPARENS newline_opt terminated_statement ELSE statement
   */
  auto parse_if_stmt(InstructionEmitter& emitter) -> ParseStatementResult
  {
    assert(lexer_->peek(false) == Token::Type::if_);
    lexer_->chew(false);

    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_if, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    ExprResult const cond{parse_expr(emitter, ExprType::expr, Msg::expected_expr_after_if)};
    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_if_expr, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);
    parse_newline_opt();

    Index const false_branch{
      emitter.emit_statement(Instruction::Opcode::branch_if_false, cond, illegal_index)};

    ParseStatementResult const then_stmt{parse_statement(emitter, Msg::missing_statement_after_if)};

    if (then_stmt == ParseStatementResult::unterminated ||
        lexer_->peek(false) != Token::Type::else_) {
      emitter.at(false_branch).op2(emitter.next_instruction_index());
      return then_stmt;
    }

    assert(then_stmt == ParseStatementResult::terminated);
    assert(lexer_->peek(false) == Token::Type::else_);

    lexer_->chew(false);
    parse_newline_opt();
    Index const true_branch{emitter.emit_statement(Instruction::Opcode::branch, illegal_index)};
    emitter.at(false_branch).op2(emitter.next_instruction_index());
    ParseStatementResult const else_stmt{
      parse_statement(emitter, Msg::missing_statement_after_else)};
    emitter.at(true_branch).op1(emitter.next_instruction_index());

    return else_stmt;
  }

  /** @brief Parse a while statement
   *
   * @param emitter Instruction emitter class
   * @return Whether the statement was terminated or not.
   *
   * while_stmt : WHILE LPARENS epxr RPARENS newline_opt statement
   */
  auto parse_while_stmt(InstructionEmitter& emitter) -> ParseStatementResult
  {
    assert(lexer_->peek(false) == Token::Type::while_);
    lexer_->chew(false);

    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_while, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    Index const while_cond{emitter.next_instruction_index()};
    ExprResult const cond{parse_expr(emitter, ExprType::expr, Msg::expected_expr_after_while)};
    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_while_expr, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);
    parse_newline_opt();

    Index const escape_loop{
      emitter.emit_statement(Instruction::Opcode::branch_if_false, cond, illegal_index)};

    ParseStatementResult const while_stmt{
      parse_statement(emitter, Msg::missing_statement_after_while)};
    emitter.emit_statement(Instruction::Opcode::branch, while_cond);
    emitter.at(escape_loop).op2(emitter.next_instruction_index());

    return while_stmt;
  }

  /** @brief Parse a for statement
   *
   * @param emitter Instruction emitter class
   * @return Whether the statement was terminated or not.
   *
   * for_stmt : FOR LPARENS simple_statement_opt SEMICOLON expr_opt SEMICOLON simple_statement_opt
   *                RPARENS newline_opt statement
   *          | FOR LPARENS name IN name RPARENS newline_opt statement
   *
   * Because we emit code as we parse this is quite branchy but basically looks like:
   *
   *                  init
   * condition_start: cond
   *                  branch_if_false end
   *                  branch body
   *    update_start: update
   *                  branch condition_start
   *            body: body
   *                  branch update_start
   *             end:
   */
  auto parse_for_stmt(InstructionEmitter& emitter) -> ParseStatementResult
  {
    assert(lexer_->peek(false) == Token::Type::for_);
    lexer_->chew(false);

    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens_after_for, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    // Initialisation
    (void)parse_simple_statement_opt(emitter);

    // Semicolon
    if (lexer_->peek(false) != Token::Type::semicolon) {
      error(Msg::expected_semicolon_after_for_init_statement, lexer_->location(),
            lexer_->peek(false));
    }
    lexer_->chew(false);

    // Condition
    Index const condition_start{emitter.next_instruction_index()};
    ExprResult const cond{parse_expr_opt(emitter, ExprType::expr)};
    Index loop_escape_branch{illegal_index};
    if (cond.has_one_value()) {
      loop_escape_branch =
        emitter.emit_statement(Instruction::Opcode::branch_if_false, cond, illegal_index);
    }
    Index const skip_update_branch{
      emitter.emit_statement(Instruction::Opcode::branch, illegal_index)};

    // Semicolon
    if (lexer_->peek(false) != Token::Type::semicolon) {
      error(Msg::expected_semicolon_after_for_cond_expr, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);

    // Update
    Index const update_start{emitter.next_instruction_index()};
    (void)parse_simple_statement_opt(emitter);
    emitter.emit_statement(Instruction::Opcode::branch, condition_start);

    // Rparens
    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens_after_for_update_stmt, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);
    parse_newline_opt();

    // Body
    emitter.at(skip_update_branch).op1(emitter.next_instruction_index());
    auto result{parse_statement(emitter, Msg::missing_statement_after_for)};
    emitter.emit_statement(Instruction::Opcode::branch, update_start);

    // End
    emitter.at(loop_escape_branch).op2(emitter.next_instruction_index());
    return result;
  }

  /** @brief Parse an optional statement
   *
   * @param  instrs Instructions to write code into
   * @return        Flag indicating whether anything was parsed, and if so whether it was
   *                terminated.
   *
   * statement : if_stmt
   *           | while_stmt
   *           | for_stmt
   *           | SEMICOLON newline_opt
   *           | action newline_opt
   */
  auto parse_statement_opt(InstructionEmitter& emitter) -> ParseStatementResult
  {
    auto const& tok{lexer_->peek(false)};
    if (tok == Token::Type::if_) {
      return parse_if_stmt(emitter);
    }
    if (tok == Token::Type::while_) {
      return parse_while_stmt(emitter);
    }
    if (tok == Token::Type::for_) {
      return parse_for_stmt(emitter);
    }
    if (tok == Token::Type::semicolon) {
      lexer_->chew(false);
      parse_newline_opt();
      return ParseStatementResult::terminated;
    }
    if (tok == Token::Type::lbrace) {
      parse_action(emitter);
      parse_newline_opt();
      return ParseStatementResult::terminated;
    }

    auto res{parse_terminatable_statement_opt(emitter)};
    return res;
  }

  auto parse_statement(InstructionEmitter& emitter, Msg msg) -> ParseStatementResult
  {
    auto result{parse_statement_opt(emitter)};

    if (result == ParseStatementResult::none) {
      error(msg, lexer_->location());
    }

    return result;
  }

  /** \brief  Parse a statement list.
   *
   * @param  instrs Instructions block to put instructions into.
   *
   * statement_list_opt : statement terminator statement_list_opt
   *                    | statement
   *                    | *empty*
   */
  void parse_statement_list_opt(InstructionEmitter& emitter)
  {
    bool cont{true};
    while (cont) {
      auto res{parse_statement_opt(emitter)};
      cont = (res == ParseStatementResult::terminated);
    }
  }

  /** \brief         Parse an optional action, generating code at the end of the instructions
   * given. \param  instrs Instructions to append to. \return        True iff we parsed an action.
   *
   * action_opt : LBRACE newline_opt statement_list_opt RBRACE
   *            | empty
   */
  auto parse_action_opt(InstructionEmitter& emitter) -> bool
  {
    if (lexer_->peek(false) != Token::Type::lbrace) {
      return false;
    }

    lexer_->chew(false);
    parse_newline_opt();
    parse_statement_list_opt(emitter);

    if (lexer_->peek(false) != Token::Type::rbrace) {
      error(Msg::missing_rbrace, lexer_->location(), lexer_->peek(false));
    }
    else {
      lexer_->chew(false);
    }

    return true;
  }

  /** \brief  Parse an action.
   *
   * action : action_opt (If empty error)
   */
  void parse_action(InstructionEmitter& emitter)
  {
    if (!parse_action_opt(emitter)) {
      error(Msg::expected_action, lexer_->location(), lexer_->peek(false));
    }
  }

  /** \brief  Parse a function definition name, returning the function name.
   *  \return Function name parsed.
   *
   * func_def_name : NAME
   *               | FUNC_NAME
   */
  auto parse_func_def_name() -> FuncName
  {
    auto tok{lexer_->peek(false)};
    if (tok == Token::Type::builtin_func_name) {
      error(Msg::cannot_redefine_builtin_functions, lexer_->location(), tok);
    }
    if (tok != Token::Type::name && tok != Token::Type::builtin_func_name) {
      error(Msg::expected_function_name, lexer_->location(), tok);
    }

    if (tok == Token::Type::name) {
      return FuncName{tok.name()};
    }

    assert(tok == Token::Type::func_name);
    return FuncName{tok.func_name()};
  }

  /** \brief  Parse the function definition header.
   *  \return Instructions for function.
   *
   * function_def: FUNCTION func_def_name LPARENS param_list_opt RPARENS newline_opt
   */
  auto parse_function_def() -> Instructions&
  {
    if (lexer_->peek(false) != Token::Type::function) {
      error(Msg::expected_function, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);
    auto func_name{parse_func_def_name()};
    if (lexer_->peek(false) != Token::Type::lparens) {
      error(Msg::expected_lparens, lexer_->location(), lexer_->peek(false));
    }
    lexer_->chew(false);
    // TODO(mgrettondann): Parse proper parameters
    if (lexer_->peek(false) != Token::Type::rparens) {
      error(Msg::expected_rparens, lexer_->location(), lexer_->peek(false));
    }

    return program_.function(func_name.get());
  }

  auto generate_pattern_range_guard() -> VariableName
  {
    std::string var_name{"*pattern_guard*"};
    var_name += std::to_string(pattern_guard_id_++);
    return VariableName{var_name};
  }

  // Returns index of comparison instruction if we emitted the pattern sequence.
  auto parse_normal_pattern_opt(InstructionEmitter& emitter) -> Index
  {
    ExprResult const pat1{parse_expr_opt(emitter, ExprType::expr)};
    if (!pat1.has_one_value()) {
      return illegal_index;
    }

    if (lexer_->peek(false) != Token::Type::comma) {
      // This is just a simple pattern.
      return emitter.emit_statement(Instruction::Opcode::branch_if_false, pat1, illegal_index);
    }
    lexer_->chew(false);

    // Pattern range.

    //        pat1: <pat1>
    //           1: variable range_guard
    //           2: lit1
    //              branch_if_false pat1, check_guard
    //              store_lvalue 1, 2
    // check_guard: load_lvalue 1
    //           3: is_equal check_guard, 2
    //        skip: branch_if_false 3, end
    //        pat2: <pat2>
    //              branch_if_false pat2, execute
    //           5: lit0
    //              store_lvalue 1, 5
    //     execute: <statement>
    //         end:
    //
    VariableName const guard_vn{generate_pattern_range_guard()};
    ExprResult const lit1{emitter.emit_expr(Instruction::Opcode::load_literal, Integer{1})};
    ExprResult const guard_var{emitter.emit_expr(Instruction::Opcode::variable, guard_vn)};
    Index const skip_start{
      emitter.emit_statement(Instruction::Opcode::branch_if_false, pat1, illegal_index)};
    emitter.emit_store_lvalue(guard_var, lit1);
    emitter.at(skip_start).op2(emitter.next_instruction_index());
    ExprResult const check_guard{emitter.emit_expr(Instruction::Opcode::is_equal, guard_var, lit1)};
    Index const skip{
      emitter.emit_statement(Instruction::Opcode::branch_if_false, check_guard, illegal_index)};

    ExprResult const pat2{parse_expr(emitter, ExprType::expr, Msg::missing_pattern_after_comma)};
    Index const skip_guard_clear{
      emitter.emit_statement(Instruction::Opcode::branch_if_false, pat2, illegal_index)};
    ExprResult const lit0{emitter.emit_expr(Instruction::Opcode::load_literal, Integer{0})};
    emitter.emit_store_lvalue(guard_var, lit0);
    emitter.at(skip_guard_clear).op2(emitter.next_instruction_index());

    return skip;
  }

  /** \brief Parse an optional item.
   *
   * @return \c true iff we parsed any items.
   *
   * item_optional : action
   *               | normal_pattern action_opt
   *               | BEGIN action
   *               | END action
   *               | function_def action
   */
  auto parse_item_opt() -> bool
  {
    auto tok{lexer_->peek(false)};
    if (tok == Token::Type::lbrace) {
      InstructionEmitter emitter{program_.per_record()};
      parse_action(emitter);
    }
    else if (tok == Token::Type::begin) {
      lexer_->chew(false);
      InstructionEmitter emitter{program_.begin()};
      parse_action(emitter);
    }
    else if (tok == Token::Type::end) {
      lexer_->chew(false);
      InstructionEmitter emitter{program_.end()};
      parse_action(emitter);
    }
    else if (tok == Token::Type::function) {
      Instructions& fn{parse_function_def()};
      InstructionEmitter emitter{fn};
      parse_action(emitter);
    }
    else {
      InstructionEmitter emitter{program_.per_record()};
      if (auto result{parse_normal_pattern_opt(emitter)}; result != illegal_index) {
        if (!parse_action_opt(emitter)) {
          // We need to put a print $0 action in here.
          // 0: emit_load_literal 0
          // 1: field 0
          // 2: load_lvalue 1
          // 3: emit_load_literal STDOUT_FILENO
          // 4: print 2, 3
          // 5: emit_variable RS
          // 6: load_lvalue 5
          // 7: print 6, 3
          int const fd{STDOUT_FILENO};
          auto const lit{emitter.emit_expr(Instruction::Opcode::load_literal, Integer{0})};
          auto const field{emitter.emit_expr(Instruction::Opcode::field, lit)};
          auto const fileno{
            emitter.emit_expr(Instruction::Opcode::load_literal, FileDescriptor(fd))};
          emitter.emit_statement(Instruction::Opcode::print, field, fileno);
          auto const rs{emitter.emit_expr(Instruction::Opcode::variable, VariableName{"RS"})};
          emitter.emit_statement(Instruction::Opcode::print, rs, fileno);
        }

        emitter.at(result).op2(emitter.next_instruction_index());
      }
      else {
        return false;
      }
    }

    return true;
  }

  /** \brief parse an optional terminator.
   *
   * @return \c true iff we parsed any terminators.
   *
   * terminator_optional : SEMICOLON terminator_optional
   *                     | NEWLINE terminator_optional
   *                     | empty
   */
  auto parse_terminator_opt() -> bool
  {
    bool cont{true};
    bool done{false};
    while (cont) {
      auto token{lexer_->peek(false)};
      cont = false;
      if (token == Token::Type::semicolon || token == Token::Type::newline) {
        lexer_->chew(false);
        done = true;
        cont = true;
      }
    }
    return done;
  }

  /** \brief Parse an item_list:
   *
   * parse_item_list_maybe_unterminated : item terminator item_list_maybe_unterminated
   *                                    | item
   *                                    | *empty*
   */
  void parse_item_list_maybe_unterminated()
  {
    while (parse_item_opt() && parse_terminator_opt()) {
      ; /* do nothing */
    }
  }

  /** \brief Parse the program
   *
   * program : item_list_maybe_unterminated
   */
  void parse_program()
  {
    // Skip any initial new-lines
    while (lexer_->peek(false) == Token::Type::newline) {
      lexer_->chew(false);
    }

    // First of all we need to do an implicit BEGIN { srand(); }.
    // This needs to be in a block so that emitter is detroyed before we call it again.
    {
      InstructionEmitter emitter{program_.begin()};
      emitter.emit_expr(Instruction::Opcode::srand,
                        emitter.emit_expr(Instruction::Opcode::current_time));
    }

    parse_item_list_maybe_unterminated();
    if (lexer_->peek(false) != Token::Type::eof) {
      error(Msg::failed_to_read_whole_program, lexer_->location(), lexer_->peek(false));
    }
  }

  auto program() -> ParsedProgram& { return program_; }

private:
  std::unique_ptr<Lexer> lexer_;
  ParsedProgram program_;
  std::uint64_t pattern_guard_id_{0};
};
}  // namespace GD::Awk::Details

auto GD::Awk::parse(std::unique_ptr<Lexer>&& lexer) -> ParsedProgram
{
  Details::ParseState parser{std::move(lexer)};
  parser.parse_program();
  return std::move(parser.program());
}
