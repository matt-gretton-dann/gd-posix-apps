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
using ExprIndex = std::optional<Instruction::Index>;

/** Types of expression.
 *
 * There are two types of expression: print_expr, and plain expr.
 *
 * Unfortunately, due to parser ambiguities we can get in the situation where we don't know which
 * we have. Therefore, we record a third type 'maybe_expr'.
 */
enum class ExprType {
  print_expr,  ///< Definitely a print_expr.
  expr,        ///< Definitely an expr.
  maybe_expr   ///< Could be print_expr or expr.
};

/** Types of expression list.
 *
 * There are three types of expression list:
 *  * print_expr_list - used in print & printf statements
 *  * expr_list - A list of expressions that may have only one entry.
 *  * multiple_expr_list - A list of expressions that must have at least two entries.
 *
 * However, print_expr_list and multiple_expr_list can appear in contexts where we don't know
 * which we have.  So we provide a fourth 'maybe' option to be used when we are uncertain.
 */
enum class ExprListType {
  print_expr_list,          ///< Definitely a print_expr_list
  expr_list,                ///< Definitely an expr_list
  multiple_expr_list,       ///< Definitely a multiple_expr_list
  maybe_multiple_expr_list  ///< Could be print_expr_list or multiple_expr_list.
};

static auto to_expr_type(ExprListType t) -> ExprType
{
  switch (t) {
  case ExprListType::print_expr_list:
    return ExprType::print_expr;
  case ExprListType::expr_list:
  case ExprListType::multiple_expr_list:
    return ExprType::expr;
  case ExprListType::maybe_multiple_expr_list:
    return ExprType::maybe_expr;
  }
}

static auto to_expr_list_type(ExprType t, ExprListType expr_type) -> ExprListType
{
  switch (t) {
  case ExprType::print_expr:
    return ExprListType::print_expr_list;
  case ExprType::expr:
    return expr_type == ExprListType::expr_list ? ExprListType::expr_list
                                                : ExprListType::multiple_expr_list;
  case ExprType::maybe_expr:
    return ExprListType::maybe_multiple_expr_list;
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
  ExprIndex index{std::nullopt};
  ExprType type{ExprType::maybe_expr};
  bool is_lvalue{false};
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

class ParseState
{
public:
  explicit ParseState(std::unique_ptr<Lexer>&& lexer) : lexer_(std::move(lexer)) {}

  /** @brief emit a load literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, Integer value) -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, value);
    return instrs.size() - 1;
  }

  /** @brief emit a load literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param fd      Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, FileDescriptor fd) -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, fd);
    return instrs.size() - 1;
  }

  /** @brief emit a load literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, Floating value) -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, value);
    return instrs.size() - 1;
  }

  /** @brief emit a load literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, std::string const& value)
    -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, value);
    return instrs.size() - 1;
  }

  /** @brief emit a load literal regex instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, std::regex const& re) -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, re);
    return instrs.size() - 1;
  }

  /** @brief emit a field instruction
   *
   * @param instrs  Instructions to emit into
   * @param expr    Offset of expression containing field ID.
   * @return        Index of emitted literal.
   */
  static auto emit_field(Instructions& instrs, ExprResult op1) -> Instruction::Index
  {
    auto idx{emit_maybe_lvalue(instrs, op1)};
    instrs.emplace_back(Instruction::Opcode::field, idx);
    return instrs.size() - 1;
  }

  /** @brief emit a variable instruction
   *
   * @param instrs  Instructions to emit into
   * @param vn      Variable name
   * @return        Index of emitted instruction
   */
  static auto emit_variable(Instructions& instrs, VariableName const& vn) -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::variable, vn);
    return instrs.size() - 1;
  }

  static auto emit_break([[maybe_unused]] Instructions& instrs) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_continue([[maybe_unused]] Instructions& instrs) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_next([[maybe_unused]] Instructions& instrs) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_exit([[maybe_unused]] Instructions& instrs, [[maybe_unused]] ExprResult op1)
    -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_return([[maybe_unused]] Instructions& instrs, [[maybe_unused]] ExprResult op1)
    -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_open([[maybe_unused]] Instructions& instrs, [[maybe_unused]] ExprResult op1,
                        [[maybe_unused]] ExprResult op2) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_popen([[maybe_unused]] Instructions& instrs, [[maybe_unused]] ExprResult op1)
    -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_maybe_lvalue(Instructions& instrs, ExprResult expr) -> Instruction::Index
  {
    assert(expr.index.has_value());
    if (!expr.is_lvalue) {
      return *(expr.index);
    }

    instrs.emplace_back(Instruction::Opcode::load_lvalue, *(expr.index));
    return instrs.size() - 1;
  }

  static auto emit_store_lvalue(Instructions& instrs, ExprResult lvalue, ExprResult expr)
    -> Instruction::Index
  {
    assert(lvalue.index.has_value());
    assert(lvalue.is_lvalue);
    assert(expr.index.has_value());
    auto expr_idx = emit_maybe_lvalue(instrs, expr);

    instrs.emplace_back(Instruction::Opcode::store_lvalue, *(lvalue.index), expr_idx);
    return instrs.size() - 1;
  }

  static auto emit_print(Instructions& instrs, ExprResult op1, ExprResult op2) -> Instruction::Index
  {
    auto idx1{emit_maybe_lvalue(instrs, op1)};
    auto idx2{emit_maybe_lvalue(instrs, op2)};
    instrs.emplace_back(Instruction::Opcode::print, idx1, idx2);
    return instrs.size() - 1;
  }

  // NOLINTNEXTLINE
  static auto emit_printf(Instructions& instrs, ExprResult stream, ExprResult param_pack)
    -> Instruction::Index
  {
    assert(param_pack.index.has_value());
    assert(instrs[*(param_pack.index)].opcode() == Instruction::Opcode::open_param_pack);
    auto idx1{emit_maybe_lvalue(instrs, stream)};
    instrs.emplace_back(Instruction::Opcode::printf, idx1, *(param_pack.index));
    return instrs.size() - 1;
  }

  static auto emit_open_param_pack(Instructions& instrs) -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::open_param_pack);
    return instrs.size() - 1;
  }

  // NOLINTNEXTLINE
  static auto emit_push_param(Instructions& instrs, ExprResult param_pack, ExprResult expr)
    -> Instruction::Index
  {
    assert(param_pack.index.has_value());
    assert(instrs[*(param_pack.index)].opcode() == Instruction::Opcode::open_param_pack);
    auto expr_idx{emit_maybe_lvalue(instrs, expr)};
    instrs.emplace_back(Instruction::Opcode::push_param, *(param_pack.index), expr_idx);
    return instrs.size() - 1;
  }

  static auto emit_close_param_pack(Instructions& instrs, Instruction::Index op1)
    -> Instruction::Index
  {
    assert(instrs[op1].opcode() == Instruction::Opcode::open_param_pack);
    instrs.emplace_back(Instruction::Opcode::close_param_pack, op1);
    return instrs.size() - 1;
  }

  static auto emit_add(Instructions& instrs, ExprResult lhs, ExprResult rhs) -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::add, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_sub(Instructions& instrs, ExprResult lhs, ExprResult rhs) -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::sub, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_power(Instructions& instrs, ExprResult lhs, ExprResult rhs) -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::power, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_multiply(Instructions& instrs, ExprResult lhs, ExprResult rhs)
    -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::multiply, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_divide(Instructions& instrs, ExprResult lhs, ExprResult rhs)
    -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::divide, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_modulo(Instructions& instrs, ExprResult lhs, ExprResult rhs)
    -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::modulo, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_concat(Instructions& instrs, ExprResult lhs, ExprResult rhs)
    -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::concat, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_comparison_op(Instructions& instrs, Token::Type type, ExprResult lhs,
                                 ExprResult rhs) -> Instruction::Index
  {
    assert(lhs.index.has_value());
    assert(rhs.index.has_value());
    Instruction::Opcode op{Instruction::Opcode::load_literal};
    switch (type) {
    case Token::Type::eq:
      op = Instruction::Opcode::is_equal;
      break;
    case Token::Type::ne:
      op = Instruction::Opcode::is_not_equal;
      break;
    case Token::Type::less_than:
      op = Instruction::Opcode::is_less_than;
      break;
    case Token::Type::le:
      op = Instruction::Opcode::is_less_than_equal;
      break;
    case Token::Type::greater_than:
      op = Instruction::Opcode::is_greater_than;
      break;
    case Token::Type::ge:
      op = Instruction::Opcode::is_greater_than_equal;
      break;
    default:
      std::abort();
      break;
    }
    auto lhs_idx{emit_maybe_lvalue(instrs, lhs)};
    auto rhs_idx{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(op, lhs_idx, rhs_idx);
    return instrs.size() - 1;
  }

  static auto emit_to_number(Instructions& instrs, ExprResult num) -> Instruction::Index
  {
    assert(num.index.has_value());
    auto num_idx{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::to_number, num_idx);
    return instrs.size() - 1;
  }

  static auto emit_to_bool(Instructions& instrs, ExprResult num) -> Instruction::Index
  {
    assert(num.index.has_value());
    auto num_idx{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::to_bool, num_idx);
    return instrs.size() - 1;
  }

  static auto emit_negate(Instructions& instrs, ExprResult num) -> Instruction::Index
  {
    assert(num.index.has_value());
    auto num_idx{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::negate, num_idx);
    return instrs.size() - 1;
  }

  static auto emit_logical_not(Instructions& instrs, ExprResult num) -> Instruction::Index
  {
    assert(num.index.has_value());
    auto num_idx{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::logical_not, num_idx);
    return instrs.size() - 1;
  }

  static auto emit_branch_if_false(Instructions& instrs, ExprResult expr, Instruction::Index dest)
  {
    assert(expr.index.has_value());
    auto expr_idx{emit_maybe_lvalue(instrs, expr)};
    instrs.emplace_back(Instruction::Opcode::branch_if_false, expr_idx, dest);
    return instrs.size() - 1;
  }

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

  // NOLINTNEXTLINE
  auto parse_if_opt([[maybe_unused]] Instructions& instrs) -> ParseStatementResult
  {
    // TODO(mgrettondann): Implement
    return ParseStatementResult::none;
  }

  // NOLINTNEXTLINE
  auto parse_while_opt([[maybe_unused]] Instructions& instrs) -> ParseStatementResult
  {
    // TODO(mgrettondann): Implement
    return ParseStatementResult::none;
  }

  // NOLINTNEXTLINE
  auto parse_for_opt([[maybe_unused]] Instructions& instrs) -> ParseStatementResult
  {
    // TODO(mgrettondann): Implement
    return ParseStatementResult::none;
  }

  /** @brief Parse primary expressions, () and lvalues.
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @param  unary_type What expression class is this?
   * @return            Index of emitted expression, and updated expression type
   *
   * primary_expr : STRING
   *              | NUMBER
   *              | ERE
   *              | LPARENS expr RPARENS
   *              | NAME // LVALUE
   *              | NAME LSQUARE expr_list RSQUARE // rvalue
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
  auto parse_primary_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    if (unary_type == UnaryType::unary) {
      return {.type = expr_type};
    }

    auto const& tok{lexer_->peek(false)};
    ExprResult result{.type = expr_type};
    switch (tok.type()) {
    case Token::Type::integer:
      result.index = emit_load_literal(instrs, tok.integer());
      lexer_->chew(false);
      break;
    case Token::Type::floating:
      result.index = emit_load_literal(instrs, tok.floating());
      lexer_->chew(false);
      break;
    case Token::Type::string:
      result.index = emit_load_literal(instrs, tok.string());
      lexer_->chew(false);
      break;
    case Token::Type::ere:
      result.index = emit_load_literal(instrs, std::regex{tok.ere(), std::regex_constants::awk});
      lexer_->chew(false);
      break;
    case Token::Type::lparens: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
      auto const& close_tok{lexer_->peek(false)};
      if (close_tok == Token::Type::rparens) {
        lexer_->chew(false);
        result.index = expr.index;
        break;
      }

      if (close_tok == Token::Type::comma && expr_type == ExprType::maybe_expr) {
        // We now know we're a full expression, and this comma is part of a list - so we'll update
        // our types.  But not chew the comma - that will be done further up.
        // TODO(mgrettondann): Check we are at the top level.
        result.index = expr.index;
        result.type = ExprType::expr;
        break;
      }

      error(Msg::expected_rparens_at_end_of_expression, lexer_->location(), lexer_->peek(false));
    }
    case Token::Type::name: {
      std::string const var_name{tok.name()};
      lexer_->chew(false);
      result.is_lvalue = true;

      auto const& tok2{lexer_->peek(false)};
      if (tok2 != Token::Type::lsquare) {
        result.index = emit_variable(instrs, VariableName{var_name});
        break;
      }

      // TODO(mgrettondann): Implement
      std::abort();
    }
    default:
      break;
    }

    return result;
  }

  /** @brief Parse a field expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
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
  auto parse_field_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    auto const& tok{lexer_->peek(false)};
    if (unary_type == UnaryType::unary || tok != Token::Type::dollar) {
      return parse_primary_expr_opt(instrs, expr_type, unary_type);
    }

    lexer_->chew(false);
    ExprResult const field_id{parse_primary_expr_opt(instrs, expr_type, unary_type)};
    if (!field_id.index.has_value()) {
      error(Msg::expected_expr_after_dollar, lexer_->location(), lexer_->peek(false));
    }

    assert(field_id.index.has_value());
    return {.index = emit_field(instrs, field_id), .type = expr_type, .is_lvalue = true};
  }

  /** @brief Parse a post- increment/decrement expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
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
  auto parse_post_incr_decr_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    // Parse the primary expression
    ExprResult result{parse_field_expr_opt(instrs, expr_type, unary_type)};

    if (!result.is_lvalue || unary_type == UnaryType::unary || !result.index.has_value()) {
      return result;
    }

    auto const& token{lexer_->peek(false)};
    if (token != Token::Type::incr && token != Token::Type::decr) {
      return result;
    }

    bool const is_incr{token == Token::Type::incr};
    lexer_->chew(false);
    // Code sequence:
    //  x: load_lvalue result.index
    //  x + 1: load_lt 1
    //  x + 2: add x, x + 1 (or sub)
    //  x + 3: store_lvalue result.index
    //  result.index = x
    Instruction::Index lvalue_index{*(result.index)};
    result.index = emit_maybe_lvalue(instrs, result);
    result.is_lvalue = false;
    auto lit1_index{emit_load_literal(instrs, Integer{1})};
    auto mod_index{is_incr ? emit_add(instrs, result, {lit1_index})
                           : emit_sub(instrs, result, {lit1_index})};
    (void)emit_store_lvalue(instrs, {.index = lvalue_index, .is_lvalue = true}, {mod_index});

    return result;
  }

  /** @brief Parse a pre- increment/decrement expression
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
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
  auto parse_pre_incr_decr_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    auto token{lexer_->peek(false)};
    if (unary_type == UnaryType::unary ||
        (token != Token::Type::incr && token != Token::Type::decr)) {
      return parse_post_incr_decr_expr_opt(instrs, expr_type, unary_type);
    }

    bool const is_incr{token == Token::Type::incr};
    lexer_->chew(false);

    // Parse the primary expression
    ExprResult lvalue{parse_post_incr_decr_expr_opt(instrs, expr_type, unary_type)};

    if (!lvalue.is_lvalue || !lvalue.index.has_value()) {
      error(Msg::expected_lvalue_after_pre_incr_decr, lexer_->location(), token,
            lexer_->peek(false));
    }

    // Code sequence:
    //  x: load_lvalue result.index
    //  x + 1: load_lt 1
    //  x + 2: add x, x + 1 (or sub)
    //  x + 3: store_lvalue result.index
    //  result.index = x + 2
    Instruction::Index const val_index{emit_maybe_lvalue(instrs, lvalue)};
    auto lit1_index{emit_load_literal(instrs, Integer{1})};
    auto mod_index{is_incr ? emit_add(instrs, lvalue, {lit1_index})
                           : emit_sub(instrs, {val_index}, {lit1_index})};
    emit_store_lvalue(instrs, lvalue, {mod_index});
    lvalue.index = mod_index;
    lvalue.is_lvalue = false;
    return lvalue;
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
  auto parse_power_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult const lhs{parse_pre_incr_decr_expr_opt(instrs, expr_type, unary_type)};
    if (lexer_->peek(true) != Token::Type::power) {
      return lhs;
    }
    lexer_->chew(true);

    ExprResult const rhs{parse_power_expr_opt(instrs, expr_type, unary_type)};
    if (!rhs.index.has_value()) {
      error(Msg::expected_expr_after_power, lexer_->location(), lexer_->peek(false));
    }

    return {emit_power(instrs, lhs, rhs)};
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
  auto parse_unary_prefix_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
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
      return parse_power_expr_opt(instrs, expr_type, unary_type);
    }

    auto expr{parse_unary_prefix_expr_opt(instrs, expr_type, UnaryType::both)};
    if (!is_not && !is_sign_prefix) {
      return expr;
    }

    if (!expr.index.has_value()) {
      error(Msg::expected_expr_after_unary_prefix, lexer_->location(),
            is_not ? "!" : (negate ? "-" : "+"), lexer_->peek(false));  // NOLINT
    }

    if (is_not) {
      if (do_not) {
        return {.index = emit_logical_not(instrs, expr), .type = expr_type};
      }

      return {.index = emit_to_bool(instrs, expr), .type = expr_type};
    }

    assert(is_sign_prefix);
    if (negate) {
      return {.index = emit_negate(instrs, expr), .type = expr_type};
    }

    return {.index = emit_to_number(instrs, expr), .type = expr_type};
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
  auto parse_multiplicative_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_unary_prefix_expr_opt(instrs, expr_type, unary_type)};
    while (is_multiplicative_op(lexer_->peek(true).type())) {
      auto type{lexer_->peek(true).type()};
      lexer_->chew(true);
      ExprResult const rhs{parse_unary_prefix_expr_opt(instrs, expr_type, UnaryType::both)};
      if (type == Token::Type::multiply) {
        lhs.index = emit_multiply(instrs, lhs, rhs);
      }
      else if (type == Token::Type::divide) {
        lhs.index = emit_divide(instrs, lhs, rhs);
      }
      else {
        assert(type == Token::Type::modulo);
        lhs.index = emit_modulo(instrs, lhs, rhs);
      }
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
  auto parse_additive_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_multiplicative_expr_opt(instrs, expr_type, unary_type)};
    while (is_additive_op(lexer_->peek(true).type())) {
      auto type{lexer_->peek(true).type()};
      lexer_->chew(true);
      ExprResult const rhs{parse_multiplicative_expr_opt(instrs, expr_type, UnaryType::both)};
      if (type == Token::Type::add) {
        lhs.index = emit_add(instrs, lhs, rhs);
      }
      else {
        assert(type == Token::Type::subtract);
        lhs.index = emit_sub(instrs, lhs, rhs);
      }
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
  auto parse_concat_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_additive_expr_opt(instrs, expr_type, unary_type)};
    bool cont{true};
    while (cont) {
      // TODO(mgrettondann): make this hack more robust/reasonable!
      // We might want to parse the next token as a '/' so let's peek(true) now to populate the
      // next token.
      (void)lexer_->peek(true);
      auto rhs{parse_additive_expr_opt(instrs, expr_type, UnaryType::non_unary)};
      cont = rhs.index.has_value();
      if (cont) {
        lhs.index = emit_concat(instrs, lhs, rhs);
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
   * | concat_expr additive_expr(Non-unary)    | Both                 | Both                 |
   */
  auto parse_comparison_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult const lhs{parse_concat_expr_opt(instrs, expr_type, unary_type)};
    if (!lhs.index.has_value() || expr_type == ExprType::print_expr) {
      return lhs;
    }

    if (auto type{lexer_->peek(true).type()}; is_comparison_op(type)) {
      lexer_->chew(true);
      ExprResult const rhs{parse_concat_expr_opt(instrs, expr_type, UnaryType::both)};
      if (!rhs.index.has_value()) {
        error(Msg::expected_expr_after_comparison_op, lexer_->location(), type,
              lexer_->peek(false));
      }
      return {emit_comparison_op(instrs, type, lhs, rhs)};
    }

    return lhs;
  }

  /** @brief Parse an expression (of any type)
   *
   * @param  instrs     Where to emit code to
   * @param  expr_type  Expression type
   * @return            Index of emitted expression, and updated expression type
   *
   * The official grammar for awk classifies expressions into two main classes: print_expr or expr.
   * The print_expr class is a subset of expr - it basically excludes getline calls, and comparison
   * operators.
   *
   * Each of these two classes is further subdivided into unary ond non-unary.  The purpose of this
   * seems to be to exclude unary '+' and '-' from the right hand side of string concatenation.
   *
   * In the grammar comments before functions ignore these differences for simplicity of exposition.
   * The following table details the classifications.
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
  auto parse_expr_opt(Instructions& instrs, ExprType expr_type,
                      [[maybe_unused]] UnaryType unary_type) -> ExprResult
  {
    return parse_comparison_expr_opt(instrs, expr_type, UnaryType::both);
  }

  /** @brief Parse an expression list (of any type)
   *
   * @param instrs      Instructions to emit into
   * @param inserter_it Iterator to use to insert the expression indices into a location
   * @param list_type   What type of list are we processing?
   */
  void parse_expr_list_opt(Instructions& instrs, auto inserter_it, ExprListType list_type)
  {
    ExprListType const initial_list_type{list_type};
    std::size_t element_count{0};  // Number of elements
    while (true) {
      // Parse the expression
      auto result = parse_expr_opt(instrs, to_expr_type(list_type), UnaryType::both);
      if (!result.index.has_value()) {
        if (element_count != 0) {
          error(Msg::expected_expr_after_comma, lexer_->location(), lexer_->peek(false));
        }
        break;
      }

      // Insert index into list of expressions.
      *inserter_it = result;
      list_type = to_expr_list_type(result.type, list_type);
      ++element_count;

      // Chew the comma, and newline separators.
      if (lexer_->peek(false) != Token::Type::comma) {
        break;
      }
      lexer_->chew(false);
      parse_newline_opt();
    }

    // If on entry we didn't know what type of list this was and we now know it is a
    // multiple_expr_list we should ensure that we have a ')' to finish the list off with.
    if (initial_list_type == ExprListType::maybe_multiple_expr_list &&
        list_type == ExprListType::multiple_expr_list) {
      if (lexer_->peek(false) != Token::Type::rparens) {
        error(Msg::expected_rparens_at_end_of_list, lexer_->location(), lexer_->peek(false));
      }
      lexer_->chew(false);
    }

    if (list_type == ExprListType::multiple_expr_list && element_count == 1) {
      error(Msg::expected_two_exprs_in_list, lexer_->location(), lexer_->peek(false));
    }
  }

  // NOLINTNEXTLINE
  void parse_do_while([[maybe_unused]] Instructions& instrs)
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
  void parse_delete_statement([[maybe_unused]] Instructions& instrs)
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
  auto parse_redirection_opt(Instructions& instrs, ExprType expr_type) -> ExprResult
  {
    auto tok{lexer_->peek(false)};

    bool const is_open{tok == Token::Type::greater_than};
    bool const is_append{tok == Token::Type::append};
    bool const is_popen{tok == Token::Type::pipe};

    if (!is_open && !is_append && !is_popen) {
      return {.type = expr_type};
    }

    lexer_->chew(false);

    auto out_expr{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
    if (!out_expr.index.has_value()) {
      error(Msg::expected_expr_after_redirection, lexer_->location(), tok, lexer_->peek(false));
    }

    if (is_open) {
      return {emit_popen(instrs, out_expr),
              expr_type};  // NOLINT(bugprone-unchecked-optional-access)
    }

    Instruction::Index const opt{emit_load_literal(instrs, is_append ? INT64_C(1) : INT64_C(0))};
    return {emit_open(instrs, out_expr, {.index = opt}),
            expr_type};  // NOLINT(bugprone-unchecked-optional-access)
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
  void parse_print_statement(Instructions& instrs)
  {
    auto const& tok = lexer_->peek(false);
    assert(tok == Token::Type::print || tok == Token::Type::printf);

    bool const is_printf{tok == Token::Type::printf};

    lexer_->chew(false);

    // So the problem we have here is that the token '(' is valid is the first token in
    // print_expr_list_opt.  So if that is the next token we have no idea whether we are parsing a
    // print_expr_list or a multiple_expr_list.  Hence the maybe_multiple_expr_list where we leave
    // it to the expression parser to work it out.
    bool const maybe_multiple_expr_list{lexer_->peek(false) == Token::Type::lparens};
    std::vector<ExprResult> indices;
    parse_expr_list_opt(instrs, std::back_inserter(indices),
                        maybe_multiple_expr_list ? ExprListType::maybe_multiple_expr_list
                                                 : ExprListType::print_expr_list);

    if (is_printf && indices.empty()) {
      error(Msg::expected_list_to_printf, lexer_->location(), lexer_->peek(false));
    }

    if (!is_printf && indices.empty()) {
      // We have a plain print.  We want to print $0.
      auto const lit_expr{emit_load_literal(instrs, Integer{0})};
      auto const field_expr{emit_field(instrs, {.index = lit_expr})};
      indices.emplace_back(ExprResult{field_expr, ExprType::print_expr, true});
    }

    // Now get the redirection.  If we don't have a redirection we will output to standard out.
    ExprResult redir{parse_redirection_opt(instrs, ExprType::expr)};
    if (!redir.index.has_value()) {
      int const fd{STDOUT_FILENO};
      redir.index = emit_load_literal(instrs, FileDescriptor{fd});
    }

    Instruction::Index fs{0};
    if (!is_printf && indices.size() > 1) {
      fs = emit_variable(instrs, VariableName{"OFS"});
      fs = emit_maybe_lvalue(instrs, {.index = fs, .is_lvalue = true});
    }

    if (!is_printf) {
      bool first{true};
      for (auto const idx : indices) {
        if (first) {
          first = false;
        }
        else {
          emit_print(instrs, {.index = fs}, redir);
        }
        emit_print(instrs, idx, redir);
      }
      emit_print(instrs, {.index = emit_variable(instrs, VariableName{"ORS"}), .is_lvalue = true},
                 redir);
    }
    else {
      Instruction::Index const pp{emit_open_param_pack(instrs)};
      for (auto const idx : indices) {
        emit_push_param(instrs, {pp}, idx);
      }
      emit_printf(instrs, {emit_close_param_pack(instrs, pp)}, redir);
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
  auto parse_simple_statement_opt(Instructions& instrs) -> bool
  {
    auto const& tok{lexer_->peek(false)};

    if (tok == Token::Type::delete_) {
      parse_delete_statement(instrs);
      return true;
    }

    if (tok == Token::Type::print || tok == Token::Type::printf) {
      parse_print_statement(instrs);
      return true;
    }

    return parse_expr_opt(instrs, ExprType::expr, UnaryType::both).index.has_value();
  }

  auto parse_terminatable_statement_opt(Instructions& instrs) -> ParseStatementResult
  {
    auto const& tok{lexer_->peek(false)};
    switch (tok.type()) {
    case Token::Type::break_:
      emit_break(instrs);
      lexer_->chew(false);
      break;
    case Token::Type::continue_:
      emit_continue(instrs);
      lexer_->chew(false);
      break;
    case Token::Type::next:
      emit_next(instrs);
      lexer_->chew(false);
      break;
    case Token::Type::exit: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
      if (!expr.index.has_value()) {
        expr.index = emit_load_literal(instrs, INT64_C(0));
      }

      emit_exit(instrs, expr);
      break;
    }
    case Token::Type::return_: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
      if (!expr.index.has_value()) {
        expr.index = emit_load_literal(instrs, INT64_C(0));
      }
      emit_return(instrs, expr);
      break;
    }
    case Token::Type::do_:
      parse_do_while(instrs);
      break;
    default:
      if (!parse_simple_statement_opt(instrs)) {
        return ParseStatementResult::none;
      }
      break;
    }

    if (lexer_->peek(false) == Token::Type::semicolon ||
        lexer_->peek(false) == Token::Type::newline) {
      lexer_->chew(false);
      return ParseStatementResult::terminated;
    }

    return ParseStatementResult::unterminated;
  }

  /** @brief Parse an optional statement
   *
   * @param  instrs Instructions to write code into
   * @return        Flag indicating whether anything was parsed, and if so whether it was
   *                terminated.
   */
  auto parse_statement_opt(Instructions& instrs) -> ParseStatementResult
  {
    auto const& tok{lexer_->peek(false)};
    if (tok == Token::Type::if_) {
      return parse_if_opt(instrs);
    }
    if (tok == Token::Type::while_) {
      return parse_while_opt(instrs);
    }
    if (tok == Token::Type::for_) {
      return parse_for_opt(instrs);
    }
    if (tok == Token::Type::semicolon) {
      lexer_->chew(false);
      parse_newline_opt();
      return ParseStatementResult::terminated;
    }

    auto res{parse_terminatable_statement_opt(instrs)};
    return res;
  }

  /** \brief  Parse a statement list.
   *
   * @param  instrs Instructions block to put instructions into.
   *
   * statement_list_opt : statement terminator statement_list_opt
   *                    | statement
   *                    | *empty*
   */
  void parse_statement_list_opt(Instructions& instrs)
  {
    bool cont{true};
    while (cont) {
      auto res{parse_statement_opt(instrs)};
      cont = (res == ParseStatementResult::terminated);
    }
  }

  /** \brief         Parse an optional action, generating code at the end of the instructions
   * given. \param  instrs Instructions to append to. \return        True iff we parsed an action.
   *
   * action_opt : LBRACE newline_opt statement_list_opt RBRACE
   *            | empty
   */
  auto parse_action_opt(Instructions& instrs) -> bool
  {
    if (lexer_->peek(false) != Token::Type::lbrace) {
      return false;
    }

    lexer_->chew(false);
    parse_newline_opt();
    parse_statement_list_opt(instrs);

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
  auto parse_action(Instructions& instrs)
  {
    if (!parse_action_opt(instrs)) {
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

  // Returns index of comparison instruction if we emitted the pattern sequence.
  auto parse_normal_pattern_opt(Instructions& instrs)
    -> std::optional<Instruction::Index>  // NOLINT - temporary until implemented
  {
    if (ExprResult const pat1{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
        pat1.index.has_value()) {
      auto branch{emit_branch_if_false(instrs, pat1, 0)};
      // TODO(mgrettondann): Handle pattern ranges
      return branch;
    }

    return std::nullopt;
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
      parse_action(program_.per_record());
    }
    else if (tok == Token::Type::begin) {
      lexer_->chew(false);
      parse_action(program_.begin());
    }
    else if (tok == Token::Type::end) {
      lexer_->chew(false);
      parse_action(program_.end());
    }
    else if (tok == Token::Type::function) {
      Instructions& fn{parse_function_def()};
      parse_action(fn);
    }
    else if (auto result{parse_normal_pattern_opt(program_.per_record())}; result.has_value()) {
      auto& instrs{program_.per_record()};
      if (!parse_action_opt(instrs)) {
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
        auto const lit{emit_load_literal(instrs, Integer{0})};
        auto const field{emit_field(instrs, {lit})};
        auto const fileno{emit_load_literal(instrs, FileDescriptor(fd))};
        (void)emit_print(instrs, {.index = field, .is_lvalue = true}, {fileno});
        auto const rs{emit_variable(instrs, VariableName{"RS"})};
        (void)emit_print(instrs, {.index = rs, .is_lvalue = true}, {fileno});
      }

      // Set the target of the branch_if_false to the end.
      instrs[*result].op2(instrs.size());
    }
    else {
      return false;
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
    parse_item_list_maybe_unterminated();
    if (lexer_->peek(false) != Token::Type::eof) {
      error(Msg::failed_to_read_whole_program, lexer_->location(), lexer_->peek(false));
    }
  }

  auto program() -> ParsedProgram& { return program_; }

private:
  std::unique_ptr<Lexer> lexer_;
  ParsedProgram program_;
};
}  // namespace GD::Awk::Details

auto GD::Awk::parse(std::unique_ptr<Lexer>&& lexer) -> ParsedProgram
{
  Details::ParseState parser{std::move(lexer)};
  parser.parse_program();
  return std::move(parser.program());
}
