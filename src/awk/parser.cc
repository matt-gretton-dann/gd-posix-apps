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
  struct OneResult
  {
    Instruction::Index index{Instruction::illegal_index};
    bool is_lvalue{false};
  };

  using Indices = std::vector<OneResult>;

  ExprResult() = default;
  explicit ExprResult(Instruction::Index index) : indices_(1, {index, false}) {}
  explicit ExprResult(OneResult const& r) : indices_(1, r) {}
  ExprResult(Instruction::Index index, bool is_lvalue) : indices_(1, {index, is_lvalue}) {}

  ~ExprResult() = default;
  ExprResult(ExprResult const&) = default;
  ExprResult(ExprResult&&) noexcept = default;
  auto operator=(ExprResult const&) -> ExprResult& = default;
  auto operator=(ExprResult&&) noexcept -> ExprResult& = default;

  [[nodiscard]] auto has_one_value() const noexcept -> bool { return indices_.size() == 1; }
  [[nodiscard]] auto has_many_values() const noexcept -> bool { return indices_.size() > 1; }
  [[nodiscard]] auto has_no_values() const noexcept -> bool { return indices_.empty(); }

  [[nodiscard]] auto is_lvalue() const noexcept -> bool
  {
    return !indices_.empty() && indices_.front().is_lvalue;
  }

  [[nodiscard]] auto index() const noexcept -> Instruction::Index
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
  static auto emit_load_literal(Instructions& instrs, Integer value) -> ExprResult
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, value);
    return ExprResult{instrs.size() - 1};
  }

  /** @brief emit a load literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param fd      Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, FileDescriptor fd) -> ExprResult
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, fd);
    return ExprResult{instrs.size() - 1};
  }

  /** @brief emit a load literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, Floating value) -> ExprResult
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, value);
    return ExprResult{instrs.size() - 1};
  }

  /** @brief emit a load literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, std::string const& value) -> ExprResult
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, value);
    return ExprResult{instrs.size() - 1};
  }

  /** @brief emit a load literal regex instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_load_literal(Instructions& instrs, std::regex const& re) -> ExprResult
  {
    instrs.emplace_back(Instruction::Opcode::load_literal, re);
    return ExprResult{instrs.size() - 1};
  }

  /** @brief emit a field instruction
   *
   * @param instrs  Instructions to emit into
   * @param expr    Offset of expression containing field expression
   * @return        Index of calculated field-id.
   */
  static auto emit_field(Instructions& instrs, ExprResult const& op1) -> ExprResult
  {
    ExprResult const idx{emit_maybe_lvalue(instrs, op1)};
    instrs.emplace_back(Instruction::Opcode::field, idx.index());
    return ExprResult{instrs.size() - 1, true};
  }

  /** @brief emit a variable instruction
   *
   * @param instrs  Instructions to emit into
   * @param vn      Variable name
   * @return        Index of emitted instruction
   */
  static auto emit_variable(Instructions& instrs, VariableName const& vn) -> ExprResult
  {
    instrs.emplace_back(Instruction::Opcode::variable, vn);
    return ExprResult{instrs.size() - 1, true};
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

  static auto emit_exit([[maybe_unused]] Instructions& instrs,
                        [[maybe_unused]] ExprResult const& op1) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_return([[maybe_unused]] Instructions& instrs,
                          [[maybe_unused]] ExprResult const& op1) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_open([[maybe_unused]] Instructions& instrs,
                        [[maybe_unused]] ExprResult const& op1,
                        [[maybe_unused]] ExprResult const& op2) -> ExprResult
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_popen([[maybe_unused]] Instructions& instrs,
                         [[maybe_unused]] ExprResult const& op1) -> ExprResult
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_maybe_lvalue(Instructions& instrs, ExprResult const& expr) -> ExprResult
  {
    assert(expr.has_one_value());
    if (!expr.is_lvalue()) {
      return expr;
    }

    instrs.emplace_back(Instruction::Opcode::load_lvalue, expr.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_store_lvalue(Instructions& instrs, ExprResult const& lvalue,
                                ExprResult const& expr)
  {
    assert(lvalue.has_one_value());
    assert(lvalue.is_lvalue());
    assert(expr.has_one_value());
    ExprResult const expr1{emit_maybe_lvalue(instrs, expr)};

    instrs.emplace_back(Instruction::Opcode::store_lvalue, lvalue.index(), expr1.index());
  }

  static auto emit_print(Instructions& instrs, ExprResult const& value, ExprResult const& stream)
  {
    ExprResult const value1{emit_maybe_lvalue(instrs, value)};
    ExprResult const stream1{emit_maybe_lvalue(instrs, stream)};
    instrs.emplace_back(Instruction::Opcode::print, value1.index(), stream1.index());
  }

  // NOLINTNEXTLINE
  static auto emit_printf(Instructions& instrs, ExprResult const& param_pack,
                          ExprResult const& stream)
  {
    assert(param_pack.has_one_value());
    assert(instrs[param_pack.index()].opcode() == Instruction::Opcode::open_param_pack);
    assert(!param_pack.is_lvalue());
    auto const stream1{emit_maybe_lvalue(instrs, stream)};
    instrs.emplace_back(Instruction::Opcode::printf, param_pack.index(), stream1.index());
  }

  static auto emit_open_param_pack(Instructions& instrs) -> ExprResult
  {
    instrs.emplace_back(Instruction::Opcode::open_param_pack);
    return ExprResult{instrs.size() - 1};
  }

  // NOLINTNEXTLINE
  static auto emit_push_param(Instructions& instrs, ExprResult const& param_pack,
                              ExprResult const& expr) -> ExprResult
  {
    assert(param_pack.has_one_value());
    assert(!param_pack.is_lvalue());
    assert(instrs[param_pack.index()].opcode() == Instruction::Opcode::open_param_pack);
    ExprResult const expr1{emit_maybe_lvalue(instrs, expr)};
    instrs.emplace_back(Instruction::Opcode::push_param, param_pack.index(), expr1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_close_param_pack(Instructions& instrs, ExprResult const& param_pack)
    -> ExprResult
  {
    assert(instrs[param_pack.index()].opcode() == Instruction::Opcode::open_param_pack);
    instrs.emplace_back(Instruction::Opcode::close_param_pack, param_pack.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto to_binary_op_opcode(Token::Type type) -> Instruction::Opcode
  {
    switch (type) {
    case Token::Type::add:
      return Instruction::Opcode::add;
    case Token::Type::subtract:
      return Instruction::Opcode::sub;
    case Token::Type::multiply:
      return Instruction::Opcode::multiply;
    case Token::Type::divide:
      return Instruction::Opcode::divide;
    case Token::Type::power:
      return Instruction::Opcode::power;
    case Token::Type::modulo:
      return Instruction::Opcode::modulo;
    default:
      std::abort();
    }
  }

  static auto emit_binary_op(Instructions& instrs, Token::Type binary_op, ExprResult const& lhs,
                             ExprResult const& rhs) -> ExprResult
  {
    assert(is_additive_op(binary_op) || is_multiplicative_op(binary_op) ||
           binary_op == Token::Type::power);
    assert(lhs.has_one_value());
    assert(rhs.has_one_value());
    ExprResult const lhs1{emit_maybe_lvalue(instrs, lhs)};
    ExprResult const rhs1{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(to_binary_op_opcode(binary_op), lhs1.index(), rhs1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_concat(Instructions& instrs, ExprResult const& lhs, ExprResult const& rhs)
    -> ExprResult
  {
    assert(lhs.has_one_value());
    assert(rhs.has_one_value());
    ExprResult const lhs1{emit_maybe_lvalue(instrs, lhs)};
    ExprResult const rhs1{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::concat, lhs1.index(), rhs1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_comparison_op(Instructions& instrs, Token::Type type, ExprResult const& lhs,
                                 ExprResult const& rhs) -> ExprResult
  {
    assert(lhs.has_one_value());
    assert(rhs.has_one_value());
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
    ExprResult const lhs1{emit_maybe_lvalue(instrs, lhs)};
    ExprResult const rhs1{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(op, lhs1.index(), rhs1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_re_match_op(Instructions& instrs, ExprResult const& lhs, ExprResult const& rhs)
    -> ExprResult
  {
    assert(lhs.has_one_value());
    assert(rhs.has_one_value());
    ExprResult const lhs1{emit_maybe_lvalue(instrs, lhs)};
    ExprResult const rhs1{emit_maybe_lvalue(instrs, rhs)};
    instrs.emplace_back(Instruction::Opcode::re_match, lhs1.index(), rhs1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_to_number(Instructions& instrs, ExprResult const& num) -> ExprResult
  {
    assert(num.has_one_value());
    ExprResult const num1{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::to_number, num1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_to_bool(Instructions& instrs, ExprResult const& num) -> ExprResult
  {
    assert(num.has_one_value());
    ExprResult const num1{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::to_bool, num1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_negate(Instructions& instrs, ExprResult const& num) -> ExprResult
  {
    assert(num.has_one_value());
    ExprResult const num1{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::negate, num1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_logical_not(Instructions& instrs, ExprResult const& num) -> ExprResult
  {
    assert(num.has_one_value());
    ExprResult const num1{emit_maybe_lvalue(instrs, num)};
    instrs.emplace_back(Instruction::Opcode::logical_not, num1.index());
    return ExprResult{instrs.size() - 1};
  }

  static auto emit_branch_if_false(Instructions& instrs, ExprResult const& expr,
                                   Instruction::Index dest) -> Instruction::Index
  {
    assert(expr.has_one_value());
    ExprResult const expr1{emit_maybe_lvalue(instrs, expr)};
    instrs.emplace_back(Instruction::Opcode::branch_if_false, expr1.index(), dest);
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
  auto parse_primary_expr_opt(Instructions& instrs, UnaryType unary_type) -> ExprResult
  {
    if (unary_type == UnaryType::unary) {
      return ExprResult{};
    }

    auto const& tok{lexer_->peek(false)};
    ExprResult result;
    switch (tok.type()) {
    case Token::Type::integer:
      result = emit_load_literal(instrs, tok.integer());
      lexer_->chew(false);
      break;
    case Token::Type::floating:
      result = emit_load_literal(instrs, tok.floating());
      lexer_->chew(false);
      break;
    case Token::Type::string:
      result = emit_load_literal(instrs, tok.string());
      lexer_->chew(false);
      break;
    case Token::Type::ere:
      result = emit_load_literal(instrs, std::regex{tok.ere(), std::regex_constants::awk});
      lexer_->chew(false);
      break;
    case Token::Type::lparens: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
      auto const& close_tok{lexer_->peek(false)};
      result = expr;

      if (close_tok == Token::Type::comma) {
        // We assume that we are in a multiple_expr_list here, hand over sorting the result to
        // parse_multiple_expr_list_rest here.
        lexer_->chew(false);
        parse_multiple_expr_list_rest(instrs, result.back_inserter());
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

      auto const& tok2{lexer_->peek(false)};
      if (tok2 != Token::Type::lsquare) {
        result = emit_variable(instrs, VariableName{var_name});
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
  auto parse_field_expr_opt(Instructions& instrs, UnaryType unary_type) -> ExprResult
  {
    auto const& tok{lexer_->peek(false)};
    if (unary_type == UnaryType::unary || tok != Token::Type::dollar) {
      return parse_primary_expr_opt(instrs, unary_type);
    }

    lexer_->chew(false);
    ExprResult const field_id{parse_primary_expr_opt(instrs, unary_type)};
    if (!field_id.has_one_value()) {
      error(Msg::expected_expr_after_dollar, lexer_->location(), lexer_->peek(false));
    }

    assert(field_id.has_one_value());
    return emit_field(instrs, field_id);
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
  auto parse_post_incr_decr_expr_opt(Instructions& instrs, UnaryType unary_type) -> ExprResult
  {
    // Parse the primary expression
    ExprResult result{parse_field_expr_opt(instrs, unary_type)};

    if (!result.is_lvalue() || unary_type == UnaryType::unary || !result.has_one_value()) {
      return result;
    }

    auto type{lexer_->peek(false).type()};
    if (type != Token::Type::incr && type != Token::Type::decr) {
      return result;
    }

    bool const is_incr{type == Token::Type::incr};
    lexer_->chew(false);
    // Code sequence:
    //  x: load_lvalue result.index
    //  x + 1: load_lt 1
    //  x + 2: add x, x + 1 (or sub)
    //  x + 3: store_lvalue result.index
    //  result.index = x
    Instruction::Index const lvalue_index{result.index()};
    result = emit_maybe_lvalue(instrs, result);
    ExprResult const lit1_index{emit_load_literal(instrs, Integer{1})};
    auto mod_index{emit_binary_op(instrs, is_incr ? Token::Type::add : Token::Type::subtract,
                                  result, lit1_index)};
    emit_store_lvalue(instrs, ExprResult{lvalue_index, true}, ExprResult{mod_index});

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
  auto parse_pre_incr_decr_expr_opt(Instructions& instrs, UnaryType unary_type) -> ExprResult
  {
    auto token{lexer_->peek(false)};
    if (unary_type == UnaryType::unary ||
        (token != Token::Type::incr && token != Token::Type::decr)) {
      return parse_post_incr_decr_expr_opt(instrs, unary_type);
    }

    bool const is_incr{token == Token::Type::incr};
    lexer_->chew(false);

    // Parse the primary expression
    ExprResult const lvalue{parse_post_incr_decr_expr_opt(instrs, unary_type)};

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
    ExprResult const lit1{emit_load_literal(instrs, Integer{1})};
    auto mod{
      emit_binary_op(instrs, is_incr ? Token::Type::add : Token::Type::subtract, lvalue, lit1)};
    emit_store_lvalue(instrs, lvalue, mod);
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
  auto parse_power_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_pre_incr_decr_expr_opt(instrs, unary_type)};
    if (lexer_->peek(true) != Token::Type::power) {
      return lhs;
    }
    lexer_->chew(true);

    ExprResult const rhs{parse_power_expr_opt(instrs, expr_type, unary_type)};
    if (!rhs.has_one_value()) {
      error(Msg::expected_expr_after_power, lexer_->location(), lexer_->peek(false));
    }

    return ExprResult{emit_binary_op(instrs, Token::Type::power, lhs, rhs)};
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

    if (!expr.has_one_value()) {
      error(Msg::expected_expr_after_unary_prefix, lexer_->location(),
            is_not ? "!" : (negate ? "-" : "+"), lexer_->peek(false));  // NOLINT
    }

    if (is_not) {
      if (do_not) {
        return emit_logical_not(instrs, expr);
      }

      return emit_to_bool(instrs, expr);
    }

    assert(is_sign_prefix);
    if (negate) {
      return emit_negate(instrs, expr);
    }

    return emit_to_number(instrs, expr);
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
    if (!lhs.has_one_value()) {
      return lhs;
    }
    lhs = emit_maybe_lvalue(instrs, lhs);
    while (is_multiplicative_op(lexer_->peek(true).type())) {
      auto type{lexer_->peek(true).type()};
      lexer_->chew(true);
      ExprResult const rhs{parse_unary_prefix_expr_opt(instrs, expr_type, UnaryType::both)};
      lhs = emit_binary_op(instrs, type, lhs, rhs);
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
    if (!lhs.has_one_value()) {
      return lhs;
    }

    lhs = emit_maybe_lvalue(instrs, lhs);
    while (is_additive_op(lexer_->peek(true).type())) {
      auto type{lexer_->peek(true).type()};
      lexer_->chew(true);
      ExprResult const rhs{parse_multiplicative_expr_opt(instrs, expr_type, UnaryType::both)};
      lhs = emit_binary_op(instrs, type, lhs, rhs);
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
    if (!lhs.has_one_value()) {
      return lhs;
    }

    lhs = emit_maybe_lvalue(instrs, lhs);

    bool cont{true};
    while (cont) {
      // TODO(mgrettondann): make this hack more robust/reasonable!
      // We might want to parse the next token as a '/' so let's peek(true) now to populate the
      // next token.
      (void)lexer_->peek(true);
      auto rhs{parse_additive_expr_opt(instrs, expr_type, UnaryType::non_unary)};
      cont = rhs.has_one_value();
      if (cont) {
        lhs = emit_concat(instrs, lhs, rhs);
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
  auto parse_comparison_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_concat_expr_opt(instrs, expr_type, unary_type)};
    if (!lhs.has_one_value() || expr_type == ExprType::print_expr) {
      return lhs;
    }
    if (auto type{lexer_->peek(true).type()}; is_comparison_op(type)) {
      lexer_->chew(true);
      ExprResult const rhs{parse_concat_expr_opt(instrs, expr_type, UnaryType::both)};
      if (!rhs.has_one_value()) {
        error(Msg::expected_expr_after_comparison_op, lexer_->location(), type,
              lexer_->peek(false));
      }
      return ExprResult{emit_comparison_op(instrs, type, lhs, rhs)};
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
  auto parse_re_match_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_comparison_expr_opt(instrs, expr_type, unary_type)};
    if (!lhs.has_one_value()) {
      return lhs;
    }
    if (auto type{lexer_->peek(true).type()}; is_re_match_op(type)) {
      lexer_->chew(true);
      ExprResult const rhs{parse_comparison_expr_opt(instrs, expr_type, UnaryType::both)};
      if (!rhs.has_one_value()) {
        error(Msg::expected_expr_after_re_match_op, lexer_->location(), type, lexer_->peek(false));
      }
      auto re_match{emit_re_match_op(instrs, lhs, rhs)};
      if (type == Token::Type::no_match) {
        re_match = emit_logical_not(instrs, ExprResult{re_match});
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
  auto parse_in_array_expr_opt(Instructions& instrs, ExprType expr_type, UnaryType unary_type)
    -> ExprResult
  {
    ExprResult lhs{parse_re_match_expr_opt(instrs, expr_type, unary_type)};
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
    return parse_in_array_expr_opt(instrs, expr_type, UnaryType::both);
  }

  /** @brief Parse the second & subsequent elements of a multiple_expr_list.
   *
   * @param instrs      Instructions to emit into
   * @param inserter_it Iterator to use to insert the expression indices into a location
   */
  void parse_multiple_expr_list_rest(Instructions& instrs, auto inserter_it)
  {
    while (true) {
      // Parse the expression
      auto result = parse_expr_opt(instrs, ExprType::expr, UnaryType::both);
      if (result.has_many_values()) {
        error(Msg::cannot_nest_multiple_expr_lists, lexer_->location());
      }

      if (result.has_no_values()) {
        error(Msg::expected_expr_after_comma, lexer_->location(), lexer_->peek(false));
      }

      // Insert index into list of expressions.
      *inserter_it++ = {result.index(), result.is_lvalue()};

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
  void parse_expr_list_opt(Instructions& instrs, auto inserter_it, ExprListType list_type)
  {
    std::size_t element_count{0};  // Number of elements
    while (true) {
      // Parse the expression
      auto result = parse_expr_opt(instrs, to_expr_type(list_type), UnaryType::both);
      if (result.has_many_values()) {
        // This turned out to be a multiple_expr_list - so we just lift the results up a level, and
        // return.
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
      *inserter_it++ = {result.index(), result.is_lvalue()};
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
  auto parse_redirection_opt(Instructions& instrs) -> ExprResult
  {
    auto tok{lexer_->peek(false)};

    bool const is_open{tok == Token::Type::greater_than};
    bool const is_append{tok == Token::Type::append};
    bool const is_popen{tok == Token::Type::pipe};

    if (!is_open && !is_append && !is_popen) {
      return ExprResult{};
    }

    lexer_->chew(false);

    auto out_expr{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
    if (!out_expr.has_one_value()) {
      error(Msg::expected_expr_after_redirection, lexer_->location(), tok, lexer_->peek(false));
    }

    if (is_open) {
      return emit_popen(instrs, out_expr);
    }

    ExprResult const opt{emit_load_literal(instrs, is_append ? INT64_C(1) : INT64_C(0))};
    return emit_open(instrs, out_expr, opt);
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
    ExprResult indices;
    parse_expr_list_opt(instrs, indices.back_inserter(), ExprListType::print_expr_list);

    if (is_printf && indices.has_no_values()) {
      error(Msg::expected_list_to_printf, lexer_->location(), lexer_->peek(false));
    }

    if (!is_printf && indices.has_no_values()) {
      // We have a plain print.  We want to print $0.
      auto const lit_expr{emit_load_literal(instrs, Integer{0})};
      auto field{emit_field(instrs, lit_expr)};
      *indices.back_inserter()++ = {field.index(), field.is_lvalue()};
    }

    // Now get the redirection.  If we don't have a redirection we will output to standard out.
    ExprResult redir{parse_redirection_opt(instrs)};
    if (!redir.has_one_value()) {
      int const fd{STDOUT_FILENO};
      redir = emit_load_literal(instrs, FileDescriptor{fd});
    }

    ExprResult fs;
    if (!is_printf && indices.has_many_values()) {
      fs = emit_variable(instrs, VariableName{"OFS"});
    }

    if (!is_printf) {
      bool first{true};
      for (auto const idx : indices) {
        if (first) {
          first = false;
        }
        else {
          emit_print(instrs, fs, redir);
        }
        emit_print(instrs, ExprResult{idx}, redir);
      }
      emit_print(instrs, emit_variable(instrs, VariableName{"ORS"}), redir);
    }
    else {
      auto const pp{emit_open_param_pack(instrs)};
      for (auto const idx : indices) {
        emit_push_param(instrs, pp, ExprResult{idx});
      }
      emit_printf(instrs, ExprResult{emit_close_param_pack(instrs, pp)}, redir);
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

    return parse_expr_opt(instrs, ExprType::expr, UnaryType::both).has_one_value();
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
      if (!expr.has_one_value()) {
        expr = emit_load_literal(instrs, INT64_C(0));
      }

      emit_exit(instrs, expr);
      break;
    }
    case Token::Type::return_: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(instrs, ExprType::expr, UnaryType::both)};
      if (!expr.has_one_value()) {
        expr = emit_load_literal(instrs, INT64_C(0));
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
        pat1.has_one_value()) {
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
        auto const field{emit_field(instrs, lit)};
        auto const fileno{emit_load_literal(instrs, FileDescriptor(fd))};
        emit_print(instrs, field, fileno);
        auto const rs{emit_variable(instrs, VariableName{"RS"})};
        emit_print(instrs, rs, fileno);
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
