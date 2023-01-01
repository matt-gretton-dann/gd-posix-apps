/** \file   parser.cc
 *  \brief  awk Parser program
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "awk-messages.hh"

#include <iostream>
#include <memory>
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
class ParseState
{
  using ExprIndex = std::optional<Instruction::Index>;

public:
  explicit ParseState(std::unique_ptr<Lexer>&& lexer) : lexer_(std::move(lexer)) {}

  /** @brief emit a literal instruction
   *
   * @param instrs  Instructions to emit into
   * @param value   Literal to emit
   * @return        Index of emitted literal.
   */
  static auto emit_lit(Instructions& instrs, std::int64_t value) -> Instruction::Index
  {
    instrs.emplace_back(Instruction::Opcode::load_literal_int, value);
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

  static auto emit_exit([[maybe_unused]] Instructions& instrs,
                        [[maybe_unused]] Instruction::Index op1) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_return([[maybe_unused]] Instructions& instrs,
                          [[maybe_unused]] Instruction::Index op1) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_open([[maybe_unused]] Instructions& instrs,
                        [[maybe_unused]] Instruction::Index op1,
                        [[maybe_unused]] Instruction::Index op2) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_popen([[maybe_unused]] Instructions& instrs,
                         [[maybe_unused]] Instruction::Index op1) -> Instruction::Index
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_print([[maybe_unused]] Instructions& instrs,
                         [[maybe_unused]] std::vector<ExprIndex> const& op1,
                         [[maybe_unused]] Instruction::Index op2) -> ExprIndex
  {
    // TODO(mgrettondann): Implement
    std::abort();
  }

  static auto emit_printf([[maybe_unused]] Instructions& instrs,
                          [[maybe_unused]] std::vector<ExprIndex> const& op1,
                          [[maybe_unused]] Instruction::Index op2) -> ExprIndex
  {
    // TODO(mgrettondann): Implement
    std::abort();
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
   * | number                              | Both                 | Non-unary            |
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
  // NOLINTNEXTLINE
  auto parse_expr_opt([[maybe_unused]] Instructions& instrs, [[maybe_unused]] ExprType expr_type)
    -> std::pair<ExprIndex, ExprType>
  {
    // TODO(mgrettondann): Implement
    return std::make_pair(std::nullopt, expr_type);
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
      auto [expr_idx, expr_type] = parse_expr_opt(instrs, to_expr_type(list_type));
      if (!expr_idx.has_value()) {
        if (element_count != 0) {
          error(Msg::expected_expr_after_comma, lexer_->location(), lexer_->peek(false));
        }
        break;
      }

      // Insert index into list of expressions.
      *inserter_it = expr_idx;
      list_type = to_expr_list_type(expr_type, list_type);
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
  auto parse_redirection_opt(Instructions& instrs) -> ExprIndex
  {
    auto tok{lexer_->peek(false)};

    bool const is_open{tok == Token::Type::greater_than};
    bool const is_append{tok == Token::Type::append};
    bool const is_popen{tok == Token::Type::pipe};

    if (!is_open && !is_append && !is_popen) {
      return std::nullopt;
    }

    lexer_->chew(false);

    [[maybe_unused]] auto [out_expr, expr_type] = parse_expr_opt(instrs, ExprType::expr);
    if (!out_expr.has_value()) {
      error(Msg::expected_expr_after_redirection, lexer_->location(), tok, lexer_->peek(false));
    }

    if (is_open) {
      return emit_popen(instrs, *out_expr);  // NOLINT(bugprone-unchecked-optional-access)
    }

    Instruction::Index const opt{emit_lit(instrs, is_append ? 1 : 0)};
    return emit_open(instrs, *out_expr, opt);  // NOLINT(bugprone-unchecked-optional-access)
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
    std::vector<ExprIndex> indices;
    parse_expr_list_opt(instrs, std::back_inserter(indices),
                        maybe_multiple_expr_list ? ExprListType::maybe_multiple_expr_list
                                                 : ExprListType::print_expr_list);

    if (is_printf && indices.empty()) {
      error(Msg::expected_list_to_printf, lexer_->location(), lexer_->peek(false));
    }

    if (!is_printf && indices.empty()) {
      // TODO(mgrettondann): Implement generation of $0.
      std::abort();
    }

    // Now get the redirection.  If we don't have a redirection we will output to standard out.
    ExprIndex redir{parse_redirection_opt(instrs)};
    if (!redir.has_value()) {
      redir = emit_lit(instrs, STDOUT_FILENO);
    }

    if (is_printf) {
      emit_printf(instrs, indices, *redir);
    }
    else {
      emit_print(instrs, indices, *redir);
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

    return parse_expr_opt(instrs, ExprType::expr).first.has_value();
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
      auto expr{parse_expr_opt(instrs, ExprType::expr).first};
      if (!expr.has_value()) {
        expr = emit_lit(instrs, 0);
      }
      assert(expr.has_value());

      emit_exit(instrs, *expr);
      break;
    }
    case Token::Type::return_: {
      lexer_->chew(false);
      auto expr{parse_expr_opt(instrs, ExprType::expr).first};
      if (!expr.has_value()) {
        expr = emit_lit(instrs, 0);
      }
      emit_return(instrs, *expr);
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
    if (res == ParseStatementResult::unterminated) {
      error(Msg::expected_terminator, lexer_->location(), lexer_->peek(false));
    }
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

  auto parse_normal_pattern_opt() -> bool  // NOLINT - temporary until implemented
  {
    // TODO(mgrettondann): Implement pattern parsing
    return false;
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
    else if (parse_normal_pattern_opt()) {
      parse_action_opt(program_.per_record_);
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
  void parse_program() { parse_item_list_maybe_unterminated(); }

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
