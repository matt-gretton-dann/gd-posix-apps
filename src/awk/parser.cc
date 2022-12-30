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

  auto parse_statement_list_opt([[maybe_unused]] Instructions& instrs)  // NOLINT - temporary
    -> bool
  {
    // TODO(mgrettondann): Implement parse statement list.
    return false;
  }

  /** \brief         Parse an optional action, generating code at the end of the instructions given.
   *  \param  instrs Instructions to append to.
   *  \return        True iff we parsed an action.
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
   * terminator_optional : SEMICOLON
   *                     | NEWLINE
   *                     | semicolon terminator_optional
   *                     | newline terminator_optional
   *                     | empty
   */
  auto parse_terminator_optional() -> bool
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
    while (parse_item_opt() && parse_terminator_optional()) {
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
