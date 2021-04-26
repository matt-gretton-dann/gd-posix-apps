#include "gd/stdlib.h"

#include "bc-messages.hh"

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "bc.hh"

/** \subsection Accepted grammar:
 *
 * ```
 * program              : EOF
 *                      | input_item program
 *
 * input_item           : semicolon_list NEWLINE
 *                      | function
 *
 * semicolon_list       : // empty
 *                      | statement
 *                      | semicolon_list ';' statement
 *                      | semicolon_list ';'
 *
 * statement_list       : // empty
 *                      | statement
 *                      | statement_list NEWLINE
 *                      | statement_list NEWLINE statement
 *                      | statement_list ';'
 *                      | statement_list ';' statement
 *
 * statement            : expression
 *                      | STRING
 *                      | Break
 *                      | Quit
 *                      | Return
 *                      | Return '(' return_expression ')'
 *                      | For '(' expression ';' relational_expression ';' expression ')' statement
 *                      | If '(' relational_expression ')' statement
 *                      | While '(' relational_expression ')' statement
 *                      | '{' statement_list '}'
 *
 * function             : Define LETTER '(' opt_parameter_list ')' '{' NEWLINE opt_auto_define_list
 *                        statement_list '}'
 *
 * opt_parameter_list   : // empty
 *                      | parameter_list
 *
 * parameter_list       : LETTER
 *                      | define_list ',' LETTER
 *
 * opt_auto_define_list : // empty
 *                      | Auto define_list NEWLINE
 *                      | Auto define_list ';'
 *
 * define_list          : LETTER
 *                      | LETTER '[' ']'
 *                      | define_list ',' LETTER
 *                      | define_list ',' LETTER '[' ']'
 *
 * opt_argument_list    : // empty
 *                      | argument_list
 *
 * argument_list        : expression
 *                      | LETTER '[' ']' ',' argument_list
 *
 * relational_expression : expression
 *                      | expression REL_OP expression
 *
 * return_expression    : // empty
 *                      | expression
 *
 * expression           : assign_expression
 *
 * incr_decr_expression : INCR_DECR named_expression
 *                      | named_expression INCR_DECR
 *                      | primary_expression
 *
 * unary_minus_expr     : '-' incr_decr_expression
 *                      : incr_decr_expression
 *
 * power_expression     : unary_minus_expression '^' power_expression
 *                      | unary_minus_expression
 *
 * mul_expression       : mul_expression MUL_OP power_expression
 *                      | power_expression
 *
 * add_expression       : mul_expression ADD_OP add_expression
 *                      | mul_expression
 *
 * assign_expression    : named_expression ASSIGN_OP assign_expression
 *                      | add_expression
 *
 * primary_expression   : named_expression
 *                      | NUMBER
 *                      | '(' expression ')'
 *                      | LETTER '(' opt_argument_list ')'
 *                      | Length '(' expression ')'
 *                      | Sqrt '(' expression ')'
 *                      | Scale '(' expression ')'
 *
 * named_expression     : LETTER
 *                      | LETTER '[' expression ']'
 *                      | Scale
 *                      | Ibase
 *                      | Obase
 * ```
 */

GD::Bc::Parser::Parser(std::unique_ptr<Lexer>&& lexer, bool interactive)
    : lexer_(std::move(lexer)), interactive_(interactive), error_(false)
{
  assert(lexer_ != nullptr);
}

std::shared_ptr<GD::Bc::Instructions> GD::Bc::Parser::parse()
{
  instructions_ = std::make_shared<Instructions>();
  parse_program();
  return instructions_;
}

void GD::Bc::Parser::parse_program()
{
  /* program : EOF | input_item program */
  bool cont = true;
  while (cont) {
    if (lexer_->peek() == Token::Type::eof) {
      insert_eof();
      cont = false;
    }
    else {
      cont = parse_input_item();
    }
  }
}

bool GD::Bc::Parser::parse_input_item()
{ /* input_item : semicolon_list NEWLINE | function */
  if (lexer_->peek() == Token::Type::define) {
    parse_function();
    return true;
  }
  else {
    parse_semicolon_list();
    if (lexer_->peek() != Token::Type::newline) {
      insert_error(Msg::missing_newline_at_end_of_semicolon_list, lexer_->peek());
    }
    else {
      lexer_->chew();
    }
    return !interactive_ && !error_;
  }
}

void GD::Bc::Parser::parse_semicolon_list()
{
  /* semicolon_list : EMPTY | statement | semicolon_list ';' statement | semicolon_list ';'
   *
   * This translates into:
   * semicolon_list: {{maybe_statement} semicolon}* {maybe_statement}
   */

  bool cont = true;
  while (cont && !error_) {
    (void)parse_opt_statement();
    cont = lexer_->peek() == Token::Type::semicolon;
    if (cont) {
      lexer_->chew();
    }
  }
}

void GD::Bc::Parser::parse_statement_list()
{
  /* semicolon_list : EMPTY | statement | statement_list ';' statement | statement_list
                    | statement_list '\n' statement | statement_list '\n'
   *
   * This translates into:
   * semicolon_list: (maybe_statement (semicolon|newline))* maybe_statement
   */

  bool cont = true;
  while (cont && !error_) {
    (void)parse_opt_statement();
    cont = (lexer_->peek() == Token::Type::semicolon || lexer_->peek() == Token::Type::newline);
    if (cont) {
      lexer_->chew();
    }
  }
}

void GD::Bc::Parser::parse_statement()
{
  if (!parse_opt_statement()) {
    insert_error(Msg::expected_statement, lexer_->peek());
  }
}

bool GD::Bc::Parser::parse_opt_statement()
{
  /* statement : expression | STRING | Break | Quit | Return | Return '(' return_expression ')'
               | For '(' expression ';' relational_expression ';' expression ')' statement
               | If '(' relational_expression ')' statement
               | While '(' relational_expression ')' statement
               | '{' statement_list '}'
  */

  auto const& token = lexer_->peek();
  switch (token.type()) {
  case Token::Type::string: {
    auto s = insert_string(token.string());
    insert_print(s, Instruction::Stream::stdout);
    lexer_->chew();
    return true;
  }
  case Token::Type::break_:
    assert(false);
  case Token::Type::quit:
    insert_quit(0);
    lexer_->chew();
    return true;
  case Token::Type::return_:
    assert(false);
  case Token::Type::for_:
    assert(false);
  case Token::Type::if_:
    assert(false);
  case Token::Type::while_:
    assert(false);
  case Token::Type::lbrace:
    lexer_->chew();
    parse_statement_list();
    if (lexer_->peek() != Token::Type::rbrace) {
      insert_error(Msg::expected_rbrace, lexer_->peek());
    }
    else {
      lexer_->chew();
    }
    return true;
  default:
    return false;
  }
}

void GD::Bc::Parser::parse_function() { assert(false); }

GD::Bc::Parser::Index GD::Bc::Parser::insert_eof()
{
  instructions_->emplace_back(Instruction::Opcode::eof);
  return instructions_->size() - 1;
}

GD::Bc::Parser::Index GD::Bc::Parser::insert_string(std::string const& s)
{
  instructions_->emplace_back(Instruction::Opcode::string, s);
  return instructions_->size() - 1;
}

GD::Bc::Parser::Index GD::Bc::Parser::insert_print(Index idx, Instruction::Stream stream)
{
  Index insert = instructions_->size();
  Offset offset = idx - insert;
  assert(offset < 0);
  instructions_->emplace_back(Instruction::Opcode::print, offset, stream);
  return insert;
}

GD::Bc::Parser::Index GD::Bc::Parser::insert_quit(unsigned exit_code)
{
  instructions_->emplace_back(Instruction::Opcode::quit, exit_code);
  return instructions_->size() - 1;
}
