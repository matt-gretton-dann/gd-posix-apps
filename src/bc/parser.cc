/** \file   parser.cc
 *  \brief  Implementation of GD::BC::Parser and related classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/stdlib.h"

#include "bc-messages.hh"

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "bc.hh"

auto GD::Bc::operator==(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs) -> bool
{
  if (lhs.type() != rhs.type()) {
    return false;
  }
  return lhs.type() == Parser::ExprType::missing || lhs.index() == rhs.index();
}

auto GD::Bc::operator==(Parser::ExprIndex const& lhs, Parser::Index rhs) -> bool
{
  return lhs.type() != Parser::ExprType::missing && lhs.index() == rhs;
}

auto GD::Bc::operator==(Parser::ExprIndex const& lhs, Parser::ExprType rhs) -> bool
{
  return lhs.type() == rhs;
}

auto GD::Bc::operator!=(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs) -> bool
{
  return !(lhs == rhs);
}

auto GD::Bc::operator!=(Parser::ExprIndex const& lhs, Parser::Index rhs) -> bool
{
  return !(lhs == rhs);
}

auto GD::Bc::operator!=(Parser::ExprIndex const& lhs, Parser::ExprType rhs) -> bool
{
  return !(lhs == rhs);
}

auto GD::Bc::operator-(Parser::ExprIndex const& lhs, Parser::ExprIndex const& rhs)
  -> GD::Bc::Parser::Offset
{
  return lhs.index() - rhs.index();
}

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
 * unary_minus_expr     : '-' unary_minus_expression
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
    : lexer_(std::move(lexer)), interactive_(interactive), error_(false), in_function_(false),
      seen_quit_(false)
{
  assert(lexer_ != nullptr);  // NOLINT
}

auto GD::Bc::Parser::parse() -> std::shared_ptr<GD::Bc::Instructions>
{
  /* reset everything - assuming we may be in error recovery. */
  instructions_ = std::make_shared<Instructions>();
  while (!loop_breaks_.empty()) {
    loop_breaks_.pop();
  }
  in_function_ = false;
  parse_program();
  return instructions_;
}

auto GD::Bc::Parser::seen_quit() const noexcept -> bool { return seen_quit_; }

void GD::Bc::Parser::parse_program()
{
  /* program : EOF | input_item program */
  bool cont = true;
  while (cont) {
    auto type = lexer_->peek().type();
    if (type == Token::Type::eof) {
      insert_eof();
      cont = false;
    }
    else {
      cont = parse_input_item();
    }
  }

  if (error_) {
    insert_eof();
  }
}

auto GD::Bc::Parser::parse_input_item() -> bool
{ /* input_item : semicolon_list NEWLINE | function */
  if (lexer_->peek() == Token::Type::define) {
    parse_function();
    return true;
  }
  parse_semicolon_list();
  if (lexer_->peek() != Token::Type::newline) {
    insert_error(Msg::missing_newline_at_end_of_semicolon_list, lexer_->peek());
  }
  else {
    lexer_->chew();
  }
  return !interactive_ && !error_;
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

auto GD::Bc::Parser::parse_opt_statement() -> bool
{
  /* statement : expression | STRING | Break | Quit | Return | Return '(' return_expression ')'
               | For '(' expression ';' relational_expression ';' expression ')' statement
               | If '(' relational_expression ')' statement
               | While '(' relational_expression ')' statement
               | '{' statement_list '}'
     If extensions enabled:
               | Halt
  */

  auto const& token = lexer_->peek();
  switch (token.type()) {
  case Token::Type::string: {
    auto s = insert_string(token.string());
    insert_print(s, Instruction::Stream::output);
    lexer_->chew();
    return true;
  }
  case Token::Type::break_:
    parse_break_statement();
    return true;
  case Token::Type::quit:
    seen_quit_ = true;
    lexer_->chew();
    return true;
  case Token::Type::halt:
    insert_quit(0);
    lexer_->chew();
    return true;
  case Token::Type::return_:
    parse_return_statement();
    return true;
  case Token::Type::for_:
    parse_for_statement();
    return true;
  case Token::Type::if_:
    parse_if_statement();
    return true;
  case Token::Type::while_:
    parse_while_statement();
    return true;
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
  default: {
    ExprIndex idx = parse_opt_expression();
    switch (idx.type()) {
    case ExprType::missing:
      return false;
    case ExprType::assignment:
      return true;
    default:
      insert_print(idx, Instruction::Stream::output);
      return true;
    }
  }
  }
}

void GD::Bc::Parser::parse_break_statement()
{
  assert(lexer_->peek() == Token::Type::break_);  // NOLINT
  lexer_->chew();

  if (!in_loop()) {
    insert_error(Msg::break_outside_of_loop);
    return;
  }

  auto branch = insert_branch(ExprIndex(0));
  add_loop_exit(branch.index());
}

void GD::Bc::Parser::parse_return_statement()
{
  if (!in_function_) {
    insert_error(Msg::return_outside_function);
    return;
  }

  assert(lexer_->peek() == Token::Type::return_);  // NOLINT
  lexer_->chew();

  ExprIndex expr = ExprIndex::missing();

  bool need_lparens = !extensions_enabled();
  bool found_lparens = false;
  if (lexer_->peek() == Token::Type::lparens) {
    lexer_->chew();
    found_lparens = true;
  }

  if (!need_lparens || found_lparens) {
    expr = parse_return_expression();
  }

  if (found_lparens) {
    if (lexer_->peek() != Token::Type::rparens) {
      insert_error(Msg::expected_rparens, lexer_->peek());
      return;
    }
    lexer_->chew();
  }

  if (expr == ExprType::missing) {
    expr = insert_number("0");
  }

  insert_return(expr);
}

void GD::Bc::Parser::parse_for_statement()
{
  /* Because we generate code as we parse it we end up with a bit of a mess here but for the loop
   *
   * for(init; rel; update) { body }
   *
   * We should end up with the code:
   *
   *    <init>
   * begin:
   *    <rel>
   *    bz exit
   *    b  body
   * update:
   *    <update>
   *    b  begin
   * body:
   *    <body>
   *    b  update
   */
  assert(lexer_->peek() == Token::Type::for_);  // NOLINT
  lexer_->chew();

  if (lexer_->peek() != Token::Type::lparens) {
    insert_error(Msg::expected_lparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  /* Initializer.  */
  parse_expression();

  if (lexer_->peek() != Token::Type::semicolon) {
    insert_error(Msg::expected_semicolon, lexer_->peek());
    return;
  }
  lexer_->chew();

  /* Relational expression. */
  ExprIndex begin(instructions_->size());
  auto rel_expr = parse_relational_expression();

  if (lexer_->peek() != Token::Type::semicolon) {
    insert_error(Msg::expected_semicolon, lexer_->peek());
    return;
  }
  lexer_->chew();

  auto bz = insert_branch_zero(rel_expr, ExprIndex(0));
  push_loop();
  add_loop_exit(bz.index());
  auto b_to_body = insert_branch(ExprIndex(0));

  /* Update expression. */
  ExprIndex update(instructions_->size());
  parse_expression();
  insert_branch(begin);
  if (lexer_->peek() != Token::Type::rparens) {
    insert_error(Msg::expected_rparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  ExprIndex body(instructions_->size());
  parse_statement();
  insert_branch(update);

  instructions_->at(b_to_body.index()).op1(body - b_to_body);
  update_loop_exits(instructions_->size());
  pop_loop();
}

void GD::Bc::Parser::parse_if_statement()
{
  assert(lexer_->peek() == Token::Type::if_);  // NOLINT
  lexer_->chew();

  if (lexer_->peek() != Token::Type::lparens) {
    insert_error(Msg::expected_lparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  ExprIndex begin(instructions_->size());
  auto rel_expr = parse_relational_expression();

  if (lexer_->peek() != Token::Type::rparens) {
    insert_error(Msg::expected_rparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  auto bz = insert_branch_zero(rel_expr, ExprIndex(0));

  parse_statement();
  ExprIndex end(instructions_->size());
  instructions_->at(bz.index()).op2(end - bz);
}

void GD::Bc::Parser::parse_while_statement()
{
  assert(lexer_->peek() == Token::Type::while_);  // NOLINT

  lexer_->chew();

  if (lexer_->peek() != Token::Type::lparens) {
    insert_error(Msg::expected_lparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  ExprIndex begin(instructions_->size());
  auto rel_expr = parse_relational_expression();

  if (lexer_->peek() != Token::Type::rparens) {
    insert_error(Msg::expected_rparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  auto bz = insert_branch_zero(rel_expr, ExprIndex(0));
  push_loop();
  add_loop_exit(bz.index());

  parse_statement();
  insert_branch(begin);
  update_loop_exits(instructions_->size());
  pop_loop();
}

void GD::Bc::Parser::push_loop() { loop_breaks_.push({}); }

void GD::Bc::Parser::add_loop_exit(Index idx) { loop_breaks_.top().push_back(idx); }

void GD::Bc::Parser::update_loop_exits(Index dest)
{
  for (auto idx : loop_breaks_.top()) {
    auto& i = instructions_->at(idx);
    Offset o = dest - idx;
    if (i.opcode() == Instruction::Opcode::branch) {
      i.op1(o);
    }
    else if (i.opcode() == Instruction::Opcode::branch_zero) {
      i.op2(o);
    }
    else {
      assert(false);  // NOLINT
    }
  }
}

void GD::Bc::Parser::pop_loop() { loop_breaks_.pop(); }

auto GD::Bc::Parser::in_loop() const -> bool { return !loop_breaks_.empty(); }

void GD::Bc::Parser::parse_function()
{
  if (in_function_) {
    insert_error(Msg::unable_to_nest_function_definitions);
    return;
  }
  in_function_ = true;
  assert(lexer_->peek() == Token::Type::define);  // NOLINT
  Location loc = lexer_->location();
  lexer_->chew();

  if (lexer_->peek() != Token::Type::letter) {
    insert_error(Msg::expected_letter, lexer_->peek());
    return;
  }
  auto letter = lexer_->peek().letter();
  lexer_->chew();
  ExprIndex function_begin = insert_function_begin(VariableMask(), loc);

  if (lexer_->peek() != Token::Type::lparens) {
    insert_error(Msg::expected_lparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  parse_opt_parameter_list(function_begin);

  insert_pop_param_mark();

  if (lexer_->peek() != Token::Type::rparens) {
    insert_error(Msg::expected_rparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  if (lexer_->peek() != Token::Type::lbrace) {
    insert_error(Msg::expected_lbrace, lexer_->peek());
    return;
  }
  lexer_->chew();

  if (lexer_->peek() == Token::Type::newline) {
    lexer_->chew();
  }
  else if (!extensions_enabled()) {
    insert_error(Msg::expected_newline, lexer_->peek());
    return;
  }

  parse_opt_auto_define_list(function_begin);
  parse_statement_list();

  if (lexer_->peek() != Token::Type::rbrace) {
    insert_error(Msg::expected_rbrace, lexer_->peek());
    return;
  }
  lexer_->chew();

  /* Ensure there's a return at the end (may be dead code).  */
  auto number = insert_number("0");
  insert_return(number);

  insert_function_end(letter, function_begin);
  in_function_ = false;
}

auto GD::Bc::Parser::parse_opt_parameter_list(ExprIndex function_begin) -> GD::Bc::Parser::ExprIndex
{
  return parse_opt_define_list(function_begin, false);
}

auto GD::Bc::Parser::parse_opt_auto_define_list(ExprIndex function_begin)
  -> GD::Bc::Parser::ExprIndex
{
  if (lexer_->peek() != Token::Type::auto_) {
    return ExprIndex::missing();
  }
  lexer_->chew();

  ExprIndex result = parse_define_list(function_begin, true);

  if (lexer_->peek() != Token::Type::semicolon && lexer_->peek() != Token::Type::newline) {
    insert_error(Msg::expected_semicolon_or_newline, lexer_->peek());
    return ExprIndex::missing();
  }
  lexer_->chew();

  return result;
}

auto GD::Bc::Parser::parse_opt_define_list(ExprIndex function_begin, bool allow_duplicates)
  -> GD::Bc::Parser::ExprIndex
{
  ExprIndex expr = parse_opt_define_element(function_begin, allow_duplicates);
  if (expr == ExprType::missing) {
    return expr;
  }

  while (lexer_->peek() == Token::Type::comma && !error_) {
    lexer_->chew();
    expr = parse_define_element(function_begin, allow_duplicates);
  }

  return expr;
}

auto GD::Bc::Parser::parse_define_list(ExprIndex function_begin, bool allow_duplicates)
  -> GD::Bc::Parser::ExprIndex
{
  auto result = parse_opt_define_list(function_begin, allow_duplicates);
  if (result == ExprType::missing) {
    return insert_error(Msg::expected_define_list, lexer_->peek());
  }
  return result;
}

auto GD::Bc::Parser::parse_opt_define_element(ExprIndex function_begin, bool allow_duplicates)
  -> GD::Bc::Parser::ExprIndex
{
  if (lexer_->peek() != Token::Type::letter) {
    return ExprIndex::missing();
  }

  VariableMask mask = std::get<VariableMask>(instructions_->at(function_begin.index()).op1());
  auto letter = lexer_->peek().letter();
  lexer_->chew();

  if (lexer_->peek() != Token::Type::lsquare) {
    if (!allow_duplicates) {
      if (mask.contains(Variable(letter))) {
        insert_error(Msg::duplicate_variables_in_function_definition, letter);
        return ExprIndex::missing();
      }

      auto pop = insert_pop_param();
      auto var = insert_variable(Variable(letter));
      insert_store(var, pop);
    }

    mask.add(Variable(letter));
    instructions_->at(function_begin.index()).op1(mask);
    return function_begin;
  }

  lexer_->chew();

  if (lexer_->peek() != Token::Type::rsquare) {
    insert_error(Msg::expected_rsquare, lexer_->peek());
    return ExprIndex::missing();
  }
  lexer_->chew();

  if (!allow_duplicates) {
    if (mask.contains(Array(letter))) {
      insert_error(Msg::duplicate_arrays_in_function_definition, letter);
      return ExprIndex::missing();
    }

    auto pop = insert_pop_param_array();
    auto var = insert_array_slice(Array(letter));
    insert_store(var, pop);
  }

  mask.add(Array(letter));
  instructions_->at(function_begin.index()).op1(mask);
  return function_begin;
}

auto GD::Bc::Parser::parse_define_element(ExprIndex function_begin, bool allow_duplicates)
  -> GD::Bc::Parser::ExprIndex
{
  auto result = parse_opt_define_element(function_begin, allow_duplicates);
  if (result == ExprType::missing) {
    return insert_error(Msg::expected_define_element, lexer_->peek());
  }
  return result;
}

auto GD::Bc::Parser::parse_opt_argument_list() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex expr = parse_opt_expression(POPEFlags::parse_array_slices);
  if (expr == ExprType::missing) {
    return expr;
  }
  insert_push_param(expr);

  while (lexer_->peek() == Token::Type::comma && !error_) {
    lexer_->chew();
    expr = parse_expression(POPEFlags::parse_array_slices);
    insert_push_param(expr);
  }

  return expr;
}

auto GD::Bc::Parser::parse_relational_expression() -> GD::Bc::Parser::ExprIndex
{
  auto lhs = parse_expression();

  if (!lexer_->peek().is_rel_op()) {
    return lhs;
  }
  auto type = lexer_->peek().type();
  lexer_->chew();
  auto rhs = parse_expression();
  switch (type) {
  case Token::Type::equals:
    return insert_arith(Instruction::Opcode::equals, lhs, rhs);
  case Token::Type::less_than_equals:
    return insert_arith(Instruction::Opcode::less_than_equals, lhs, rhs);
  case Token::Type::greater_than_equals:
    return insert_arith(Instruction::Opcode::less_than_equals, rhs, lhs);
  case Token::Type::not_equals:
    return insert_arith(Instruction::Opcode::not_equals, lhs, rhs);
  case Token::Type::less_than:
    return insert_arith(Instruction::Opcode::less_than, lhs, rhs);
  case Token::Type::greater_than:
    return insert_arith(Instruction::Opcode::less_than, rhs, lhs);
  default:
    assert(false);
    abort();
  }
}

auto GD::Bc::Parser::parse_return_expression() -> GD::Bc::Parser::ExprIndex
{
  return parse_opt_expression();
}

auto GD::Bc::Parser::parse_opt_expression(POPEFlags flags) -> GD::Bc::Parser::ExprIndex
{
  /* expression : assign_expression */
  return parse_opt_assign_expression(flags);
}

auto GD::Bc::Parser::parse_expression(POPEFlags flags) -> GD::Bc::Parser::ExprIndex
{
  auto idx = parse_opt_expression(flags);
  if (idx == ExprType::missing) {
    idx = insert_error(Msg::expected_expression, lexer_->peek());
  }

  return idx;
}

auto GD::Bc::Parser::parse_opt_assign_expression(POPEFlags flags) -> GD::Bc::Parser::ExprIndex
{
  /* assign_expression : named_exprssion ASSIGN_OP assign_expression | add_expression
   *
   * This is awkward because named_expression can also be the first part of add_expression.  So we
   * do a lot of bits by hand.  We basically process the first 'primary' token by hand and then
   * roll up the precedence stack to calculate the correct value.
   *
   * The first tokens can be: named_expression, primary_expression, '-', '++', '--'.
   *
   * Of which only named_expression can be followed by a assign_op.
   */
  ExprIndex primary_expr = parse_opt_primary_expression(flags);

  /* If this was an array slice we just return the result.  */
  if (primary_expr == ExprType::array_slice) {
    return primary_expr;
  }

  /* It was a named expression so see if we are an assign op and handle that if we are.  */
  if (primary_expr == ExprType::named) {
    if (lexer_->peek().is_assign_op()) {
      Token::Type assign_type = lexer_->peek().type();
      lexer_->chew();
      ExprIndex assign_index = parse_assign_expression();
      if (assign_type != Token::Type::assign) {
        switch (assign_type) {
        case Token::Type::add_assign:
          assign_index = insert_arith(Instruction::Opcode::add, primary_expr, assign_index);
          break;
        case Token::Type::subtract_assign:
          assign_index = insert_arith(Instruction::Opcode::subtract, primary_expr, assign_index);
          break;
        case Token::Type::multiply_assign:
          assign_index = insert_arith(Instruction::Opcode::multiply, primary_expr, assign_index);
          break;
        case Token::Type::divide_assign:
          assign_index = insert_arith(Instruction::Opcode::divide, primary_expr, assign_index);
          break;
        case Token::Type::modulo_assign:
          assign_index = insert_arith(Instruction::Opcode::modulo, primary_expr, assign_index);
          break;
        case Token::Type::power_assign:
          assign_index = insert_arith(Instruction::Opcode::power, primary_expr, assign_index);
          break;
        default:
          assert(false);
        }
      }
      return insert_store(primary_expr, assign_index);
    }

    /* Not an assign op but may be an post increment/decrement that needs dealing with.  */
    if (lexer_->peek().is_incr_decr_op()) {
      bool incr = lexer_->peek() == Token::Type::increment;
      lexer_->chew();
      ExprIndex load = insert_load(primary_expr);
      ExprIndex num1 = insert_number("1");
      ExprIndex assign_index =
        insert_arith(incr ? Instruction::Opcode::add : Instruction::Opcode::subtract, load, num1);
      insert_store(primary_expr, assign_index);
      primary_expr = load;
    }
  }

  /* We aren't a primary expression so see if we're a unary minus or incr/decr expression, or if
   * there really isn't anything to read here.  */
  if (primary_expr == ExprType::missing) {
    switch (lexer_->peek().type()) {
    case Token::Type::subtract: /* Unary minus in this case.  */
      primary_expr = parse_unary_minus_expression();
      break;
    case Token::Type::increment:
    case Token::Type::decrement:
      primary_expr = parse_incr_decr_expression();
      break;
    default:
      return primary_expr;
    }
  }

  /* If we've reached here we're definitely some form of expression and we have its left hand
   * side. We just need to walk up the parsing stack to see what are.
   */
  auto result = parse_power_expression(primary_expr);
  result = parse_mul_expression(result);
  result = parse_add_expression(result);

  return result;
}

auto GD::Bc::Parser::parse_assign_expression(POPEFlags flags) -> GD::Bc::Parser::ExprIndex
{
  auto idx = parse_opt_assign_expression(flags);
  if (idx == ExprType::missing) {
    idx = insert_error(Msg::expected_assign_expression, lexer_->peek());
  }

  return idx;
}

auto GD::Bc::Parser::parse_add_expression() -> GD::Bc::Parser::ExprIndex
{
  return parse_add_expression(parse_mul_expression());
}

auto GD::Bc::Parser::parse_add_expression(GD::Bc::Parser::ExprIndex lhs)
  -> GD::Bc::Parser::ExprIndex
{
  /* add_expression : mul_expression ADD_OP add_expression | mul_expression */

  while (lexer_->peek().is_add_op()) {
    Token::Type type = lexer_->peek().type();
    lexer_->chew();
    ExprIndex rhs = parse_mul_expression();
    switch (type) {
    case Token::Type::add:
      lhs = insert_arith(Instruction::Opcode::add, lhs, rhs);
      break;
    case Token::Type::subtract:
      lhs = insert_arith(Instruction::Opcode::subtract, lhs, rhs);
      break;
    default:
      assert(false);
    }
  }
  return lhs;
}

auto GD::Bc::Parser::parse_mul_expression() -> GD::Bc::Parser::ExprIndex
{
  return parse_mul_expression(parse_power_expression());
}

auto GD::Bc::Parser::parse_mul_expression(GD::Bc::Parser::ExprIndex lhs)
  -> GD::Bc::Parser::ExprIndex
{
  /* mul_expression : power_expression MUL_OP mul_expression | power_expression */

  while (lexer_->peek().is_mul_op()) {
    Token::Type type = lexer_->peek().type();
    lexer_->chew();
    ExprIndex rhs = parse_power_expression();
    switch (type) {
    case Token::Type::multiply:
      lhs = insert_arith(Instruction::Opcode::multiply, lhs, rhs);
      break;
    case Token::Type::divide:
      lhs = insert_arith(Instruction::Opcode::divide, lhs, rhs);
      break;
    case Token::Type::modulo:
      lhs = insert_arith(Instruction::Opcode::modulo, lhs, rhs);
      break;
    default:
      assert(false);
    }
  }
  return lhs;
}

auto GD::Bc::Parser::parse_power_expression() -> GD::Bc::Parser::ExprIndex
{
  return parse_power_expression(parse_unary_minus_expression());
}

auto GD::Bc::Parser::parse_power_expression(GD::Bc::Parser::ExprIndex lhs)
  -> GD::Bc::Parser::ExprIndex
{
  /* power_expression : unary_minus_expression '^' power_expression | unary_minus_expression */

  while (lexer_->peek() == Token::Type::power) {
    lexer_->chew();
    ExprIndex rhs = parse_power_expression();
    lhs = insert_arith(Instruction::Opcode::power, lhs, rhs);
  }
  return lhs;
}

auto GD::Bc::Parser::parse_unary_minus_expression() -> GD::Bc::Parser::ExprIndex
{
  /* unary_minus_expr : '-' unary_minus_expression | incr_decr_expression */

  if (lexer_->peek() == Token::Type::subtract) {
    lexer_->chew();
    return insert_negate(parse_unary_minus_expression());
  }

  return parse_incr_decr_expression();
}

auto GD::Bc::Parser::parse_incr_decr_expression() -> GD::Bc::Parser::ExprIndex
{
  /* incr_decr_expression : INCR_DECR named_expression | named_expression INCR_DECR
   *                      | primary_expression.
   */

  bool pre = false;
  bool incr = false;

  if (lexer_->peek().is_incr_decr_op()) {
    pre = true;
    incr = lexer_->peek() == Token::Type::increment;
    lexer_->chew();
  }

  /* Now we expect an expression.  If we're `pre` then it must be a named expression.  If its not
   * a named expression then we can't be a post inrement or decrement.
   */
  ExprIndex named_expr = parse_primary_expression();
  if (named_expr != ExprType::named) {
    if (pre) {
      return insert_error(Msg::expected_named_expression, lexer_->peek());
    }
    return named_expr;
  }

  /* Now we know we have a named expression, if it could be post increment/decrement check.  */
  if (!pre) {
    if (!lexer_->peek().is_incr_decr_op()) {
      return named_expr;
    }

    incr = lexer_->peek() == Token::Type::increment;
    lexer_->chew();
  }

  ExprIndex load_idx = insert_load(named_expr);
  ExprIndex num_idx = insert_number("1");
  ExprIndex op_idx = insert_arith(incr ? Instruction::Opcode::add : Instruction::Opcode::subtract,
                                  load_idx, num_idx);
  insert_store(named_expr, op_idx);
  return pre ? op_idx : load_idx;
}

auto GD::Bc::Parser::parse_opt_primary_expression(POPEFlags flags) -> GD::Bc::Parser::ExprIndex
{
  bool parse_array_slices = (flags == POPEFlags::parse_array_slices);

  /* Because of the number of special cases for named_expression vs primary_expression we do
   * both in here.  */
  switch (lexer_->peek().type()) {
  case Token::Type::letter: {
    auto letter = lexer_->peek().letter();
    lexer_->chew();
    if (lexer_->peek() == Token::Type::lsquare) {
      lexer_->chew();
      auto elt = parse_opt_expression();
      if (elt == ExprType::missing && !parse_array_slices) {
        return insert_error(Msg::expected_expression, lexer_->peek());
      }
      if (lexer_->peek() != Token::Type::rsquare) {
        return insert_error(Msg::expected_rsquare, lexer_->peek());
      }
      lexer_->chew();
      if (elt == ExprType::missing) {
        return insert_array_slice(Array(letter));
      }

      return insert_array_element(Array(letter), elt);
    }

    if (lexer_->peek() == Token::Type::lparens) {
      Location loc = lexer_->location();
      lexer_->chew();

      insert_push_param_mark();
      parse_opt_argument_list();

      if (lexer_->peek() != Token::Type::rparens) {
        return insert_error(Msg::expected_rparens, lexer_->peek());
      }
      lexer_->chew();

      return insert_call(letter, loc);
    }

    return insert_variable(Variable(letter));
  }
  case Token::Type::scale: {
    lexer_->chew();
    if (lexer_->peek() == Token::Type::lparens) {
      lexer_->chew();
      auto elt = parse_expression();
      if (lexer_->peek() != Token::Type::rparens) {
        return insert_error(Msg::expected_rparens, lexer_->peek());
      }
      lexer_->chew();
      return insert_scale_expr(elt);
    }

    return insert_scale();
  }
  case Token::Type::ibase:
    lexer_->chew();
    return insert_ibase();
  case Token::Type::obase:
    lexer_->chew();
    return insert_obase();
  case Token::Type::number: {
    auto idx = insert_number(lexer_->peek().number());
    lexer_->chew();
    return idx;
  }
  case Token::Type::lparens: {
    lexer_->chew();
    auto idx = parse_expression();
    if (lexer_->peek() != Token::Type::rparens) {
      return insert_error(Msg::expected_rparens, lexer_->peek());
    }
    lexer_->chew();
    return idx;
  }
  case Token::Type::sqrt: {
    lexer_->chew();
    if (lexer_->peek() != Token::Type::lparens) {
      return insert_error(Msg::expected_lparens);
    }
    lexer_->chew();
    auto elt = parse_expression();

    if (lexer_->peek() != Token::Type::rparens) {
      return insert_error(Msg::expected_rparens, lexer_->peek());
    }
    lexer_->chew();
    return insert_sqrt(elt);
  }
  case Token::Type::abs: {
    lexer_->chew();
    if (lexer_->peek() != Token::Type::lparens) {
      return insert_error(Msg::expected_lparens);
    }
    lexer_->chew();
    auto elt = parse_expression();

    if (lexer_->peek() != Token::Type::rparens) {
      return insert_error(Msg::expected_rparens, lexer_->peek());
    }
    lexer_->chew();
    return insert_abs(elt);
  }
  case Token::Type::length: {
    lexer_->chew();
    if (lexer_->peek() != Token::Type::lparens) {
      return insert_error(Msg::expected_lparens);
    }
    lexer_->chew();
    POPEFlags expr_flags =
      extensions_enabled() ? POPEFlags::parse_array_slices : POPEFlags::parse_primary;
    auto elt = parse_expression(expr_flags);

    if (lexer_->peek() != Token::Type::rparens) {
      return insert_error(Msg::expected_rparens, lexer_->peek());
    }
    lexer_->chew();
    return insert_length(elt);
  }
  default:
    return ExprIndex::missing();
  }
}

auto GD::Bc::Parser::parse_primary_expression(POPEFlags flags) -> GD::Bc::Parser::ExprIndex
{
  auto result = parse_opt_primary_expression(flags);
  if (result == ExprType::missing) {
    result = insert_error(Msg::expected_primary_expression, lexer_->peek());
  }
  return result;
}

auto GD::Bc::Parser::ensure_expr_loaded(ExprIndex idx) -> GD::Bc::Parser::ExprIndex
{
  assert(idx != ExprType::missing);  // NOLINT
  if (idx == ExprType::named || idx == ExprType::array_slice) {
    idx = insert_load(idx);
  }
  return idx;
}

auto GD::Bc::Parser::insert_eof() -> GD::Bc::Parser::ExprIndex
{
  instructions_->emplace_back(Instruction::Opcode::eof);
  return ExprIndex(instructions_->size() - 1);
}

auto GD::Bc::Parser::insert_string(std::string const& s) -> GD::Bc::Parser::ExprIndex
{
  instructions_->emplace_back(Instruction::Opcode::string, s);
  return ExprIndex(instructions_->size() - 1);
}

auto GD::Bc::Parser::insert_print(ExprIndex idx, Instruction::Stream stream)
  -> GD::Bc::Parser::ExprIndex
{
  idx = ensure_expr_loaded(idx);
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::print, idx - result, stream);
  return result;
}

auto GD::Bc::Parser::insert_quit(unsigned exit_code) -> GD::Bc::Parser::ExprIndex
{
  instructions_->emplace_back(Instruction::Opcode::quit, exit_code);
  return ExprIndex(instructions_->size() - 1);
}

auto GD::Bc::Parser::insert_load(ExprIndex idx) -> GD::Bc::Parser::ExprIndex
{
  assert(idx == ExprType::named || idx == ExprType::array_slice);  // NOLINT
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::load, idx - result);
  return result;
}

auto GD::Bc::Parser::insert_store(ExprIndex var, ExprIndex value) -> GD::Bc::Parser::ExprIndex
{
  assert(var == ExprType::named || var == ExprType::array_slice);  // NOLINT
  value = ensure_expr_loaded(value);
  ExprIndex end(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::store, var - end, value - end);
  return ExprIndex(value.index(), ExprType::assignment);
}

auto GD::Bc::Parser::insert_arith(Instruction::Opcode opcode, ExprIndex lhs, ExprIndex rhs)
  -> GD::Bc::Parser::ExprIndex
{
  lhs = ensure_expr_loaded(lhs);
  rhs = ensure_expr_loaded(rhs);
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(opcode, lhs - result, rhs - result);
  return result;
}

auto GD::Bc::Parser::insert_negate(ExprIndex expr) -> GD::Bc::Parser::ExprIndex
{
  expr = ensure_expr_loaded(expr);
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::negate, expr - result);
  return result;
}

auto GD::Bc::Parser::insert_number(std::string const& number) -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size(), ExprType::primary);
  instructions_->emplace_back(Instruction::Opcode::number, number);
  return result;
}

auto GD::Bc::Parser::insert_array_element(Array v, ExprIndex element) -> GD::Bc::Parser::ExprIndex
{
  element = ensure_expr_loaded(element);
  ExprIndex result(instructions_->size(), ExprType::named);
  instructions_->emplace_back(Instruction::Opcode::array_element, v, element - result);
  return result;
}

auto GD::Bc::Parser::insert_array_slice(Array v) -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size(), ExprType::array_slice);
  instructions_->emplace_back(Instruction::Opcode::array, v);
  return result;
}

auto GD::Bc::Parser::insert_variable(Variable v) -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size(), ExprType::named);
  instructions_->emplace_back(Instruction::Opcode::variable, v);
  return result;
}

auto GD::Bc::Parser::insert_scale() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size(), ExprType::named);
  instructions_->emplace_back(Instruction::Opcode::scale);
  return result;
}

auto GD::Bc::Parser::insert_obase() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size(), ExprType::named);
  instructions_->emplace_back(Instruction::Opcode::obase);
  return result;
}

auto GD::Bc::Parser::insert_ibase() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size(), ExprType::named);
  instructions_->emplace_back(Instruction::Opcode::ibase);
  return result;
}

auto GD::Bc::Parser::insert_scale_expr(ExprIndex expr) -> GD::Bc::Parser::ExprIndex
{
  expr = ensure_expr_loaded(expr);
  ExprIndex result(instructions_->size(), ExprType::primary);
  instructions_->emplace_back(Instruction::Opcode::scale_expr, expr - result);
  return result;
}

auto GD::Bc::Parser::insert_sqrt(ExprIndex expr) -> GD::Bc::Parser::ExprIndex
{
  expr = ensure_expr_loaded(expr);
  ExprIndex result(instructions_->size(), ExprType::primary);
  instructions_->emplace_back(Instruction::Opcode::sqrt, expr - result);
  return result;
}

auto GD::Bc::Parser::insert_abs(ExprIndex expr) -> GD::Bc::Parser::ExprIndex
{
  expr = ensure_expr_loaded(expr);
  ExprIndex result(instructions_->size(), ExprType::primary);
  instructions_->emplace_back(Instruction::Opcode::abs, expr - result);
  return result;
}

auto GD::Bc::Parser::insert_length(ExprIndex expr) -> GD::Bc::Parser::ExprIndex
{
  if (expr != ExprType::array_slice) {
    expr = ensure_expr_loaded(expr);
  }
  ExprIndex result(instructions_->size(), ExprType::primary);
  instructions_->emplace_back(Instruction::Opcode::length, expr - result);
  return result;
}

auto GD::Bc::Parser::insert_branch_zero(ExprIndex dest, ExprIndex cmp) -> GD::Bc::Parser::ExprIndex
{
  cmp = ensure_expr_loaded(cmp);
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::branch_zero, dest - result, cmp - result);
  return result;
}

auto GD::Bc::Parser::insert_branch(ExprIndex dest) -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::branch, dest - result);
  return result;
}

auto GD::Bc::Parser::insert_return(ExprIndex expr) -> GD::Bc::Parser::ExprIndex
{
  expr = ensure_expr_loaded(expr);
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::return_, expr - result);
  return result;
}

auto GD::Bc::Parser::insert_call(Letter v, Location const& loc) -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size(), ExprType::primary);
  instructions_->emplace_back(Instruction::Opcode::call, v, loc);
  return result;
}

auto GD::Bc::Parser::insert_push_param_mark() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::push_param_mark);
  return result;
}

auto GD::Bc::Parser::insert_pop_param_mark() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::pop_param_mark);
  return result;
}

auto GD::Bc::Parser::insert_push_param(ExprIndex expr) -> GD::Bc::Parser::ExprIndex
{
  expr = ensure_expr_loaded(expr);
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::push_param, expr - result);
  return result;
}

auto GD::Bc::Parser::insert_pop_param() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::pop_param);
  return result;
}

auto GD::Bc::Parser::insert_pop_param_array() -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::pop_param_array);
  return result;
}

auto GD::Bc::Parser::insert_function_begin(VariableMask mask, Location const& loc)
  -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::function_begin, mask, loc);
  return result;
}

auto GD::Bc::Parser::insert_function_end(Letter letter, ExprIndex begin)
  -> GD::Bc::Parser::ExprIndex
{
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::function_end, letter, begin - result);
  return result;
}
