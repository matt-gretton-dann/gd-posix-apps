/** \file   parser.cc
 *  \brief  Implementation of GD::BC::Parser and related classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/stdlib.h"

#include "bc-messages.hh"

#include <array>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "bc.hh"

namespace {
using TokenType = GD::Bc::Token::Type;
using Opcode = GD::Bc::Instruction::Opcode;

struct BinaryOperator
{
  Opcode opcode{};
  unsigned precedence{};
  unsigned right_precedence{};
};

// Zero precedence marks every token outside the arithmetic grammar. The
// right operand of ^ admits another ^, whereas the left-associative
// operators require strictly higher precedence on their right.
constexpr auto binary_operators = [] {
  std::array<BinaryOperator, static_cast<std::size_t>(TokenType::semicolon) + 1> table{};
  table[static_cast<std::size_t>(TokenType::add)] = {Opcode::add, 1, 2};
  table[static_cast<std::size_t>(TokenType::subtract)] = {Opcode::subtract, 1, 2};
  table[static_cast<std::size_t>(TokenType::multiply)] = {Opcode::multiply, 2, 3};
  table[static_cast<std::size_t>(TokenType::divide)] = {Opcode::divide, 2, 3};
  table[static_cast<std::size_t>(TokenType::modulo)] = {Opcode::modulo, 2, 3};
  table[static_cast<std::size_t>(TokenType::power)] = {Opcode::power, 3, 3};
  return table;
}();
template<typename T, std::size_t InlineSize>
class ParseStack
{
public:
  auto size() const -> std::size_t { return size_; }
  auto back() const -> T const&
  {
    return size_ <= InlineSize ? local_[size_ - 1] : overflow_.back();
  }
  void push(T value)
  {
    if (size_ < InlineSize)
      local_[size_] = value;
    else
      overflow_.push_back(value);
    ++size_;
  }
  auto pop() -> T
  {
    auto const result = back();
    if (--size_ >= InlineSize)
      overflow_.pop_back();
    return result;
  }

private:
  std::array<T, InlineSize> local_;
  std::vector<T> overflow_;
  std::size_t size_ = 0;
};

}  // namespace

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
  return static_cast<Parser::Offset>(lhs.index() - rhs.index());
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
 * add_expression       : add_expression ADD_OP mul_expression
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
    : lexer_(std::move(lexer)), interactive_(interactive)
{
  assert(lexer_ != nullptr);  // NOLINT
}

void GD::Bc::Parser::clear_context()
{
  while (!loop_breaks_.empty()) {
    loop_breaks_.pop();
  }
  in_function_ = false;
}

void GD::Bc::Parser::emit_fatal_error(std::string const& diagnostic)
{
  instructions_->clear();
  clear_context();
  auto const message = insert_string(diagnostic);
  insert_print(message, Instruction::Stream::error);
  insert_quit(1);
  insert_eof();
}

auto GD::Bc::Parser::parse() -> std::shared_ptr<GD::Bc::Instructions>
{
  if (auto const* exception = std::get_if<std::exception_ptr>(&fatal_failure_)) {
    std::rethrow_exception(*exception);
  }
  // Retained results are independent snapshots. Only reuse an unobserved owner;
  // clear every instruction/cache while retaining the largest item's capacity.
  // A fatal parser still cannot resume with partially consumed input.
  if (instructions_ && instructions_.use_count() == 1) {
    instructions_->clear();
  }
  else {
    instructions_ = GD_BC_PROFILE_SHARED(Profile::AllocationKind::instructions, Instructions)();
  }
  clear_context();
  if (auto const* diagnostic = std::get_if<std::string>(&fatal_failure_)) {
    emit_fatal_error(*diagnostic);
    return instructions_;
  }
  try {
    parse_program();
  }
  catch (ParseError& error) {
    // Keep the first diagnostic terminal even when a caller parses again instead
    // of executing its quit instruction. Recoverable parsing is separate work.
    // Moving the diagnostic does not allocate. Remember it before emitting
    // instructions, which may itself fail and be retried by the caller.
    auto const& diagnostic = fatal_failure_.emplace<std::string>(std::move(error.diagnostic));
    emit_fatal_error(diagnostic);
  }
  catch (...) {
    // A Reader or allocation failure may leave the lexer midway through an
    // item. Replaying its tail would silently change the program boundary.
    fatal_failure_.emplace<std::exception_ptr>(std::current_exception());
    instructions_.reset();
    clear_context();
    throw;
  }
  return instructions_;
}

auto GD::Bc::Parser::seen_quit() const noexcept -> bool { return seen_quit_; }

void GD::Bc::Parser::parse_program()
{
  /* program : EOF | input_item program */
  // Diagnostics unwind to parse(); other input failures are terminal there too.
  // A normally returning grammar helper never carries a pending error state.
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
  return !interactive_;
}

void GD::Bc::Parser::parse_semicolon_list()
{
  /* semicolon_list : EMPTY | statement | semicolon_list ';' statement | semicolon_list ';'
   *
   * This translates into:
   * semicolon_list: {{maybe_statement} semicolon}* {maybe_statement}
   */

  bool cont = true;
  while (cont) {
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
  while (cont) {
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
    ExprIndex const idx = parse_opt_expression();
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

  bool constexpr need_lparens = !extensions_enabled();
  bool found_lparens = false;
  if (lexer_->peek() == Token::Type::lparens) {
    lexer_->chew();
    found_lparens = true;
  }

  if (found_lparens || !need_lparens) {
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
  ExprIndex const begin(instructions_->size());
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
  ExprIndex const update(instructions_->size());
  parse_expression();
  insert_branch(begin);
  if (lexer_->peek() != Token::Type::rparens) {
    insert_error(Msg::expected_rparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  ExprIndex const body(instructions_->size());
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

  ExprIndex const begin(instructions_->size());
  auto rel_expr = parse_relational_expression();

  if (lexer_->peek() != Token::Type::rparens) {
    insert_error(Msg::expected_rparens, lexer_->peek());
    return;
  }
  lexer_->chew();

  auto const bz = insert_branch_zero(rel_expr, ExprIndex(0));

  parse_statement();
  ExprIndex const end(instructions_->size());
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

  ExprIndex const begin(instructions_->size());
  auto const rel_expr = parse_relational_expression();

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
    auto o = static_cast<Offset>(dest - idx);
    if (i.opcode() == Instruction::Opcode::branch) {
      i.op1(o);
    }
    else if (i.opcode() == Instruction::Opcode::branch_zero) {
      i.op2(o);
    }
    else {
      abort();
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
  Location const loc = lexer_->location();
  lexer_->chew();

  if (lexer_->peek() != Token::Type::letter) {
    insert_error(Msg::expected_letter, lexer_->peek());
    return;
  }
  auto letter = lexer_->peek().letter();
  lexer_->chew();
  ExprIndex const function_begin = insert_function_begin(VariableMask(), loc);

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

  while (lexer_->peek() == Token::Type::comma) {
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

    return insert_arith(Instruction::Opcode::less_than_equals, rhs, lhs);  // NOLINT
  case Token::Type::not_equals:
    return insert_arith(Instruction::Opcode::not_equals, lhs, rhs);
  case Token::Type::less_than:
    return insert_arith(Instruction::Opcode::less_than, lhs, rhs);
  case Token::Type::greater_than:
    return insert_arith(Instruction::Opcode::less_than, rhs, lhs);  // NOLINT
  default:
    abort();
  }
}

auto GD::Bc::Parser::parse_return_expression() -> GD::Bc::Parser::ExprIndex
{
  return parse_opt_expression();
}

auto GD::Bc::Parser::parse_expression(POPEFlags flags) -> GD::Bc::Parser::ExprIndex
{
  auto idx = parse_opt_expression(flags);
  if (idx == ExprType::missing) {
    idx = insert_error(Msg::expected_expression, lexer_->peek());
  }

  return idx;
}

// Leaf primaries are complete immediately. A nested primary saves only the
// opener and the metadata needed after its inner expression is parsed.
struct GD::Bc::Parser::Primary
{
  ExprIndex result = ExprIndex::missing();
  std::optional<TokenType> opener;
  Letter letter{'a'};
  std::optional<Location> location;
};

auto GD::Bc::Parser::begin_primary() -> Primary
{
  auto const token = lexer_->peek().type();
  switch (token) {
  case TokenType::letter: {
    auto const letter = lexer_->peek().letter();
    lexer_->chew();
    if (lexer_->peek() == TokenType::lsquare) {
      lexer_->chew();
      return {.opener = TokenType::lsquare, .letter = letter};
    }
    if (lexer_->peek() == TokenType::lparens) {
      Location loc = lexer_->location();
      lexer_->chew();
      insert_push_param_mark();
      return {.opener = TokenType::letter, .letter = letter, .location = std::move(loc)};
    }
    return {.result = insert_variable(Variable(letter))};
  }
  case TokenType::scale:
    lexer_->chew();
    if (lexer_->peek() != TokenType::lparens)
      return {.result = insert_scale()};
    lexer_->chew();
    return {.opener = token};
  case TokenType::ibase:
    lexer_->chew();
    return {.result = insert_ibase()};
  case TokenType::obase:
    lexer_->chew();
    return {.result = insert_obase()};
  case TokenType::number: {
    auto const idx = insert_number(lexer_->peek().number());
    lexer_->chew();
    return {.result = idx};
  }
  case TokenType::lparens:
    lexer_->chew();
    return {.opener = token};
  case TokenType::sqrt:
  case TokenType::abs:
  case TokenType::length:
    lexer_->chew();
    if (lexer_->peek() != TokenType::lparens)
      return {.result = insert_error(Msg::expected_lparens, lexer_->peek())};
    lexer_->chew();
    return {.opener = token};
  default:
    return {};
  }
}

auto GD::Bc::Parser::finish_primary(Primary const& primary, ExprIndex inner, POPEFlags flags)
  -> ExprIndex
{
  auto const opener = *primary.opener;
  if (opener == TokenType::lsquare) {
    if (inner == ExprType::missing && flags != POPEFlags::parse_array_slices)
      return insert_error(Msg::expected_expression, lexer_->peek());
    if (lexer_->peek() != TokenType::rsquare)
      return insert_error(Msg::expected_rsquare, lexer_->peek());
    lexer_->chew();
    auto const array = Array(primary.letter);
    return inner == ExprType::missing ? insert_array_slice(array)
                                      : insert_array_element(array, inner);
  }

  if (lexer_->peek() != TokenType::rparens)
    return insert_error(Msg::expected_rparens, lexer_->peek());
  lexer_->chew();
  switch (opener) {
  case TokenType::letter:
    return insert_call(primary.letter, *primary.location);
  case TokenType::scale:
    return insert_scale_expr(inner);
  case TokenType::lparens:
    // Only a bare assignment statement suppresses output.
    return {ensure_expr_loaded(inner).index(), ExprType::primary};
  case TokenType::sqrt:
    return insert_sqrt(inner);
  case TokenType::abs:
    return insert_abs(inner);
  case TokenType::length:
    return insert_length(inner);
  default:
    abort();
  }
}

auto GD::Bc::Parser::parse_opt_expression(POPEFlags flags) -> ExprIndex
{
  struct Assignment
  {
    ExprIndex target{0};
    TokenType token{};
  };
  struct Operator
  {
    ExprIndex lhs{0};
    BinaryOperator operation{};
  };
  enum class Phase { leading, operand, primary_ready, operators, complete };
  struct Frame
  {
    POPEFlags flags;
    std::size_t assignment_base;
    std::size_t operator_base;
    Phase phase = Phase::leading;
    std::size_t negations = 0;
    bool required_operand = false;
    std::optional<bool> prefix_increment{};
  };
  struct SuspendedPrimary
  {
    Frame parent;
    Primary primary;
    bool first_argument = true;
  };

  // Flat expressions keep their common pending operators and assignments
  // inline. Only nesting or long right-associative chains need heap storage.
  ParseStack<Assignment, 2> assignments;
  ParseStack<Operator, 3> operators;
  std::vector<SuspendedPrimary> nested;
  Frame frame{flags, 0, 0};
  ExprIndex value = ExprIndex::missing();

  auto const push_expression = [&](POPEFlags child_flags) {
    frame = Frame{child_flags, assignments.size(), operators.size()};
  };
  auto const store_assignment = [&](Assignment assignment) {
    auto const target = assignment.target;
    switch (assignment.token) {
    case TokenType::assign:
      break;
    case TokenType::add_assign:
      value = insert_arith(Opcode::add, target, value);
      break;
    case TokenType::subtract_assign:
      value = insert_arith(Opcode::subtract, target, value);
      break;
    case TokenType::multiply_assign:
      value = insert_arith(Opcode::multiply, target, value);
      break;
    case TokenType::divide_assign:
      value = insert_arith(Opcode::divide, target, value);
      break;
    case TokenType::modulo_assign:
      value = insert_arith(Opcode::modulo, target, value);
      break;
    case TokenType::power_assign:
      value = insert_arith(Opcode::power, target, value);
      break;
    default:
      abort();
    }
    value = insert_store(target, value);
  };

  while (true) {
    switch (frame.phase) {
    case Phase::leading:
    case Phase::operand: {
      frame.required_operand = frame.phase == Phase::operand;
      frame.negations = 0;
      while (lexer_->peek() == TokenType::subtract) {
        lexer_->chew();
        ++frame.negations;
        frame.required_operand = true;
      }
      frame.prefix_increment.reset();
      if (lexer_->peek().is_incr_decr_op()) {
        frame.prefix_increment = lexer_->peek() == TokenType::increment;
        lexer_->chew();
        frame.required_operand = true;
      }
      auto primary = begin_primary();
      frame.phase = Phase::primary_ready;
      if (primary.opener) {
        auto const opener = *primary.opener;
        nested.push_back({frame, std::move(primary)});
        auto const slices =
          opener == TokenType::letter || (opener == TokenType::length && extensions_enabled());
        push_expression(slices ? POPEFlags::parse_array_slices : POPEFlags::parse_primary);
        break;
      }
      value = primary.result;
      [[fallthrough]];
    }
    case Phase::primary_ready: {
      if (!frame.required_operand && value == ExprType::named && lexer_->peek().is_assign_op()) {
        assignments.push({value, lexer_->peek().type()});
        lexer_->chew();
        frame.flags = POPEFlags::parse_primary;
        frame.phase = Phase::leading;
        break;
      }
      if (!frame.required_operand &&
          (value == ExprType::missing || value == ExprType::array_slice)) {
        frame.phase = Phase::complete;
        break;
      }
      if (frame.required_operand && value == ExprType::missing)
        value = insert_error(Msg::expected_primary_expression, lexer_->peek());
      if (frame.prefix_increment && value != ExprType::named)
        value = insert_error(Msg::expected_named_expression, lexer_->peek());
      if (value == ExprType::named &&
          (frame.prefix_increment || lexer_->peek().is_incr_decr_op())) {
        auto const increment =
          frame.prefix_increment ? *frame.prefix_increment : lexer_->peek() == TokenType::increment;
        if (!frame.prefix_increment)
          lexer_->chew();
        auto const load = insert_load(value);
        auto const one = insert_number("1");
        auto const result = insert_arith(increment ? Opcode::add : Opcode::subtract, load, one);
        insert_store(value, result);
        value = frame.prefix_increment ? result : load;
      }
      for (; frame.negations != 0; --frame.negations)
        value = insert_negate(value);
      [[fallthrough]];
    }
    case Phase::operators: {
      auto const token = static_cast<std::size_t>(lexer_->peek().type());
      auto const op = token < binary_operators.size() ? binary_operators[token] : BinaryOperator{};
      while (operators.size() != frame.operator_base &&
             operators.back().operation.right_precedence > op.precedence) {
        auto const pending = operators.pop();
        value = insert_arith(pending.operation.opcode, pending.lhs, value);
      }
      if (op.precedence != 0) {
        operators.push({value, op});
        lexer_->chew();
        frame.phase = Phase::operand;
        break;
      }
      [[fallthrough]];
    }
    case Phase::complete: {
      if (assignments.size() != frame.assignment_base && value == ExprType::missing)
        value = insert_error(Msg::expected_assign_expression, lexer_->peek());
      while (assignments.size() != frame.assignment_base)
        store_assignment(assignments.pop());
      if (nested.empty())
        return value;

      auto& suspended = nested.back();
      auto const opener = *suspended.primary.opener;
      if (opener == TokenType::letter) {
        if (!suspended.first_argument && value == ExprType::missing)
          value = insert_error(Msg::expected_expression, lexer_->peek());
        if (!suspended.first_argument || value != ExprType::missing) {
          insert_push_param(value);
          if (lexer_->peek() == TokenType::comma) {
            lexer_->chew();
            suspended.first_argument = false;
            push_expression(POPEFlags::parse_array_slices);
            break;
          }
        }
      }
      else if (opener != TokenType::lsquare && value == ExprType::missing) {
        value = insert_error(Msg::expected_expression, lexer_->peek());
      }
      frame = suspended.parent;
      auto const primary_flags = frame.required_operand ? POPEFlags::parse_primary : frame.flags;
      value = finish_primary(suspended.primary, value, primary_flags);
      nested.pop_back();
      break;
    }
    }
  }
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
  ExprIndex const end(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::store, var - end, value - end);
  return {value.index(), ExprType::assignment};
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

auto GD::Bc::Parser::insert_branch_zero(ExprIndex condition, ExprIndex destination)
  -> GD::Bc::Parser::ExprIndex
{
  // The first operand is a value. Load a named condition before emitting its
  // use; keep the destination as an instruction index for later loop fixups.
  condition = ensure_expr_loaded(condition);
  ExprIndex result(instructions_->size());
  instructions_->emplace_back(Instruction::Opcode::branch_zero, condition - result,
                              destination - result);
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
