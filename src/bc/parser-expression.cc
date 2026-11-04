/** \file parser-expression.cc
 * Parse expressions with explicit continuations and emit VM instructions.
 * Copyright 2021, Matthew Gretton-Dann
 * SPDX-License-Identifier: Apache-2.0
 */

#include "gd/stdlib.h"

#include <array>
#include <optional>
#include <utility>
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
  std::optional<TokenType> opener{};
  Letter letter{'a'};
  std::optional<Location> location{};
};

auto GD::Bc::Parser::begin_primary(TokenType token) -> Primary
{
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
      auto token = lexer_->peek().type();
      while (token == TokenType::subtract) {
        lexer_->chew();
        ++frame.negations;
        frame.required_operand = true;
        token = lexer_->peek().type();
      }
      frame.prefix_increment.reset();
      if (token == TokenType::increment || token == TokenType::decrement) {
        frame.prefix_increment = token == TokenType::increment;
        lexer_->chew();
        frame.required_operand = true;
        token = lexer_->peek().type();
      }
      auto primary = begin_primary(token);
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
