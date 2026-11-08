/** Independent values, malformed plans and lifetime cuts for the word core.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <array>
#include <charconv>
#include <limits>
#include <random>
#include <sstream>
#include <string>
#include <vector>

#include "bc.hh"
#include "word-function.hh"

namespace {
using namespace GD::Bc;
namespace W = GD::Bc::Details::Word;
using O = W::Opcode;
using I = W::Instruction;
using V = std::int64_t;
constexpr auto x = W::local_tag;
constexpr auto y = W::local_tag | 1;

auto decimal(Number const& value) -> std::string
{
  std::ostringstream out;
  value.output(out, 10, 0);
  return out.str();
}
auto number(V value) -> Number
{
  auto text = std::to_string(value);
  Number result(value < 0 ? text.substr(1) : text, 10);
  if (value < 0)
    result.negate();
  return result;
}
auto small(Number const& value) -> std::optional<V>
{
  auto text = decimal(value);
  V result{};
  auto parsed = std::from_chars(text.data(), text.data() + text.size(), result);
  if (parsed.ec != std::errc{} || parsed.ptr != text.data() + text.size())
    return {};
  return result;
}
auto binary(O op, Number left, Number const& right) -> Number
{
  switch (op) {
  case O::add:
    left.add(right);
    break;
  case O::subtract:
    left.sub(right);
    break;
  case O::multiply:
    left.multiply(right, 0);
    break;
  case O::less:
    return Number(left < right ? 1 : 0);
  case O::less_equal:
    return Number(left <= right ? 1 : 0);
  case O::equal:
    return Number(left == right ? 1 : 0);
  case O::not_equal:
    return Number(left != right ? 1 : 0);
  default:
    FAIL("Unexpected test arithmetic");
  }
  return left;
}

// Only tests see Number here. The executor transports stable integer indices
// through the callbacks, including across reallocations and recursive returns.
struct Bank
{
  std::vector<Number> values;
  std::size_t bytes = 0;
  std::size_t promotions = 0;
  std::size_t demotions = 0;
  std::size_t stop_at = std::numeric_limits<std::size_t>::max();
  std::size_t polls = 0;
  bool fail_promotion = false;
  auto get(W::Value value) const -> Number
  {
    return value.big ? values.at(static_cast<std::size_t>(value.payload)) : number(value.payload);
  }
  auto save(Number value, W::Value& target, std::size_t available) -> bool
  {
    if (auto word = small(value)) {
      target = {*word, false};
      ++demotions;
      return true;
    }
    if (fail_promotion)
      throw std::bad_alloc();
    auto charge = sizeof(Number) + std::size_t(value.length()) * sizeof(Number::NumType);
    if (value.length() > 4096 || charge > available)
      return false;
    auto index = values.size();
    values.push_back(std::move(value));
    bytes += charge;
    ++promotions;
    target = {static_cast<V>(index), true};
    return true;
  }
  auto callbacks() -> W::Callbacks
  {
    return {this,
            [](void* p) noexcept {
              auto& b = *static_cast<Bank*>(p);
              return b.polls++ == b.stop_at;
            },
            [](void* p) noexcept { return static_cast<Bank*>(p)->bytes; },
            [](void* p, O op, W::Value a, W::Value b, W::Value& result, std::size_t available) {
              auto& bank = *static_cast<Bank*>(p);
              return bank.save(binary(op, bank.get(a), bank.get(b)), result, available);
            },
            [](void* p, W::Value a, W::Value& result, std::size_t available) {
              auto& bank = *static_cast<Bank*>(p);
              auto value = bank.get(a);
              value.negate();
              return bank.save(std::move(value), result, available);
            }};
  }
};
auto prepare(std::initializer_list<I> code) -> W::Program
{
  auto program = W::Program::prepare(std::span<I const>(code.begin(), code.size()), 0);
  REQUIRE(program);
  return std::move(*program);
}
auto factorial() -> W::Program
{
  return prepare({{O::constant, 1},
                  {O::less_equal, x, 0},
                  {O::branch_zero, 1, 5},
                  {O::constant, 1},
                  {O::return_, 3},
                  {O::mark},
                  {O::constant, 1},
                  {O::subtract, x, 6},
                  {O::push, 7},
                  {O::call},
                  {O::multiply, x, 9},
                  {O::return_, 10}});
}
auto with_literal(O op, V rhs) -> W::Program
{
  std::vector<I> code{{O::constant, 9}, {O::constant, 1}, {O::add, 0, 1}, {O::constant, 0}};
  // Construct the independent decimal operand through ordinary decoded
  // operations. This also crosses signed-minimum promotion/demotion edges.
  auto text = std::to_string(rhs);
  auto index = std::uint16_t(3);
  for (char digit : text) {
    if (digit == '-')
      continue;
    auto next = static_cast<std::uint16_t>(code.size());
    code.push_back({O::multiply, index, 2});
    code.push_back({O::constant, static_cast<std::uint16_t>(digit - '0')});
    code.push_back({O::add, next, static_cast<std::uint16_t>(next + 1)});
    index = static_cast<std::uint16_t>(next + 2);
  }
  if (rhs < 0) {
    code.push_back({O::negate, index});
    index = static_cast<std::uint16_t>(code.size() - 1);
  }
  auto result = static_cast<std::uint16_t>(code.size());
  code.push_back({op, x, index});
  code.push_back({O::return_, result});
  auto program = W::Program::prepare(code, 0);
  REQUIRE(program);
  return std::move(*program);
}
void empty(W::Scratch const& scratch)
{
  CHECK(scratch.values.empty());
  CHECK(scratch.frames.empty());
  CHECK(scratch.arguments.empty());
}
}  // namespace

TEST_CASE("Word plans reject malformed operands, control joins and argument marks", "[bc][word]")
{
  using Code = std::vector<I>;
  for (auto const& code :
       std::vector<Code>{{},
                         {{O::return_, 0}},
                         {{O::return_, W::local_tag | 26}},
                         {{O::constant, 10}, {O::return_, 0}},
                         {{O::load, x}},
                         {{O::branch, 9}},
                         {{static_cast<O>(255)}},
                         {{O::constant, 1}, {O::store, 0, 0}, {O::return_, 0}},
                         {{O::location}, {O::load, 0}, {O::return_, 1}},
                         {{O::mark}, {O::return_, x}},
                         {{O::push, x}, {O::return_, x}},
                         {{O::mark}, {O::call}, {O::return_, 1}},
                         {{O::mark}, {O::push, x}, {O::push, x}, {O::call}, {O::return_, 3}},
                         // The branch skips a producer that is valid only on the fallthrough.
                         {{O::branch_zero, x, 2}, {O::constant, 1}, {O::return_, 1}},
                         // Different pending argument state reaches the same return.
                         {{O::branch_zero, x, 2}, {O::mark}, {O::return_, x}},
                         // The backedge cannot authorize a read before its first production.
                         {{O::branch, 2}, {O::constant, 1}, {O::load, 1}, {O::branch, 1}},
                         // Invalid unreachable instructions must not evade validation.
                         {{O::return_, x}, {static_cast<O>(255)}}}) {
    CHECK_FALSE(W::Program::prepare(code, 0));
  }
  CHECK_FALSE(W::Program::prepare(std::array<I, 1>{{{O::return_, x}}}, 26));
  Code oversized(W::instruction_limit + 1, {O::return_, x});
  CHECK_FALSE(W::Program::prepare(oversized, 0));
  auto valid = prepare({{O::return_, x}});
  auto moved = std::move(valid);
  W::Scratch scratch;
  W::Counters counters;
  CHECK_FALSE(W::execute(valid, 2, scratch, {}, counters));
  CHECK(W::execute(moved, 2, scratch, {}, counters)->payload == 2);
  empty(scratch);
}

TEST_CASE("Checked word operations match independent small native arithmetic", "[bc][word]")
{
  for (O op : {O::add, O::subtract, O::multiply, O::less, O::less_equal, O::equal, O::not_equal}) {
    for (unsigned digit = 0; digit != 10; ++digit) {
      auto program =
        prepare({{O::constant, static_cast<std::uint16_t>(digit)}, {op, x, 0}, {O::return_, 1}});
      for (V input = -80; input <= 80; ++input) {
        V expected{};
        switch (op) {
        case O::add:
          expected = input + digit;
          break;
        case O::subtract:
          expected = input - digit;
          break;
        case O::multiply:
          expected = input * digit;
          break;
        case O::less:
          expected = input < V(digit);
          break;
        case O::less_equal:
          expected = input <= V(digit);
          break;
        case O::equal:
          expected = input == V(digit);
          break;
        case O::not_equal:
          expected = input != V(digit);
          break;
        default:
          FAIL("Unexpected operation");
        }
        W::Scratch scratch;
        W::Counters counters;
        auto result = W::execute(program, input, scratch, {}, counters);
        REQUIRE(result);
        CHECK_FALSE(result->big);
        CHECK(result->payload == expected);
        empty(scratch);
      }
    }
  }
}

TEST_CASE("Opaque promotion and signed endpoints preserve exact arithmetic", "[bc][word]")
{
  std::array<V, 12> const inputs{{std::numeric_limits<V>::min(), std::numeric_limits<V>::min() + 1,
                                  -3037000500LL, -9, -1, 0, 1, 9, 3037000499LL, 3037000500LL,
                                  std::numeric_limits<V>::max() - 1,
                                  std::numeric_limits<V>::max()}};
  for (O op : {O::add, O::subtract, O::multiply, O::less, O::less_equal, O::equal, O::not_equal}) {
    for (bool same : {false, true}) {
      auto program = prepare(
        {{O::constant, 9}, {op, x, static_cast<std::uint16_t>(same ? x : 0)}, {O::return_, 1}});
      for (auto input : inputs) {
        auto expected = binary(op, number(input), same ? number(input) : Number(9));
        Bank bank;
        W::Scratch scratch;
        W::Counters counters;
        auto result = W::execute(program, input, scratch, bank.callbacks(), counters);
        REQUIRE(result);
        CHECK(decimal(bank.get(*result)) == decimal(expected));
        CHECK(result->big == !small(expected).has_value());
        empty(scratch);
        if (!small(expected))
          CHECK_FALSE(W::execute(program, input, scratch, {}, counters));
      }
    }
  }
  auto cancel = prepare({{O::multiply, x, x}, {O::subtract, 0, 0}, {O::return_, 1}});
  Bank bank;
  W::Scratch scratch;
  W::Counters counters;
  auto result =
    W::execute(cancel, std::numeric_limits<V>::max(), scratch, bank.callbacks(), counters);
  REQUIRE(result);
  CHECK_FALSE(result->big);
  CHECK(result->payload == 0);
  CHECK(bank.promotions == 1);
  CHECK(bank.demotions == 1);
  auto negate = prepare({{O::negate, x}, {O::return_, 0}});
  result = W::execute(negate, std::numeric_limits<V>::min(), scratch, bank.callbacks(), counters);
  REQUIRE(result);
  CHECK(decimal(bank.get(*result)) == "9223372036854775808");
  empty(scratch);
}

TEST_CASE("Independent signed operand pairs cross every checked arithmetic edge", "[bc][word]")
{
  std::array<V, 14> const inputs{
    {std::numeric_limits<V>::min(), std::numeric_limits<V>::min() + 1, -3037000500LL, -3037000499LL,
     -9, -1, 0, 1, 9, 3037000499LL, 3037000500LL, std::numeric_limits<V>::max() / 2,
     std::numeric_limits<V>::max() - 1, std::numeric_limits<V>::max()}};
  for (O op : {O::add, O::subtract, O::multiply, O::less, O::less_equal, O::equal, O::not_equal}) {
    for (auto rhs : inputs) {
      auto program = with_literal(op, rhs);
      for (auto lhs : inputs) {
        Bank bank;
        W::Scratch scratch;
        W::Counters counters;
        auto result = W::execute(program, lhs, scratch, bank.callbacks(), counters);
        REQUIRE(result);
        CHECK(decimal(bank.get(*result)) == decimal(binary(op, number(lhs), number(rhs))));
        empty(scratch);
      }
    }
  }
}

TEST_CASE("Word locations remain lazy while loads snapshot and scratch resets", "[bc][word]")
{
  auto program = prepare({{O::load, x},
                          {O::constant, 7},
                          {O::store, x, 1},
                          {O::subtract, x, 0},
                          {O::add, 3, y},
                          {O::return_, 4}});
  W::Scratch scratch;
  W::Counters counters;
  for (V input : {1, 8, -5, 1}) {
    auto result = W::execute(program, input, scratch, {}, counters);
    REQUIRE(result);
    CHECK(result->payload == 7 - input);
    empty(scratch);
  }
}

TEST_CASE("Explicit continuations retain promoted recursive results", "[bc][word]")
{
  auto program = factorial();
  Bank bank;
  W::Scratch scratch;
  W::Counters counters;
  auto result = W::execute(program, 50, scratch, bank.callbacks(), counters);
  REQUIRE(result);
  CHECK(decimal(bank.get(*result)) ==
        "30414093201713378043612608166064768844377641568960512000000000000");
  CHECK(counters.self_calls == 49);
  CHECK(bank.promotions > 1);
  empty(scratch);
  auto instructions = counters.instructions;
  // Cut every dispatch boundary, including prepared arguments, recursive
  // frames and big results. Each later retry starts with empty live scratch.
  for (std::size_t cut = 0; cut <= instructions; ++cut) {
    Bank interrupted;
    interrupted.stop_at = cut;
    W::Counters cuts;
    CHECK_FALSE(W::execute(program, 50, scratch, interrupted.callbacks(), cuts));
    empty(scratch);
  }
  Bank retry;
  result = W::execute(program, 5, scratch, retry.callbacks(), counters);
  REQUIRE(result);
  CHECK(result->payload == 120);
  empty(scratch);
}

TEST_CASE("Fuel, scratch, spill failures and exceptions leave reusable state", "[bc][word]")
{
  auto loop = prepare({{O::branch, 0}});
  W::Scratch scratch;
  W::Counters counters;
  CHECK_FALSE(W::execute(loop, 0, scratch, {}, counters, W::scratch_limit, 9));
  CHECK(counters.instructions == 9);
  empty(scratch);
  auto program = factorial();
  Bank bank;
  CHECK_FALSE(W::execute(program, 10000, scratch, bank.callbacks(), counters));
  empty(scratch);
  CHECK_FALSE(W::execute(program, 5, scratch, {}, counters, 0));
  empty(scratch);
  CHECK_FALSE(W::execute(program, 5, scratch, {}, counters, W::scratch_limit + 1));
  empty(scratch);
  CHECK_FALSE(W::execute(program, 5, scratch, {}, counters, W::scratch_limit, W::fuel_limit + 1));
  empty(scratch);
  Bank failing;
  failing.fail_promotion = true;
  CHECK_THROWS_AS(W::execute(program, 50, scratch, failing.callbacks(), counters), std::bad_alloc);
  empty(scratch);
  Bank oversized;
  oversized.bytes = W::scratch_limit + 1;
  CHECK_FALSE(W::execute(program, 2, scratch, oversized.callbacks(), counters));
  empty(scratch);
  // Even a broken callback's accounting cannot authorize an over-budget
  // successful return from the core.
  auto square = prepare({{O::multiply, x, x}, {O::return_, 0}});
  auto broken = bank.callbacks();
  broken.binary = [](void* p, O, W::Value, W::Value, W::Value& out, std::size_t) {
    static_cast<Bank*>(p)->bytes = W::scratch_limit + 1;
    out = {1, false};
    return true;
  };
  CHECK_FALSE(W::execute(square, std::numeric_limits<V>::max(), scratch, broken, counters));
  empty(scratch);
  auto simple = prepare({{O::return_, x}});
  auto result = W::execute(simple, 37, scratch, {}, counters);
  REQUIRE(result);
  CHECK(result->payload == 37);
  empty(scratch);
}

TEST_CASE("Mutated decoded programs cannot escape validation or execution bounds", "[bc][word]")
{
  auto source = factorial();
  std::mt19937 random(690197);
  W::Scratch scratch;
  for (unsigned trial = 0; trial != 2000; ++trial) {
    auto code = std::vector<I>(source.instructions().begin(), source.instructions().end());
    auto& instruction = code[random() % code.size()];
    switch (trial % 3) {
    case 0:
      instruction.opcode = static_cast<O>(random() % 23);
      break;
    case 1:
      instruction.left = static_cast<std::uint16_t>(random());
      break;
    case 2:
      instruction.right = static_cast<std::uint16_t>(random());
      break;
    }
    auto program = W::Program::prepare(code, 0);
    if (program) {
      W::Counters counters;
      (void)W::execute(*program, V(random() % 100) - 50, scratch, {}, counters, W::scratch_limit,
                       128);
      CHECK(counters.instructions <= 128);
      empty(scratch);
    }
  }
}

#if GD_BC_PROFILE
TEST_CASE("Word plans and retained scratch release selected tracked storage", "[bc][word]")
{
  auto const before = Profile::snapshot();
  auto const plans = static_cast<std::size_t>(Profile::AllocationKind::execution_plans);
  auto const frames = static_cast<std::size_t>(Profile::AllocationKind::frames);
  {
    auto program = factorial();
    CHECK(Profile::snapshot().allocations[plans].live_bytes > before.allocations[plans].live_bytes);
    W::Scratch scratch;
    W::Counters counters;
    auto result = W::execute(program, 10, scratch, {}, counters);
    REQUIRE(result);
    CHECK(result->payload == 3628800);
    empty(scratch);
    CHECK(Profile::snapshot().allocations[frames].live_bytes >
          before.allocations[frames].live_bytes);
  }
  CHECK(Profile::snapshot().total.live_bytes == before.total.live_bytes);
}
#endif
