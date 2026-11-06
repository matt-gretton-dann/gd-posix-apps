/** \file test-vm-execution.cc
 * Exact semantic oracles for direct and parsed VM execution.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>

#include <catch2/catch.hpp>

#include <cstdint>
#include <random>
#include <sstream>
#include <string>
#include <tuple>

#include "bc.hh"
#include "live-result-plan.hh"
#include "verification.hh"

namespace {
using I = GD::Bc::Instruction;
using O = I::Opcode;
using D = I::Offset;

#if GD_BC_PROFILE
void check_typed_selection(GD::Bc::Profile::Snapshot const& before,
                           GD::Bc::Profile::Snapshot const& after,
                           GD::Bc::VM::ExecutionChecks checks)
{
  using Event = GD::Bc::Profile::Event;
  auto const typed = static_cast<std::size_t>(Event::typed_instructions_executed);
  if (checks == GD::Bc::VM::ExecutionChecks::full || !GD_BC_EXPECT_TYPED_RESULTS)
    CHECK(after.events[typed] == before.events[typed]);
  else if (!GD_BC_EXPECT_LIVE_RESULTS)
    CHECK(after.events[typed] > before.events[typed]);
  else {
    // Dead padding no longer guarantees profitable typed storage. The
    // simultaneous-live-values contract separately requires typed execution;
    // these semantic fixtures may select either valid layout after reuse.
    auto const executed = static_cast<std::size_t>(Event::instructions_executed);
    CHECK(after.events[executed] > before.events[executed]);
  }
}
#endif

auto execute(GD::Bc::Instructions code, bool typed_eligible = false) -> std::string
{
  if (typed_eligible) {
    REQUIRE_FALSE(code.empty());
    REQUIRE(code.back().opcode() == O::eof);
    // Cover fixed layout costs on each supported ABI. These unused producers
    // leave earlier data offsets and branch targets unchanged.
    code.insert(code.end() - 1, 64, I(O::number, "0"));
  }
  auto run = [&code, typed_eligible](GD::Bc::VM::ExecutionChecks checks) {
    auto independent = code;  // Independent literal caches and activation state.
    std::ostringstream output;
    std::ostringstream errors;
    GD::Bc::VM vm(output, errors, false, checks);
#if GD_BC_PROFILE
    auto const before = GD::Bc::Profile::snapshot();
#endif
    CHECK_FALSE(vm.execute(independent));
    CHECK(errors.str().empty());
#if GD_BC_PROFILE
    if (typed_eligible) {
      auto const after = GD::Bc::Profile::snapshot();
      check_typed_selection(before, after, checks);
    }
#else
    (void)typed_eligible;
#endif
    return output.str();
  };
  auto checked = run(GD::Bc::VM::ExecutionChecks::full);
  CHECK(run(GD::Bc::VM::ExecutionChecks::verified) == checked);
  return checked;
}

auto literal(GD::Bc::Instructions& code, int64_t value) -> D
{
  code.emplace_back(O::number, std::to_string(value < 0 ? -value : value));
  if (value < 0) {
    code.emplace_back(O::negate, D(-1));
  }
  return D(code.size() - 1);
}

struct Expression
{
  std::string text;
  int64_t value;
  D result;
};

auto expression(std::mt19937& rng, unsigned depth, GD::Bc::Instructions& code) -> Expression
{
  if (depth == 0 || rng() % 4 == 0) {
    int64_t const value = int64_t(rng() % 19) - 9;
    return {value < 0 ? "(" + std::to_string(value) + ")" : std::to_string(value), value,
            literal(code, value)};
  }
  auto const lhs = expression(rng, depth - 1, code);
  auto const rhs = expression(rng, depth - 1, code);
  unsigned const operation = static_cast<unsigned>(rng() % 3);
  char const symbol = operation == 0 ? '+' : operation == 1 ? '-' : '*';
  O const opcode = operation == 0 ? O::add : operation == 1 ? O::subtract : O::multiply;
  int64_t const value = operation == 0   ? lhs.value + rhs.value
                        : operation == 1 ? lhs.value - rhs.value
                                         : lhs.value * rhs.value;
  D const result = D(code.size());
  code.emplace_back(opcode, lhs.result - result, rhs.result - result);
  return {"(" + lhs.text + symbol + rhs.text + ")", value, result};
}
}  // namespace

TEST_CASE("VM binary opcodes agree with native integer arithmetic", "[bc][vm][execution]")
{
  for (int64_t lhs = -4; lhs <= 4; ++lhs) {
    for (int64_t rhs = -4; rhs <= 4; ++rhs) {
      for (O opcode : {O::add, O::subtract, O::multiply, O::divide, O::modulo, O::power, O::equals,
                       O::not_equals, O::less_than, O::less_than_equals}) {
        if (((opcode == O::divide || opcode == O::modulo) && rhs == 0) ||
            (opcode == O::power && rhs < 0)) {
          continue;  // Runtime-error and fractional reciprocal cases have separate tests.
        }
        int64_t expected = 0;
        switch (opcode) {
        case O::add:
          expected = lhs + rhs;
          break;
        case O::subtract:
          expected = lhs - rhs;
          break;
        case O::multiply:
          expected = lhs * rhs;
          break;
        case O::divide:
          expected = lhs / rhs;
          break;
        case O::modulo:
          expected = lhs % rhs;
          break;
        case O::power:
          expected = 1;
          for (int64_t n = 0; n < rhs; ++n) {
            expected *= lhs;
          }
          break;
        case O::equals:
          expected = lhs == rhs;
          break;
        case O::not_equals:
          expected = lhs != rhs;
          break;
        case O::less_than:
          expected = lhs < rhs;
          break;
        case O::less_than_equals:
          expected = lhs <= rhs;
          break;
        default:
          FAIL("unexpected binary test opcode");
        }
        INFO("lhs = " << lhs << ", rhs = " << rhs << ", opcode = " << unsigned(opcode));
        GD::Bc::Instructions code;
        D const left = literal(code, lhs);
        D const right = literal(code, rhs);
        D const at = D(code.size());
        code.emplace_back(opcode, left - at, right - at);
        code.emplace_back(O::print, D(-1), I::Stream::output);
        // Keep this oracle eligible for verified lowered execution, including
        // typed registers. Unused literals preserve the result/evaluation order.
        while (code.size() < 16)
          code.emplace_back(O::number, "0");
        code.emplace_back(O::eof);
        CHECK(execute(code) == std::to_string(expected) + "\n");
      }
    }
  }
}

TEST_CASE("Generated expressions agree across grammar bytecode and native oracles",
          "[bc][vm][execution][property]")
{
  constexpr unsigned seed = 197215;
  std::mt19937 rng(seed);
  for (unsigned sample = 0; sample < 200; ++sample) {
    GD::Bc::Instructions code;
    auto const value = expression(rng, 3, code);
    std::string const source = value.text + "\n";
    INFO("seed = " << seed << ", sample = " << sample << ", source = " << source);
    code.emplace_back(O::print, value.result - D(code.size()), I::Stream::output);
    code.emplace_back(O::eof);
    std::string const expected = std::to_string(value.value) + "\n";
    CHECK(execute(code) == expected);
    GD::Bc::Parser parser(
      std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
    auto parsed = parser.parse();
    REQUIRE(parsed != nullptr);
    CHECK(execute(*parsed) == expected);
  }
}

TEST_CASE("VM unary operations have exact observable results", "[bc][vm][execution][typed-results]")
{
  for (auto const& entry :
       {std::make_tuple(O::negate, "7", "-7"), std::make_tuple(O::abs, "7", "7"),
        std::make_tuple(O::sqrt, "81", "9"), std::make_tuple(O::scale_expr, "12.340", "3"),
        std::make_tuple(O::length, "123", "3")}) {
    GD::Bc::Instructions code{I(O::number, std::get<1>(entry)), I(std::get<0>(entry), D(-1)),
                              I(O::print, D(-1), I::Stream::output), I(O::eof)};
    CHECK(execute(code, true) == std::string(std::get<2>(entry)) + "\n");
  }
  GD::Bc::Instructions code{I(O::number, "7"), I(O::negate, D(-1)), I(O::abs, D(-1)),
                            I(O::print, D(-1), I::Stream::output), I(O::eof)};
  CHECK(execute(code, true) == "7\n");
}

TEST_CASE("VM conditional and unconditional branches select one output", "[bc][vm][execution]")
{
  for (auto condition : {"0", "1"}) {
    GD::Bc::Instructions code{I(O::number, condition),
                              I(O::branch_zero, D(-1), D(4)),
                              I(O::string, "nonzero"),
                              I(O::print, D(-1), I::Stream::output),
                              I(O::branch, D(3)),
                              I(O::string, "zero"),
                              I(O::print, D(-1), I::Stream::output),
                              I(O::eof)};
    CHECK(execute(code, true) == (std::string(condition) == "0" ? "zero" : "nonzero"));
  }
}

TEST_CASE("Bare source conditions load values in both execution modes",
          "[bc][vm][execution][conditions]")
{
  std::string const source = "n=3\ns=0\nwhile(n) {\ns+=n\nn-=1\n}\ns\n"
                             "a[2]=2\nfor(i=0;a[2];i++) {\na[2]-=1\n}\ni\n"
                             "scale=0\nif(scale) 9\nscale=2\nif(scale) 7\n"
                             "define f(x) {\nif(x) return(x)\nreturn(0)\n}\n"
                             "f(0)\nf(-3)\nif(ibase) 5\nif(obase) 6\n";
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
  auto code = parser.parse();
  REQUIRE(code != nullptr);
  CHECK(execute(*code) == "6\n2\n7\n0\n-3\n5\n6\n");
}

TEST_CASE("VM special register stores survive instruction packs", "[bc][vm][execution]")
{
  auto checks = GENERATE(GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified);
  for (O opcode : {O::scale, O::ibase, O::obase}) {
    std::ostringstream output;
    std::ostringstream errors;
    GD::Bc::VM vm(output, errors, false, checks);
    GD::Bc::Instructions store{I(opcode), I(O::number, "16"), I(O::store, D(-2), D(-1)), I(O::eof)};
    CHECK_FALSE(vm.execute(store));
    GD::Bc::Instructions load{I(opcode), I(O::load, D(-1)), I(O::print, D(-1), I::Stream::output),
                              I(O::eof)};
    CHECK_FALSE(vm.execute(load));
    CHECK(output.str() == (opcode == O::obase ? "10\n" : "16\n"));
    CHECK(errors.str().empty());
  }
}

TEST_CASE("VM diagnostic printing accepts every public result alternative", "[bc][vm][execution]")
{
  auto checks = GENERATE(GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified);
  for (auto stream : {I::Stream::output, I::Stream::error}) {
    GD::Bc::Instructions code;
    auto print = [&](I value) {
      code.push_back(std::move(value));
      code.emplace_back(O::print, D(-1), stream);
    };
    print(I(O::string, "text\n"));
    print(I(O::number, "42"));
    print(I(O::variable, GD::Bc::Variable('a')));
    print(I(O::array, GD::Bc::Array('b')));
    code.emplace_back(O::number, "2");
    print(I(O::array_element, GD::Bc::Array('b'), D(-1)));
    code.emplace_back(O::array, GD::Bc::Array('c'));
    print(I(O::load, D(-1)));
    print(I(O::ibase));
    print(I(O::obase));
    print(I(O::scale));
    code.emplace_back(O::eof);
    std::ostringstream output;
    std::ostringstream errors;
    GD::Bc::VM vm(output, errors, false, checks);
    CHECK_FALSE(vm.execute(code));
    std::string const expected = "text\n42\na\nb\nb[2]\n<ARRAY VALUES>\nibase\nobase\nscale\n";
    CHECK(output.str() == (stream == I::Stream::output ? expected : ""));
    CHECK(errors.str() == (stream == I::Stream::error ? expected : ""));
  }
}

TEST_CASE("VM special-register saving is independent of the frontend option gate",
          "[bc][vm][execution][special-registers]")
{
  auto checks = GENERATE(GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified);
  bool const save_specials = GENERATE(false, true);
  // -g is only a frontend extension; the VM constructor's mode remains a
  // callable contract in either build. Reset the radix before reading saved
  // values so the assertion also detects leaking ibase and obase changes.
  std::string const source =
    "scale=2\ndefine f() {\nscale=3\nibase=16\nobase=2\nreturn (0)\n}\n"
    "f()\ns=scale\ni=ibase\no=obase\nibase=2\nibase=1010\nobase=10\ns\ni\no\n";
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
  auto code = parser.parse();
  REQUIRE(code != nullptr);
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, save_specials, checks);
  CHECK_FALSE(vm.execute(*code));
  CHECK(output.str() == (save_specials ? "0\n2\n10\n10\n" : "0\n3\n16\n2\n"));
  CHECK(errors.str().empty());
}

TEST_CASE("Consumed parameters and terminal returns preserve snapshots",
          "[bc][vm][execution][ownership]")
{
  auto checks = GENERATE(GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified);
  // For the recursive call, r(0)=N+3 and r(k)=N+(4-k)+r(k-1)+k,
  // giving r(3)=4*N+15. Both parameter/local restoration and the caller's
  // large scalar/array snapshots must survive moving consumed frame values.
  std::string const source =
    "x=999999999999999999999999999999999999999999999999999999999999999\na[0]=x\nt=88\nz=17\ndefine "
    "g(z) {\nreturn(z)\n}\ndefine f(x,a[]) {\nauto t\nif(x==0) "
    "return(a[0])\na[0]+=1\nt=f(x-1,a[])\nreturn(a[0]+t+x)\n}\nf(3,a[])\na[0]\nx\nt\ny=g(x)\ny+="
    "1\nx\ny\ng(-1.2500)\nz\n";
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false, checks);
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), true);
  while (auto code = parser.parse()) {
    if (!vm.execute(*code)) {
      break;
    }
  }
  CHECK(errors.str().empty());
  CHECK(output.str() ==
        "4000000000000000000000000000000000000000000000000000000000000011\n999999999999999999999999"
        "999999999999999999999999999999999999999\n9999999999999999999999999999999999999999999999999"
        "99999999999999\n88\n999999999999999999999999999999999999999999999999999999999999999\n10000"
        "00000000000000000000000000000000000000000000000000000000000\n-1.2500\n17\n");
}

TEST_CASE("Repeated executable code observes replaced literal text", "[bc][vm][literal]")
{
  auto checks = GENERATE(GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified);
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false, checks);
  GD::Bc::Instructions code;
  code.emplace_back(O::number, std::string("123456789012345678901234567890"));
  code.emplace_back(O::print, D(-1), I::Stream::output);
  CHECK(vm.execute(code));
  CHECK(vm.execute(code));
  code.front().op1(std::string("987654321098765432109876543210"));
  CHECK(vm.execute(code));
  CHECK(errors.str().empty());
  CHECK(output.str() == "123456789012345678901234567890\n"
                        "123456789012345678901234567890\n"
                        "987654321098765432109876543210\n");
}

#if GD_BC_PROFILE
TEST_CASE("Configured liveness prepares verified layouts and preserves live numeric registers",
          "[bc][vm][live-results][verified-profile]")
{
  using Checks = GD::Bc::VM::ExecutionChecks;
  using Event = GD::Bc::Profile::Event;
  auto event = [](GD::Bc::Profile::Snapshot const& snapshot, Event which) {
    return snapshot.events[static_cast<std::size_t>(which)];
  };
  for (auto checks : {Checks::full, Checks::verified}) {
    GD::Bc::Instructions code;
    std::string expected;
    // Keep many distinct values simultaneously live. Unlike dead padding,
    // this also leaves typed storage profitable after liveness reuse.
    for (unsigned i = 0; i != 64; ++i) {
      code.emplace_back(O::number, std::to_string(i));
      expected += std::to_string(i) + "\n";
    }
    for (unsigned i = 0; i != 64; ++i) {
      auto const pc = code.size();
      code.emplace_back(O::print, D(i) - D(pc), I::Stream::output);
      code.emplace_back(O::number, "999");
    }
    code.emplace_back(O::eof);
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, checks);
    auto const before = GD::Bc::Profile::snapshot();
    CHECK_FALSE(vm.execute(code));
    auto const after = GD::Bc::Profile::snapshot();
    CHECK(output.str() == expected);
    CHECK(errors.str().empty());
    auto const active = checks == Checks::verified && GD_BC_EXPECT_LIVE_RESULTS;
    CHECK((event(after, Event::live_result_preparations) >
           event(before, Event::live_result_preparations)) == active);
    CHECK((event(after, Event::live_result_plan_improvements) >
           event(before, Event::live_result_plan_improvements)) == active);
    CHECK((event(after, Event::typed_instructions_executed) >
           event(before, Event::typed_instructions_executed)) ==
          (checks == Checks::verified && GD_BC_EXPECT_TYPED_RESULTS));
    auto const generic = event(after, Event::generic_result_slots_acquired) -
                         event(before, Event::generic_result_slots_acquired);
    auto const numbers = event(after, Event::number_result_slots_acquired) -
                         event(before, Event::number_result_slots_acquired);
    auto const others = event(after, Event::other_result_slots_acquired) -
                        event(before, Event::other_result_slots_acquired);
    if (checks == Checks::full) {
      CHECK(generic == code.size());
      CHECK(numbers == 0);
    }
    else if (GD_BC_EXPECT_TYPED_RESULTS) {
      CHECK(generic == 0);
      CHECK(numbers == (GD_BC_EXPECT_LIVE_RESULTS ? 64 : 128));
    }
    else {
      CHECK(generic == (GD_BC_EXPECT_LIVE_RESULTS ? 65 : 129));
      CHECK(numbers == 0);
    }
    CHECK(others == 0);
  }
}

TEST_CASE("Acquired mixed lanes report numeric and array snapshot storage independently",
          "[bc][vm][live-results][verified-profile]")
{
  using Checks = GD::Bc::VM::ExecutionChecks;
  using Event = GD::Bc::Profile::Event;
  for (auto checks : {Checks::full, Checks::verified}) {
    GD::Bc::Instructions code{I(O::array, GD::Bc::Array('a'))};
    for (unsigned i = 0; i != 64; ++i) {
      auto const pc = code.size();
      code.emplace_back(O::load, -D(pc));
    }
    auto const numeric_begin = code.size();
    for (unsigned i = 0; i != 64; ++i)
      code.emplace_back(O::number, std::to_string(i));
    std::string expected;
    for (unsigned i = 0; i != 64; ++i) {
      auto const pc = code.size();
      code.emplace_back(O::print, D(numeric_begin + i) - D(pc), I::Stream::output);
      code.emplace_back(O::print, D(1 + i) - D(pc + 1), I::Stream::output);
      code.emplace_back(O::number, "999");
      expected += std::to_string(i) + "\n<ARRAY VALUES>\n";
    }
    code.emplace_back(O::eof);
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, checks);
    auto const before = GD::Bc::Profile::snapshot();
    CHECK_FALSE(vm.execute(code));
    auto const after = GD::Bc::Profile::snapshot();
    auto acquired = [&](Event which) {
      auto const index = static_cast<std::size_t>(which);
      return after.events[index] - before.events[index];
    };
    CHECK(output.str() == expected);
    CHECK(errors.str().empty());
    if (checks == Checks::full) {
      CHECK(acquired(Event::generic_result_slots_acquired) == code.size());
      CHECK(acquired(Event::number_result_slots_acquired) == 0);
      CHECK(acquired(Event::other_result_slots_acquired) == 0);
    }
    else if (GD_BC_EXPECT_TYPED_RESULTS) {
      CHECK(acquired(Event::generic_result_slots_acquired) == 0);
      CHECK(acquired(Event::number_result_slots_acquired) ==
            (GD_BC_EXPECT_LIVE_RESULTS ? 64 : 128));
      CHECK(acquired(Event::other_result_slots_acquired) == 65);
    }
    else {
      CHECK(acquired(Event::generic_result_slots_acquired) ==
            (GD_BC_EXPECT_LIVE_RESULTS ? 129 : 194));
      CHECK(acquired(Event::number_result_slots_acquired) == 0);
      CHECK(acquired(Event::other_result_slots_acquired) == 0);
    }
  }
}

TEST_CASE("Typed execution respects configuration and profitable layout selection",
          "[bc][vm][typed-results][verified-profile]")
{
  using Checks = GD::Bc::VM::ExecutionChecks;
  for (auto checks : {Checks::full, Checks::verified}) {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, checks);
    GD::Bc::Instructions code;
    std::string expected;
    for (unsigned i = 0; i != 64; ++i) {
      code.emplace_back(O::number, "7");
      code.emplace_back(O::print, D(-1), I::Stream::output);
      expected += "7\n";
    }
    code.emplace_back(O::eof);
    auto const before = GD::Bc::Profile::snapshot();
    CHECK_FALSE(vm.execute(code));
    CHECK(output.str() == expected);
    CHECK(errors.str().empty());
    check_typed_selection(before, GD::Bc::Profile::snapshot(), checks);
  }
}

TEST_CASE("Persistent liveness is prepared once behind a tiny caller",
          "[bc][vm][live-results][verified-profile]")
{
  using Checks = GD::Bc::VM::ExecutionChecks;
  using Event = GD::Bc::Profile::Event;
  auto parse = [](std::string const& source) {
    GD::Bc::Parser parser(
      std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
    return parser.parse();
  };
  std::string body;
  for (unsigned i = 0; i != 64; ++i)
    body += "t=1\n";
  for (auto checks : {Checks::full, Checks::verified}) {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, checks);
    auto definition = parse("define f(a[]) {\nauto t\n" + body + "return(a[0]+7)\n}\n");
    REQUIRE(definition != nullptr);
    CHECK_FALSE(vm.execute(*definition));
    auto caller = parse("f(a[])\n");
    REQUIRE(caller != nullptr);
    REQUIRE(caller->size() < 16);
    auto const before = GD::Bc::Profile::snapshot();
    CHECK_FALSE(vm.execute(*caller));
    auto const first = GD::Bc::Profile::snapshot();
    auto const prepared = static_cast<std::size_t>(Event::live_result_preparations);
    auto const improved = static_cast<std::size_t>(Event::live_result_plan_improvements);
    auto const active = checks == Checks::verified && GD_BC_EXPECT_LIVE_RESULTS;
    CHECK((first.events[improved] > before.events[improved]) == active);
    // The transient caller itself also attempts bounded preparation. Only
    // the first call may additionally prepare the persistent body.
    auto const cold = first.events[prepared] - before.events[prepared];
    CHECK_FALSE(vm.execute(*caller));
    auto const second = GD::Bc::Profile::snapshot();
    auto const warm = second.events[prepared] - first.events[prepared];
    CHECK(cold == warm + (active ? 1 : 0));
    CHECK(second.events[improved] == first.events[improved]);
    CHECK(output.str() == "7\n7\n");
    CHECK(errors.str().empty());
  }
}

TEST_CASE("Verified execution omits only the result diagnostic switch",
          "[bc][vm][verified-profile]")
{
  using Checks = GD::Bc::VM::ExecutionChecks;
  auto count = [] {
    return GD::Bc::Profile::snapshot()
      .events[static_cast<std::size_t>(GD::Bc::Profile::Event::result_validation_calls)];
  };
  for (auto checks : {Checks::full, Checks::verified}) {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, checks);
    GD::Bc::Instructions code{I(O::number, "42"), I(O::print, D(-1), I::Stream::output), I(O::eof)};
    auto before = count();
    CHECK_FALSE(vm.execute(code));
    CHECK(output.str() == "42\n");
    CHECK(errors.str().empty());
    if (checks == Checks::full)
      CHECK(count() > before);
    else
      CHECK(count() == before);
  }
}
#endif

TEST_CASE("A producer outside a loop retains its value on every iteration",
          "[bc][vm][temporary-liveness]")
{
  // A sole static use is not enough: this consumer executes repeatedly while
  // its dominating producer is outside the backedge. Consuming it would print
  // the correct value once and then use a moved-from Number.
  auto const counter = GD::Bc::Variable('i');
  GD::Bc::Instructions code{
    I(O::number, "1" + std::string(100, '0')),  // 0: live across loop iterations
    I(O::variable, counter),                    // 1
    I(O::number, "0"),                          // 2
    I(O::store, D(-2), D(-1)),                  // 3
    I(O::load, D(-3)),                          // 4: loop header
    I(O::number, "3"),                          // 5
    I(O::less_than, D(-2), D(-1)),              // 6
    I(O::branch_zero, D(-1), D(9)),             // 7: exit at EOF
    I(O::number, "1"),                          // 8
    I(O::add, D(-9), D(-1)),                    // 9: sole static read of 0
    I(O::print, D(-1), I::Stream::output),      // 10
    I(O::load, D(-10)),                         // 11
    I(O::number, "1"),                          // 12
    I(O::add, D(-2), D(-1)),                    // 13
    I(O::store, D(-13), D(-1)),                 // 14
    I(O::branch, D(-11)),                       // 15
    I(O::eof)};
  std::string const line = "1" + std::string(67, '0') + "\\\n" + std::string(32, '0') + "1\n";
  CHECK(execute(code) == line + line + line);
}

TEST_CASE("Repeated operands preserve both reads of the same producer",
          "[bc][vm][temporary-liveness]")
{
  GD::Bc::Instructions code{I(O::number, "1" + std::string(100, '0')), I(O::add, D(-1), D(-1)),
                            I(O::print, D(-1), I::Stream::output)};
  // Keep this outer stream large enough for execution-plan analysis; these
  // unused literals must not create a second semantic use of the producer.
  for (unsigned n = 0; n < 16; ++n)
    code.emplace_back(O::number, "0");
  code.emplace_back(O::eof);
  CHECK(execute(code) == "2" + std::string(67, '0') + "\\\n" + std::string(33, '0') + "\n");
}

#if GD_BC_PROFILE
TEST_CASE("Last-use transfers avoid an intermediate COW clone only in verified execution",
          "[bc][vm][temporary-liveness-profile]")
{
  for (auto checks : {GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified}) {
    GD::Bc::Instructions code{I(O::number, "1" + std::string(100, '0')),
                              I(O::number, "1"),
                              I(O::add, D(-2), D(-1)),
                              I(O::number, "1"),
                              I(O::add, D(-2), D(-1)),
                              I(O::print, D(-1), I::Stream::output)};
    for (unsigned n = 0; n < 16; ++n)
      code.emplace_back(O::number, "0");
    code.emplace_back(O::eof);
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, checks);
    auto const before = GD::Bc::Profile::snapshot();
    CHECK_FALSE(vm.execute(code));
    auto const after = GD::Bc::Profile::snapshot();
    auto delta = [&](GD::Bc::Profile::Event event) {
      auto const index = static_cast<std::size_t>(event);
      return after.events[index] - before.events[index];
    };
    CHECK(output.str() == "1" + std::string(67, '0') + "\\\n" + std::string(32, '0') + "2\n");
    CHECK(errors.str().empty());
    // The literal cache retains the original coefficient in both modes. Only
    // the otherwise unique intermediate sum can avoid detaching a second time.
    if (checks == GD::Bc::VM::ExecutionChecks::full) {
      CHECK(delta(GD::Bc::Profile::Event::temporary_consumptions) == 0);
      CHECK(delta(GD::Bc::Profile::Event::cow_clones) == 2);
    }
    else {
      CHECK(delta(GD::Bc::Profile::Event::temporary_consumptions) > 0);
      CHECK(delta(GD::Bc::Profile::Event::cow_clones) == 1);
    }
  }
}
#endif

TEST_CASE("Reused activation slots release mixed result alternatives",
          "[bc][vm][execution][ownership]")
{
  auto checks = GENERATE(GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified);
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false, checks);
  std::string expected;
  std::string const coefficient(60, '9');
  for (unsigned repetition = 0; repetition < 32; ++repetition) {
    // Reuse the same activation buffer for differently sized packs and
    // owning/non-owning alternatives. Each execution must start empty,
    // independently of the previous result kind or retained capacity.
    GD::Bc::Instructions number{I(O::number, coefficient), I(O::print, D(-1), I::Stream::output),
                                I(O::eof)};
    // An eligible owning numeric pack alternates with tiny generic packs.
    // Every lane must release its snapshots before this depth slot is reused.
    for (unsigned i = 0; i != 32; ++i)
      number.insert(number.end() - 1, I(O::number, coefficient));
    CHECK_FALSE(vm.execute(number));
    expected += coefficient + "\n";
    GD::Bc::Instructions array{I(O::array, GD::Bc::Array('a')), I(O::load, D(-1)),
                               I(O::print, D(-1), I::Stream::output), I(O::eof)};
    CHECK_FALSE(vm.execute(array));
    expected += "<ARRAY VALUES>\n";
    GD::Bc::Instructions text{I(O::string, "ready\n"), I(O::print, D(-1), I::Stream::output),
                              I(O::scale), I(O::print, D(-1), I::Stream::output), I(O::eof)};
    CHECK_FALSE(vm.execute(text));
    expected += "ready\nscale\n";
  }
  CHECK(errors.str().empty());
  CHECK(output.str() == expected);
}

TEST_CASE("Compact control flow preserves exact forward targets and a backedge to zero",
          "[bc][vm][execution][compact-executable]")
{
  SECTION("Forward data reference through a later producer")
  {
    GD::Bc::Instructions code{I(O::branch, D(4)),
                              I(O::print, D(3), I::Stream::output),
                              I(O::number, "0"),
                              I(O::branch, D(4)),
                              I(O::number, "7"),
                              I(O::branch, D(-4)),
                              I(O::number, "0"),
                              I(O::number, "9"),
                              I(O::print, D(-1), I::Stream::output)};
    while (code.size() < 16)
      code.emplace_back(O::number, "0");
    code.emplace_back(O::eof);
    CHECK(execute(code, true) == "7\n9\n");
  }
  SECTION("Zero is a valid absolute branch target")
  {
    GD::Bc::Instructions code{I(O::variable, GD::Bc::Variable('i')),
                              I(O::load, D(-1)),
                              I(O::number, "1"),
                              I(O::add, D(-2), D(-1)),
                              I(O::store, D(-4), D(-1)),
                              I(O::number, "3"),
                              I(O::less_than, D(-3), D(-1)),
                              I(O::branch_zero, D(-1), D(2)),
                              I(O::branch, D(-8)),
                              I(O::print, D(-6), I::Stream::output)};
    while (code.size() < 16)
      code.emplace_back(O::number, "0");
    code.emplace_back(O::eof);
    CHECK(execute(code, true) == "3\n");
  }
}

TEST_CASE("Typed nested calls retain snapshots and restore special registers",
          "[bc][vm][execution][typed-results]")
{
  auto checks = GENERATE(GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified);
  std::string body = "auto t\n";
  for (unsigned i = 0; i != 64; ++i)
    body += "t=0\n";
  // Every recursive function activation is eligible independently of the
  // top-level stream. Changing radix at the leaf must not change the caller's
  // literal interpretation, and passing an array must retain its snapshot.
  std::string const source = "scale=2\na[0]=7\nx=99\ndefine f(x,a[]) {\n" + body +
                             "if(x==0) {\nscale=3\nibase=16\nreturn(a[0])\n}\n"
                             "a[0]+=1\nt=f(x-1,a[])\nreturn(a[0]+t+x)\n}\n"
                             "f(3,a[])\na[0]\nx\nscale\nibase\n";
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
  auto code = parser.parse();
  REQUIRE(code != nullptr);
  std::ostringstream output, errors;
  GD::Bc::VM vm(output, errors, true, checks);
#if GD_BC_PROFILE
  auto const before = GD::Bc::Profile::snapshot();
#endif
  CHECK_FALSE(vm.execute(*code));
  CHECK(output.str() == "43\n7\n99\n2\n10\n");
  CHECK(errors.str().empty());
#if GD_BC_PROFILE
  auto const after = GD::Bc::Profile::snapshot();
  check_typed_selection(before, after, checks);
#endif
  auto call = [&] {
    GD::Bc::Parser caller(
      std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>("f(1,a[])\n")), false);
    auto tiny = caller.parse();
    REQUIRE(tiny != nullptr);
    REQUIRE(tiny->size() < 16);
#if GD_BC_PROFILE
    auto const count_before = GD::Bc::Profile::snapshot();
#endif
    CHECK_FALSE(vm.execute(*tiny));
#if GD_BC_PROFILE
    auto const count_after = GD::Bc::Profile::snapshot();
    // The caller is ineligible. Without liveness, this proves execution of
    // the function's lanes; reuse can instead make its generic plan cheaper.
    check_typed_selection(count_before, count_after, checks);
#endif
  };
  call();
  CHECK(output.str() == "43\n7\n99\n2\n10\n17\n");
  std::string const replacement = "define f(x,a[]) {\n" + body + "return(42)\n}\n";
  GD::Bc::Parser redefine(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(replacement)), false);
  auto definition = redefine.parse();
  REQUIRE(definition != nullptr);
  CHECK_FALSE(vm.execute(*definition));
  call();
  CHECK(output.str() == "43\n7\n99\n2\n10\n17\n42\n");
  CHECK(errors.str().empty());
}

TEST_CASE("Compact short functions keep runtime radix and owned literal sidecars",
          "[bc][vm][execution][compact-executable]")
{
  std::string const source = "define f(x) {\nreturn(x+1)\n}\nf(1)\nibase=16\nf(A)\n"
                             "ibase=A\nf(10)\n";
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
  auto code = parser.parse();
  REQUIRE(code != nullptr);
  for (auto const& instruction : *code)
    REQUIRE(instruction.opcode() != O::quit);
  CHECK(execute(*code) == "2\n11\n11\n");
}

TEST_CASE("A call may overwrite its forwarded argument only after consuming the borrow",
          "[bc][vm][live-results][execution]")
{
  auto const location = GD::Bc::Location("borrow", 1, 1);
  GD::Bc::Instructions caller;
  for (unsigned i = 0; i != 20; ++i)
    caller.emplace_back(O::number, "0");
  caller.emplace_back(O::push_param_mark);
  auto const argument = caller.size();
  caller.emplace_back(O::number, "123456789012345678901234567890");
  caller.emplace_back(O::push_param, D(-1));
  auto const call = caller.size();
  caller.emplace_back(O::call, GD::Bc::Letter('f'), location);
  caller.emplace_back(O::print, D(-1), I::Stream::output);
  caller.emplace_back(O::eof);
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(caller, false).has_value());
  auto fallback = GD::Bc::Details::plan_result_slots(caller, 48);
  auto slots = GD::Bc::Details::plan_live_result_slots(caller, std::move(fallback), 48, false);
  REQUIRE_FALSE(slots.indices.empty());
  REQUIRE(slots.index(argument) == slots.index(call));
  for (auto checks : {GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified}) {
    GD::Bc::Parser parser(std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(
                            "define f(x) {\nreturn(x+1)\n}\n")),
                          false);
    auto definition = parser.parse();
    REQUIRE(definition != nullptr);
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, checks);
    CHECK_FALSE(vm.execute(*definition));
    auto independent = caller;
    CHECK_FALSE(vm.execute(independent));
    CHECK_FALSE(vm.execute(independent));
    CHECK(errors.str().empty());
    CHECK(output.str() == "123456789012345678901234567891\n123456789012345678901234567891\n");
  }
}

TEST_CASE("Function bodies can retain an incoming parameter result", "[bc][vm][execution]")
{
  auto const loc = GD::Bc::Location("fixture", 1, 1);
  auto const name = GD::Bc::Letter('f');
  GD::Bc::Instructions code{I(O::function_begin, GD::Bc::VariableMask("x", ""), loc),
                            I(O::pop_param),
                            I(O::variable, GD::Bc::Variable('x')),
                            I(O::store, D(-1), D(-2)),
                            I(O::pop_param_mark),
                            I(O::return_, D(-4)),
                            I(O::function_end, name, D(-6)),
                            I(O::push_param_mark),
                            I(O::number, "7"),
                            I(O::push_param, D(-1)),
                            I(O::call, name, loc),
                            I(O::print, D(-1), I::Stream::output),
                            I(O::eof)};
  CHECK(execute(code) == "7\n");
}
