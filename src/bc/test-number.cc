/** \file   test-number.cc
 *  \brief  Tests for bc's arbitrary precision arithmetic
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <array>
#include <cstdint>
#include <limits>
#include <random>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>

#include "bc.hh"
#include "benchmark-support.hh"

TEST_CASE("GD::Bc::Number - Number construction, directed", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [in_num, ibase, out_num,
        obase] = GENERATE(table<std::string_view, uint64_t, std::string_view, uint64_t>({
    {"0", 10, "0", 10},
    {"0.0", 10, "0", 10},
    {"1.0", 10, "1.0", 10},
    {".", 10, "0", 10},
    {".1", 10, "0.1", 10},
    {".0", 10, "0", 10},
    {"1", 10, "1", 10},
    {"00000000000000000000000000000000000000000000000000000000000000001", 10, "1", 10},
    {"123456789012345678901234567890", 10, "123456789012345678901234567890", 10},
    {"F", 10, "15", 10},
    {"0.A", 16, "0.6", 10},
    {"0.A0", 16, "0.62", 10},
    {"0.A00", 16, "0.625", 10},
    {"0.A000", 16, "0.6250", 10},
    {"0.A00", 16, "0.A00", 16},
    {"0.A", 16, "0.9", 16},   /* This is a surprise :-) */
    {"0.A0", 16, "0.9E", 16}, /* Another surprise. */
    {"0.1", 10, "0.0001", 2},
    {"12345678901234567890.12345678901234567890", 10,
     " 012 345 678 901 234 567 890.123 456 789 012 345 678 900", 1000},
    {"123456789123456789.123456789123456789", 10,
     " 123 456 789 123 456 789.123 456 789 123 456 789", 1000},
    {"FFFFFFFFFFFFFFFF", 16, "18446744073709551615", 10},
    {"FFFFFFFFFFFFFFFF", 16, "1111111111111111111111111111111111111111111111111111111111111111", 2},
    {"FFFFFFFFFFFFFFFF", 16, " 255 255 255 255 255 255 255 255", 256},
    {"FFFFFFFFFFFFFFFF", 16, " 65535 65535 65535 65535", 65536},
    {"FFFFFFFFFFFFFFFF", 16, " 00065535 16777215 16777215", 16777216},
    {".12345678", 10, "0.12 34 56 78", 100},
    {".12345678", 10, "0.123 456 780", 1000},
    {".123456789", 10, "0.123 456 789", 1000},
  }));
  if (obase < GD::Bc::Number::base_) {
    GD::Bc::Number const num(in_num, static_cast<GD::Bc::Number::NumType>(ibase));
    std::ostringstream ss;
    std::string expected(out_num);
    if (GD::Bc::extensions_enabled() && expected.substr(0, 2) == "0.") {
      expected = expected.substr(1);
    }
    num.output(ss, static_cast<GD::Bc::Number::NumType>(obase), 0);
    INFO("in_num = " << in_num << " ibase = " << ibase << " out_num = " << out_num
                     << " obase = " << obase);
    REQUIRE(ss.str() == expected);
  }
}

/* Make a number from a potentially signed string.
 *
 * GD::Bc::Number doesn't handle the sign.
 */
auto make_number(std::string_view s, unsigned base) -> GD::Bc::Number
{
  bool negate = false;
  if (s[0] == '-') {
    negate = true;
    s = s.substr(1);
  }
  GD::Bc::Number n(s, base);
  if (negate) {
    n.negate();
  }
  return n;
}

void test_add_subtract(const GD::Bc::Number& lhs, const GD::Bc::Number& rhs,
                       const GD::Bc::Number& expected_add, GD::Bc::Number expected_sub)
{
  INFO("lhs = " << lhs << " rhs = " << rhs << " expected_add = " << expected_add
                << " expected_sub = " << expected_sub);

  GD::Bc::Number result = lhs;
  result.add(rhs);
  REQUIRE(result == expected_add);

  result.sub(rhs);
  REQUIRE(result == lhs);

  result.sub(rhs);
  REQUIRE(result == expected_sub);

  result.add(rhs);
  REQUIRE(result == lhs);

  result = rhs;
  expected_sub.negate();
  result.add(lhs);
  REQUIRE(result == expected_add);

  result.sub(lhs);
  REQUIRE(result == rhs);

  result.sub(lhs);
  REQUIRE(result == expected_sub);

  result.add(lhs);
  REQUIRE(result == rhs);
}

TEST_CASE("GD::Bc::Number - Addition and subtraction, directed", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [lhs, rhs, expected_add, expected_sub] =
    GENERATE(table<std::string_view, std::string_view, std::string_view, std::string_view>({
      {"1", "2", "3", "-1"},
      {"0", "0", "0", "0"},
      {"1", "0", "1", "1"},
      {"0", "1", "1", "-1"},
      {"1", "0.2", "1.2", "0.8"},
      {"999999999", "1", "1000000000", "999999998"},
      {"0.999999999", "0.000000001", "1.000000000", "0.999999998"},
      {"999999999999999999", "1000000000000000000", "1999999999999999999", "-1"},
      {"1004.0000611", "1004", "2008.0000611", "0.0000611"},
      {"1001.0000000561", "1001", "2002.0000000561", "0.0000000561"},
      {"31.6859590355097190", "0.000000000000000030", "31.685959035509719030",
       "31.685959035509718970"},
    }));
  constexpr unsigned working_base = 10;
  GD::Bc::Number lhs_num = make_number(lhs, working_base);
  GD::Bc::Number rhs_num = make_number(rhs, working_base);
  GD::Bc::Number expected_add_num = make_number(expected_add, working_base);
  GD::Bc::Number expected_sub_num = make_number(expected_sub, working_base);

  test_add_subtract(lhs_num, rhs_num, expected_add_num, expected_sub_num);
  rhs_num.negate();
  // NOLINTNEXTLINE(readability-suspicious-call-argument)
  test_add_subtract(lhs_num, rhs_num, expected_sub_num, expected_add_num);
  lhs_num.negate();
  expected_add_num.negate();
  expected_sub_num.negate();
  test_add_subtract(lhs_num, rhs_num, expected_add_num, expected_sub_num);
  rhs_num.negate();
  // NOLINTNEXTLINE(readability-suspicious-call-argument)
  test_add_subtract(lhs_num, rhs_num, expected_sub_num, expected_add_num);
}

TEST_CASE("GD::Bc::Number - Scale and length, directed", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [num, scale, length] =
    GENERATE(table<std::string_view, GD::Bc::Number::NumType, GD::Bc::Number::NumType>(
      {{"0.01", 2, 1},
       {".1", 1, 1},
       {"1", 0, 1},
       {"0", 0, 1},
       {"00000000000000000000000000000000000000001", 0, 1},
       {"0.00000000000", 11, 11},
       {"1.0000", 4, 5},
       {"123456789123456789123456789", 0, 27}}));
  constexpr unsigned working_base = 10;
  GD::Bc::Number const n(num, working_base);
  REQUIRE(n.scale() == scale);
  REQUIRE(n.length() == length);
}

namespace {

using Number8 = GD::Bc::BasicNumber<GD::Bc::NumberTraits8>;
using UInt64Pair = std::pair<uint64_t, uint64_t>;

// Unit tests start at BasicDigits' default threshold. Restore it even when a
// REQUIRE aborts a generated case, so later tests never inherit a tuned value.
template<typename Number>
struct RestoreMultiplyThreshold
{
  ~RestoreMultiplyThreshold() { Number::multiply_split_point(1); }
};

class RandomUIntPairGenerator : public Catch::Generators::IGenerator<UInt64Pair>
{
public:
  RandomUIntPairGenerator()
      : rand_(Catch::rngSeed()), dist_(0, std::numeric_limits<uint64_t>::max())
  {
    RandomUIntPairGenerator::next();
  }

  [[nodiscard]] auto get() const -> UInt64Pair const& override { return pair_; }

  auto next() -> bool override
  {
    pair_ = std::make_pair(dist_(rand_), dist_(rand_));
    return true;
  }

private:
  std::mt19937_64 rand_;
  std::uniform_int_distribution<uint64_t> dist_;
  UInt64Pair pair_;
};

auto random_pair() -> Catch::Generators::GeneratorWrapper<UInt64Pair>
{
  return {std::make_unique<RandomUIntPairGenerator>()};
}

class RandomNumberGenerator : public Catch::Generators::IGenerator<GD::Bc::Number>
{
public:
  RandomNumberGenerator()
      : rand_(Catch::rngSeed() + 1), dist_(GD::Bc::Bench::limb_distribution<GD::Bc::Number>())
  {
    RandomNumberGenerator::next();
  }

  [[nodiscard]] auto get() const -> GD::Bc::Number const& override { return number_; }

  auto next() -> bool override
  {
    constexpr auto max_digits = 40 * GD::Bc::Number::base_log10_;
    constexpr unsigned working_base = 10;
    auto digit_count = dist_(rand_) % max_digits;
    auto int_len = dist_(rand_) % (digit_count + 1);

    std::string num;
    GD::Bc::Number::NumType digits = 0;
    for (decltype(digit_count) i = 0; i < digit_count; ++i) {
      if (i % GD::Bc::Number::base_log10_ == 0) {
        digits = dist_(rand_);
      }
      auto digit = digits / (GD::Bc::Number::base_ / working_base);
      digits = (digits % (GD::Bc::Number::base_ / working_base)) * working_base;
      assert(digit < working_base);  // NOLINT

      if (num.size() == int_len) {
        num += '.';
      }
      num += static_cast<char>('0' + digit);
    }

    number_ = GD::Bc::Number(num, working_base);
    return true;
  }

private:
  std::mt19937_64 rand_;
  std::uniform_int_distribution<GD::Bc::Number::NumType> dist_;
  GD::Bc::Number number_;
};

auto random_number() -> Catch::Generators::GeneratorWrapper<GD::Bc::Number>
{
  return {std::make_unique<RandomNumberGenerator>()};
}

class RandomNumberPairGenerator
    : public Catch::Generators::IGenerator<std::pair<GD::Bc::Number, GD::Bc::Number>>
{
public:
  explicit RandomNumberPairGenerator(GD::Bc::Number::NumType max_digits = default_max_digits_)
      : rand_(Catch::rngSeed() + 2), dist_(GD::Bc::Bench::limb_distribution<GD::Bc::Number>()),
        max_digits_(max_digits)
  {
    RandomNumberPairGenerator::next();
  }

  [[nodiscard]] auto get() const -> std::pair<GD::Bc::Number, GD::Bc::Number> const& override
  {
    return pair_;
  }

  auto get_a_number() -> GD::Bc::Number
  {
    constexpr auto working_base = 10;
    auto digit_count = dist_(rand_) % (max_digits_ * GD::Bc::Number::base_log10_);
    auto int_len = dist_(rand_) % (digit_count + 1);

    std::string num;
    GD::Bc::Number::NumType digits = 0;
    for (decltype(digit_count) i = 0; i < digit_count; ++i) {
      if (i % GD::Bc::Number::base_log10_ == 0) {
        digits = dist_(rand_);
      }
      auto digit = digits / (GD::Bc::Number::base_ / working_base);
      digits = (digits % (GD::Bc::Number::base_ / working_base)) * working_base;
      assert(digit < working_base);  // NOLINT

      if (num.size() == int_len) {
        num += '.';
      }
      num += static_cast<char>('0' + digit);
    }
    return {num, working_base};
  }

  auto next() -> bool override
  {
    pair_ = std::make_pair(get_a_number(), get_a_number());
    return true;
  }

  static constexpr GD::Bc::Number::NumType default_max_digits_ = 40;

private:
  std::mt19937_64 rand_;
  std::uniform_int_distribution<GD::Bc::Number::NumType> dist_;
  std::pair<GD::Bc::Number, GD::Bc::Number> pair_;
  GD::Bc::Number::NumType max_digits_;
};

auto random_number_pair(
  GD::Bc::Number::NumType max_digits = RandomNumberPairGenerator::default_max_digits_)
  -> Catch::Generators::GeneratorWrapper<std::pair<GD::Bc::Number, GD::Bc::Number>>

{
  return {std::make_unique<RandomNumberPairGenerator>(max_digits)};
}

}  // namespace

TEST_CASE("GD::Bc::Number - division, random", "[bc][number]")
{
  constexpr unsigned working_base = 10;
  auto nums = GENERATE(take(5000, random_pair()));
  uint64_t mask = std::numeric_limits<uint64_t>::max();

  auto ud = nums.first % mask;
  while (mask != 0) {
    auto vd = nums.second % mask;
    Number8 const u(std::to_string(ud), working_base);
    if (vd != 0) {
      Number8 const v(std::to_string(vd), working_base);
      auto q = ud / vd;
      auto r = ud % vd;
      Number8 const expected_q(std::to_string(q), working_base);
      Number8 const expected_r(std::to_string(r), working_base);

      INFO("u = " << ud << " v = " << vd << " q = " << q << " r = " << r);
      Number8 div(u);
      div.divide(v, 0);

      Number8 mod(u);
      mod.modulo(v, 0);

      REQUIRE(div == expected_q);
      REQUIRE(mod == expected_r);
    }
    mask /= Number8::base_;
  }
}

TEST_CASE("GD::Bc::Number - square root, random", "[bc][number]")
{
  /* Test square rooting by taking the square root and multiplying it up again to see if we have the
   * correct result.
   */
  auto num = GENERATE(take(5000, random_number()));

  auto sqrt = num;
  sqrt.sqrt(0);

  /* sqrt is the truncated square root so sqrt * sqrt <= num.  */
  auto square = sqrt;
  square.multiply(sqrt, sqrt.scale() * 2);
  REQUIRE(square <= num);

  /* ... and (sqrt + epsilon) * (sqrt + epsilon) > num.
   * Where epsilon is 10^-sqrt.scale().
   */
  sqrt.add_epsilon();
  square = sqrt;
  square.multiply(sqrt, sqrt.scale() * 2);
  REQUIRE(square > num);
}

TEST_CASE("GD::Bc::Number - multiplication, random", "[bc][number]")
{
  /* Compare schoolbook, Karatsuba and mixed dispatch on the same operands. */
  auto nums = GENERATE(take(500, random_number_pair(200)));
  RestoreMultiplyThreshold<GD::Bc::Number> const restore;
  INFO("seed = " << Catch::rngSeed() << " lhs = " << nums.first << " rhs = " << nums.second);

  /* Always do basic multiplication.  */
  auto res1 = nums.first;
  GD::Bc::Number::multiply_split_point(GD::Bc::Number::base_);
  res1.multiply(nums.second, 0);

  /* Always do Karatsuba multiplication.  */
  auto res2 = nums.first;
  GD::Bc::Number::multiply_split_point(1);
  res2.multiply(nums.second, 0);
  REQUIRE(res1 == res2);

  /* Do a mix.  */
  auto res3 = nums.first;
  GD::Bc::Number::multiply_split_point(100);  // NOLINT - explicit magic
  res3.multiply(nums.second, 0);
  REQUIRE(res1 == res3);
}

namespace {
template<typename Number>
auto native_number(int64_t value) -> Number
{
  Number result(std::to_string(value < 0 ? -value : value), 10);
  if (value < 0) {
    result.negate();
  }
  return result;
}

template<typename Number>
void check_native_pair(int64_t lhs, int64_t rhs)
{
  INFO("lhs = " << lhs << " rhs = " << rhs);
  auto const a = native_number<Number>(lhs);
  auto const b = native_number<Number>(rhs);
  auto result = a;
  result.add(b);
  REQUIRE(result == native_number<Number>(lhs + rhs));
  result = a;
  result.sub(b);
  REQUIRE(result == native_number<Number>(lhs - rhs));
  result = a;
  result.multiply(b, 0);
  REQUIRE(result == native_number<Number>(lhs * rhs));
  if (rhs != 0) {
    result = a;
    result.divide(b, 0);
    REQUIRE(result == native_number<Number>(lhs / rhs));
    result = a;
    result.modulo(b, 0);
    REQUIRE(result == native_number<Number>(lhs % rhs));
  }
  // Mutating a shared copy must leave both original operands unchanged.
  REQUIRE(a == native_number<Number>(lhs));
  REQUIRE(b == native_number<Number>(rhs));
}
}  // namespace

TEST_CASE("Number8 exhaustive one-limb arithmetic", "[bc][number][oracle]")
{
  RestoreMultiplyThreshold<Number8> const restore;
  for (int64_t a = 0; a < Number8::base_; ++a) {
    for (int64_t b = 0; b < Number8::base_; ++b) {
      check_native_pair<Number8>(a, b);
      // Low limbs force carry into and borrow from the next limb.
      check_native_pair<Number8>(a * 100 + 99, b * 100 + 1);
    }
  }
}

TEMPLATE_TEST_CASE("Generic arithmetic native oracles", "[bc][number][oracle]",
                   GD::Bc::NumberTraits8, GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  RestoreMultiplyThreshold<Number> const restore;
  int64_t const base = Number::base_;
  // Products fit in int64_t even for base 10^9. Small traits also exercise
  // odd/even multi-limb lengths, sparse halves and normalization boundaries.
  std::vector<int64_t> const values{0,      1,       2,        49,       50,    99,
                                    100,    101,     199,      9999,     10000, 10001,
                                    999999, 1000000, 99999999, base - 1, base,  base + 1};
  for (auto threshold : {1U, 2U, 3U, static_cast<unsigned>(Number::base_)}) {
    Number::multiply_split_point(static_cast<typename Number::NumType>(threshold));
    INFO("threshold = " << threshold);
    for (auto a : values) {
      for (auto b : values) {
        check_native_pair<Number>(a, b);
        check_native_pair<Number>(-a, b);
        check_native_pair<Number>(a, -b);
        check_native_pair<Number>(-a, -b);
      }
      auto square = native_number<Number>(a);
      auto shared = square;
      square.multiply(square, 0);
      REQUIRE(square == native_number<Number>(a * a));
      REQUIRE(shared == native_number<Number>(a));
      auto doubled = shared;
      doubled.add(doubled);
      REQUIRE(doubled == native_number<Number>(a + a));
      auto cancelled = shared;
      cancelled.sub(cancelled);
      REQUIRE(cancelled.is_zero());
    }
    std::mt19937_64 rng(197199);
    for (unsigned i = 0; i < 256; ++i) {
      int64_t const a = static_cast<int64_t>(rng() % 3000000000ULL);
      int64_t const b = static_cast<int64_t>(rng() % 3000000000ULL);
      INFO("seed = 197199 sample = " << i);
      check_native_pair<Number>(a, b);
    }
  }
}

TEMPLATE_TEST_CASE("Generic input conversion and width boundaries", "[bc][number][oracle]",
                   GD::Bc::NumberTraits8, GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using NumType = typename Number::NumType;
  static_assert(static_cast<uint64_t>(Number::base_) * Number::base_ - 1 <=
                std::numeric_limits<typename Number::WideType>::max());
  auto const last_limb = static_cast<NumType>(Number::base_ - 1);
  REQUIRE(Number(last_limb) == native_number<Number>(last_limb));
  REQUIRE(Number(last_limb).to_unsigned() == last_limb);
  REQUIRE(Number(std::to_string(last_limb) + ".0", 10).to_unsigned() == last_limb);
  REQUIRE(Digits().to_unsigned(0) == 0);
  REQUIRE(Digits(1).to_unsigned(Number::base_log10_) == Digits::to_unsigned_error_fractional_);
  auto too_large = Digits(last_limb);
  too_large.mul_pow10(static_cast<NumType>(Number::base_log10_ * 3));
  REQUIRE(too_large.to_unsigned(0) == Digits::to_unsigned_error_too_large_);

  // Isolate each part of the conversion early-return decision: decimal,
  // integer and zero-fraction cases versus genuine nondecimal fractions.
  REQUIRE(Number("0.625", 10) == Number("0.A00", 16));
  REQUIRE(Number("10", 16) == Number("16", 10));
  REQUIRE(Number("0.000", 16).is_zero());
  REQUIRE(Number("0.1", 2) == Number("0.5", 10));
  REQUIRE(Number("0.01", 2) == Number("0.25", 10));
  REQUIRE(Number("0.0000000001", 2) == Number("0.0009765625", 10));
  // 35 binary fractional digits exceed the largest native chunk (29 bits),
  // exercising the t <= ibase exit as well as the short-input scale exit.
  REQUIRE(Number("0." + std::string(35, '1'), 2) ==
          Number("0.99999999997089616954326629638671875", 10));
  REQUIRE(Number("0." + std::string(34, '0') + "1", 2) ==
          Number("0.00000000002910383045673370361328125", 10));

  std::string const fraction = "0." + std::string(80, '1');
  std::ostringstream output;
  Number(fraction, 10).output(output, 10, 0);
  auto wrapped = output.str();
  REQUIRE(wrapped.find("\\\n") != std::string::npos);
  for (auto pos = wrapped.find("\\\n"); pos != std::string::npos; pos = wrapped.find("\\\n")) {
    wrapped.erase(pos, 2);
  }
  REQUIRE(wrapped == (GD::Bc::extensions_enabled() ? fraction.substr(1) : fraction));
}

TEMPLATE_TEST_CASE("Unsigned digit conversion agrees with exact integer division",
                   "[bc][number][oracle][unsigned-conversion]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using N = typename Number::NumType;
  uint64_t const base = Number::base_;
  std::array<uint64_t, 17> const coefficients{
    {0, 1, 2, 9, 10, 11, 99, 100, 101, base - 1, base, base + 1, base * base - 1, base * base,
     base * base + 1, base * base / 2, std::numeric_limits<N>::max()}};
  unsigned const chunk = Number::base_log10_;
  for (auto const coefficient : coefficients) {
    Digits digits(N(0));
    for (auto digit : std::to_string(coefficient)) {
      digits.mac(N(10), N(digit - '0'));
    }
    for (unsigned places : {0U, 1U, 2U, chunk - 1, chunk, chunk + 1, 2 * chunk, 2 * chunk + 1}) {
      uint64_t denominator = 1;
      for (unsigned i = 0; i < places; ++i) {
        denominator *= 10;
      }
      uint64_t const quotient = coefficient / denominator;
      uint64_t const remainder = coefficient % denominator;
      N const expected = remainder != 0     ? Digits::to_unsigned_error_fractional_
                         : quotient >= base ? Digits::to_unsigned_error_too_large_
                                            : N(quotient);
      INFO("coefficient = " << coefficient << ", scale = " << places << ", base = " << base);
      CHECK(digits.to_unsigned(N(places)) == expected);
      N const truncated = quotient >= base ? Digits::to_unsigned_error_too_large_ : N(quotient);
      CHECK(digits.to_unsigned(N(places), true) == truncated);
    }
  }
}

TEMPLATE_TEST_CASE("Native number construction normalizes the full input value",
                   "[bc][number][oracle][native-construction]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  for (N const value :
       {N(0), N(1), N(Number::base_ - 1), N(Number::base_), std::numeric_limits<N>::max()}) {
    auto const text = std::to_string(value);
    INFO("value = " << text << ", base = " << Number::base_);
    Number const actual(value);
    Number const expected(text, 10);
    REQUIRE(actual == expected);
    REQUIRE_FALSE(actual < expected);
    REQUIRE_FALSE(actual > expected);
    auto changed = actual;
    changed.add(Number(N(1)));
    auto expected_changed = expected;
    expected_changed.add(Number("1", 10));
    REQUIRE(changed == expected_changed);
    REQUIRE(actual == expected);
  }
}
