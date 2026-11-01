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

TEMPLATE_TEST_CASE("Fraction helpers preserve exact padded and empty values",
                   "[bc][number][oracle][scale-helper]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using N = typename Digits::NumType;
  for (bool allocated : {false, true}) {
    Digits zero = allocated ? Digits(N(0)) : Digits();
    CHECK(zero.mul_mod_pow10(N(16), N(1)) == 0);
    CHECK(zero.is_zero());
  }

  // The split pads the fractional coefficient past its original limb. Its
  // value is still one, so successive binary digits below are all zero.
  N const scale = N(Digits::base_log10_ + 1);
  auto const split = Digits(N(1)).split_frac(scale);
  CHECK(split.first.is_zero());
  CHECK(split.second.to_unsigned(0) == 1);
  auto fraction = split.second;
  CHECK(fraction.mul_mod_pow10(N(2), scale) == 0);
  CHECK(fraction.to_unsigned(0) == 2);
  CHECK(split.second.to_unsigned(0) == 1);

  // Independent native arithmetic covers both a partial and a complete
  // decimal limb, including a carry into the returned quotient.
  for (N const coefficient : {N(1), N(7), N(19), N(99)}) {
    for (N const places : {N(1), N(Digits::base_log10_), scale}) {
      uint64_t denominator = 1;
      for (N i = 0; i < places; ++i) {
        denominator *= 10;
      }
      uint64_t const product = uint64_t(coefficient) * 7;
      Digits value(coefficient);
      CHECK(value.mul_mod_pow10(N(7), places) == product / denominator);
      Digits expected(N(0));
      for (char digit : std::to_string(product % denominator)) {
        expected.mac(N(10), N(digit - '0'));
      }
      CHECK(value.compare(expected, N(0)) == GD::Bc::Details::ComparisonResult::equality);
    }
  }
}

TEMPLATE_TEST_CASE("Positive powers fold scales larger than the limb radix",
                   "[bc][number][oracle][scale-helper]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  Number value("0.1", 10);
  Number const exponent(std::to_string(uint64_t(Number::base_) + 2), 10);
  value.power(exponent, N(2));
  // 10^-(base+2) truncated to two places is exactly zero. The coefficient
  // stays one throughout exponentiation, keeping this native-width case
  // bounded even for the billion-scale production trait.
  CHECK(value.is_zero());
  CHECK(value.scale() == 2);
  CHECK(value == Number("0.00", 10));
  CHECK(exponent == Number(std::to_string(uint64_t(Number::base_) + 2), 10));
}

TEMPLATE_TEST_CASE("Digit helpers handle absent storage and extended scale padding",
                   "[bc][number][oracle][digit-boundary]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using N = typename Digits::NumType;
  using C = GD::Bc::Details::ComparisonResult;
  CHECK(Digits().length() == 1);
  CHECK(Digits(N(0)).length() == 1);

  // Comparison requires allocated coefficients, including allocated zero.
  // A scale spanning several complete limbs must supply missing LHS zeroes.
  N const scale = N(2 * Digits::base_log10_ + 1);
  CHECK(Digits(N(0)).compare(Digits(N(1)), scale) == C::less_than);
  CHECK(Digits(N(1)).compare(Digits(N(0)), scale) == C::greater_than);
  CHECK(Digits(N(0)).compare(Digits(N(0)), scale) == C::equality);

  // Splitting beyond the coefficient pads whole absent fractional limbs as
  // well as a final partial limb, while retaining the exact coefficient.
  Digits const source(N(1));
  auto const parts = source.split_frac(scale);
  CHECK(parts.first.is_zero());
  CHECK(parts.second.to_unsigned(0) == 1);
  CHECK(source.to_unsigned(0) == 1);
}

TEMPLATE_TEST_CASE("Sparse products preserve empty recursive half sums at every width",
                   "[bc][number][oracle][zero-sum-product]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  RestoreMultiplyThreshold<Number> restore;
  auto polynomial = [](bool extra, bool product) {
    constexpr unsigned chunk = Number::base_log10_;
    unsigned const degree = product ? 14 : 7;
    std::string text(degree * chunk + 1, '0');
    auto term = [&](unsigned power, char digit) { text[text.size() - 1 - power * chunk] = digit; };
    term(0, '1');
    term(degree, '1');
    if (product) {
      term(7, '2');
    }
    if (extra) {
      term(3, '1');
      if (product) {
        term(10, '1');
      }
    }
    return text;
  };
  // Let B be the limb radix. A=B^7+1 contains long all-zero recursive
  // blocks. C=A+B^3 keeps the matching block nonzero. Both A*C and C*A
  // therefore distinguish which temporary half sum is empty, using a valid
  // positive threshold. A*A also exercises the both-empty case.
  for (N const threshold : {N(1), N(2), N(Number::base_)}) {
    Number::multiply_split_point(threshold);
    for (unsigned order : {0U, 1U, 2U}) {
      std::string const left = polynomial(order == 2, false);
      std::string const right = polynomial(order == 1, false);
      std::string const expected = polynomial(order != 0, true);
      INFO("base = " << Number::base_ << ", threshold = " << unsigned(threshold)
                     << ", order = " << order);
      Number lhs(left, 10);
      Number const rhs(right, 10);
      Number value = lhs;
      value.multiply(rhs, N(0));
      CHECK(value == Number(expected, 10));
      std::ostringstream output;
      value.output(output, N(10), 0);
      auto text = output.str();
      for (auto at = text.find("\\\n"); at != std::string::npos; at = text.find("\\\n")) {
        text.erase(at, 2);
      }
      CHECK(text == expected);
      CHECK(lhs == Number(left, 10));
      CHECK(rhs == Number(right, 10));
    }
  }
}

TEMPLATE_TEST_CASE("Equal-scale arithmetic preserves long chains and aliases",
                   "[bc][number][aligned-arithmetic]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using N = GD::Bc::BasicNumber<TestType>;
  auto render = [](N const& value) {
    std::ostringstream output;
    value.output(output, 10, 0);
    return output.str();
  };
  auto const digits = 6 * N::base_log10_;
  std::string const nines(digits, '9');
  N const large(nines, 10);
  N const one(1);
  auto shared = large;

  auto value = large;
  value.add(one);
  CHECK(render(value) == "1" + std::string(digits, '0'));
  value.sub(one);
  CHECK(render(value) == nines);
  CHECK(render(shared) == nines);

  value = one;
  value.add(large);  // Grow the shorter destination before creating iterators.
  CHECK(render(value) == "1" + std::string(digits, '0'));
  value = one;
  value.sub(large);
  CHECK(render(value) == "-" + std::string(digits - 1, '9') + "8");

  value = large;
  value.add(value);
  CHECK(render(value) == "1" + std::string(digits - 1, '9') + "8");
  value = large;
  value.sub(value);
  CHECK(render(value) == "0");
  CHECK(render(shared) == nines);

  std::string const high = "7" + std::string(digits - 1, '0');
  value = N(high, 10);
  value.sub(one);
  CHECK(render(value) == "6" + std::string(digits - 1, '9'));
  std::string const sparse = "7" + std::string(digits - 2, '0') + "3";
  value = N(sparse, 10);
  value.add(one);
  CHECK(render(value) == "7" + std::string(digits - 2, '0') + "4");
  value.sub(one);
  CHECK(render(value) == sparse);
}

TEMPLATE_TEST_CASE("Decimal construction agrees with digit arithmetic at chunk boundaries",
                   "[bc][number][oracle][decimal-parse]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  Number const ten(N(10));
  auto reference = [&](std::string const& text) {
    // Construct only small integers directly. Multiplication/addition and exact
    // division supply a reference independent of decimal string decoding.
    Number coefficient(N(0));
    std::size_t places = 0;
    bool fractional = false;
    for (char c : text) {
      if (c == '.') {
        fractional = true;
      }
      else {
        coefficient.multiply(ten, N(0));
        coefficient.add(Number(N(c - '0')));
        if (fractional) {
          ++places;
        }
      }
    }
    Number divisor(N(1));
    for (std::size_t i = 0; i < places; ++i) {
      divisor.multiply(ten, N(0));
    }
    coefficient.divide(divisor, static_cast<N>(places));
    return coefficient;
  };
  auto check = [&](std::string const& text) {
    INFO("decimal input = " << text << ", limb width = " << Number::base_log10_);
    auto const expected = reference(text);
    Number const parsed(text, N(10));
    CHECK(parsed == expected);
    CHECK(parsed.scale() == expected.scale());
    CHECK(parsed.length() == expected.length());
    auto mutated = parsed;
    mutated.add(Number(N(1)));
    CHECK(parsed == expected);
  };
  auto const width = std::size_t{Number::base_log10_};
  for (std::size_t count : {std::size_t{1}, width, width + 1, 4 * width + 1, 16 * width + 3}) {
    std::string digits;
    for (std::size_t i = 0; i < count; ++i) {
      digits += static_cast<char>('0' + (7 * i + 3) % 10);
    }
    for (std::size_t places : {std::size_t{0}, std::size_t{1}, count / 2, count}) {
      auto text = "000" + digits;
      if (places != 0) {
        text.insert(text.size() - places, 1, '.');
      }
      check(text);
      if (places != 0) {
        check(text + "00");
      }
    }
  }
  // Every point position around partial/full chunks, including maximal limbs
  // and zero suffixes, must agree with independent digit arithmetic. These
  // bounds stay inside the narrow trait's existing decimal-length domain.
  for (auto count : {width - 1, width, width + 1, 2 * width - 1, 2 * width, 2 * width + 1}) {
    for (bool maximal : {false, true}) {
      std::string digits(count, maximal ? '9' : '0');
      if (!maximal) {
        digits.front() = '7';
      }
      for (std::size_t point = 0; point <= count; ++point) {
        auto text = "000" + digits;
        text.insert(3 + point, 1, '.');
        check(text);
      }
    }
  }
  for (auto const* text : {"", ".", "0", "000", "0.", ".0000", "000.000", ".0001", "001.0000"}) {
    check(text);
  }
  for (N base = 2; base <= 16; ++base) {
    for (unsigned digit = 0; digit < 16; ++digit) {
      auto const text = std::string(1, "0123456789ABCDEF"[digit]);
      CHECK(Number(text, base) == Number(static_cast<N>(digit)));
    }
  }
}

#if GD_BC_PROFILE
TEMPLATE_TEST_CASE("Scan diagnostics count logical limb positions", "[bc][number][scan-profile]",
                   GD::Bc::NumberTraits8, GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using N = typename TestType::NumType;
  namespace P = GD::Bc::Profile;
  auto event = [](P::Event kind) { return P::snapshot().events[static_cast<std::size_t>(kind)]; };
  Digits value(N(7));
  value.mul_pow10(N(3 * TestType::base_log10_));
  auto before = event(P::Event::zero_scan_limbs);
  CHECK_FALSE(value.is_zero());
  CHECK(event(P::Event::zero_scan_limbs) - before == 4);
  before = event(P::Event::length_scan_limbs);
  CHECK(value.length() == 3 * TestType::base_log10_ + 1);
  CHECK(event(P::Event::length_scan_limbs) - before == 1);
  auto zero = value;
  zero.mac(N(0), N(0));
  before = event(P::Event::zero_scan_limbs);
  CHECK(zero.is_zero());
  CHECK(event(P::Event::zero_scan_limbs) - before == 4);
  before = event(P::Event::length_scan_limbs);
  CHECK(zero.length() == 1);
  CHECK(event(P::Event::length_scan_limbs) - before == 4);
}
#endif
