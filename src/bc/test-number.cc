/** \file   test-number.cc
 *  \brief  Tests for bc's arbitrary precision arithmetic
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <cstdint>
#include <random>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>

#include "bc.hh"

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
  RandomNumberGenerator() : rand_(Catch::rngSeed() + 1), dist_(0, GD::Bc::Number::base_)
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
      : rand_(Catch::rngSeed() + 2), dist_(0, GD::Bc::Number::base_), max_digits_(max_digits)
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
  /* Test square rooting by taking the square root and multiplying it up again to see if we have the
   * correct result.
   */
  auto nums = GENERATE(take(500, random_number_pair(200)));

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
  auto res3 = nums.second;
  GD::Bc::Number::multiply_split_point(100);  // NOLINT - explicit magic
  res3.multiply(nums.second, 0);
  REQUIRE(res1 == res2);
}
