/** \file test-number-workspace.cc
 * Independent decimal convolution checks for recursive coefficient products.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>
#include <catch2/catch.hpp>

#include <algorithm>
#include <array>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "bc.hh"
#include "limb-product.hh"
#include "limb-schoolbook.hh"

namespace {
// This oracle uses decimal characters, not the implementation's limb radix,
// split rule, workspace layout or recursive multiplication.
auto decimal_product(std::string const& left, std::string const& right) -> std::string
{
  std::vector<unsigned> digits(left.size() + right.size());
  for (std::size_t i = 0; i < left.size(); ++i) {
    for (std::size_t j = 0; j < right.size(); ++j) {
      digits[i + j] +=
        unsigned(left[left.size() - 1 - i] - '0') * unsigned(right[right.size() - 1 - j] - '0');
    }
  }
  for (std::size_t i = 0; i + 1 < digits.size(); ++i) {
    digits[i + 1] += digits[i] / 10;
    digits[i] %= 10;
  }
  while (digits.size() > 1 && digits.back() == 0) {
    digits.pop_back();
  }
  std::string result;
  for (auto it = digits.rbegin(); it != digits.rend(); ++it) {
    result += static_cast<char>('0' + *it);
  }
  return result;
}

TEMPLATE_TEST_CASE("Recursive workspace stays within planned buffers",
                   "[bc][number][multiply][workspace]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Limb = typename TestType::NumType;
  using Kernel = GD::Bc::Details::LimbProduct<TestType>;
  auto decimal = [](std::span<Limb const> value) {
    std::string result;
    for (auto it = value.rbegin(); it != value.rend(); ++it) {
      auto const part = std::to_string(*it);
      result.append(TestType::base_log10_ - part.size(), '0');
      result += part;
    }
    auto const first = result.find_first_not_of('0');
    return first == std::string::npos ? std::string("0") : result.substr(first);
  };
  auto const guard = static_cast<Limb>(TestType::base_ - 1);
  for (std::size_t count : {4U, 5U, 7U, 13U, 32U, 63U}) {
    for (std::size_t shorter : {count, count / 2 + 1, count / 3}) {
      for (unsigned leading_zero : {0U, 1U}) {
        std::vector<Limb> a(count, guard);
        std::vector<Limb> b(shorter, guard);
        if (leading_zero != 0) {
          // The planner sees the original extents. Trimming can change both
          // the recursive shape and whether a schoolbook fallback is selected.
          std::fill(b.begin() + static_cast<std::ptrdiff_t>(shorter / 2), b.end(), Limb{0});
        }
        auto const size = Kernel::workspace_size(count, 1);
        std::vector<Limb> scratch(size + 2, guard);
        std::vector<Limb> output(a.size() + b.size() + 2, Limb{0});
        output.front() = output.back() = guard;
        auto const product = std::span<Limb>(output).subspan(1, a.size() + b.size());
        Kernel::multiply(a, b, product, std::span<Limb>(scratch).subspan(1, size), 1);
        CHECK(decimal(product) == decimal_product(decimal(a), decimal(b)));
        CHECK(output.front() == guard);
        CHECK(output.back() == guard);
        CHECK(scratch.front() == guard);
        CHECK(scratch.back() == guard);
      }
    }
  }
}
}  // namespace

TEMPLATE_TEST_CASE("Recursive products agree with decimal convolution",
                   "[bc][number][multiply][workspace]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  struct Restore
  {
    ~Restore() { Number::multiply_split_point(1); }
  } const restore;
  for (auto const& [a, b] : {std::pair{4U, 4U},
                             {5U, 7U},
                             {8U, 9U},
                             {17U, 16U},
                             {31U, 63U},
                             {2U, 63U},
                             {10U, 63U},
                             {63U, 31U},
                             {63U, 2U},
                             {63U, 10U}}) {
    for (unsigned pattern : {0U, 1U}) {
      std::string left(a * Number::base_log10_, '9');
      std::string right(b * Number::base_log10_, '9');
      if (pattern != 0) {
        // Zero halves and asymmetric sparse blocks exercise trimmed recursive
        // operands without allowing the decimal representation to lose a limb.
        std::fill(left.begin() + 1, left.end() - 1, '0');
        std::fill(right.begin() + 1, right.end() - 1, '0');
        right[right.size() / 3] = '7';
      }
      std::string const expected = decimal_product(left, right);
      for (unsigned threshold : {1U, 3U, 4U, 7U, 99U}) {
        INFO("limb digits " << Number::base_log10_ << ", shape " << a << 'x' << b << ", pattern "
                            << pattern << ", threshold " << threshold);
        Number::multiply_split_point(static_cast<N>(threshold));
        Number lhs(left, N(10));
        Number const rhs(right, N(10));
        Number snapshot = lhs;
        lhs.multiply(rhs, N(0));
        CHECK(lhs == Number(expected, N(10)));
        CHECK(snapshot == Number(left, N(10)));
        CHECK(rhs == Number(right, N(10)));
        snapshot.multiply(snapshot, N(0));
        CHECK(snapshot == Number(decimal_product(left, left), N(10)));
      }
    }
  }
}

TEMPLATE_TEST_CASE("Aliased and opposite-sign shared squares preserve snapshots",
                   "[bc][number][multiply][square]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  struct Restore
  {
    ~Restore() { Number::multiply_split_point(1); }
  } const restore;
  for (unsigned count : {2U, 3U, 4U, 7U, 16U, 17U, 63U}) {
    for (unsigned sparse : {0U, 1U}) {
      std::string input(count * Number::base_log10_, '9');
      if (sparse != 0) {
        std::fill(input.begin() + 1, input.end() - 1, '0');
      }
      Number const expected(decimal_product(input, input), N(10));
      for (N point : {N(1), N(4), N(16), N(Number::base_)}) {
        INFO("limb digits " << Number::base_log10_ << ", count " << count << ", sparse " << sparse
                            << ", threshold " << unsigned(point));
        Number::multiply_split_point(point);
        Number const snapshot(input, N(10));
        auto square = snapshot;
        square.multiply(square, N(0));
        CHECK(square == expected);
        CHECK(snapshot == Number(input, N(10)));
        auto negative = snapshot;
        negative.negate();  // Same shared coefficient, opposite public sign.
        auto product = snapshot;
        product.multiply(negative, N(0));
        auto negative_expected = expected;
        negative_expected.negate();
        CHECK(product == negative_expected);
        CHECK(snapshot == Number(input, N(10)));
      }
    }
  }
  Number fraction("1.2500", N(10));
  auto const original = fraction;
  fraction.multiply(fraction, N(6));
  CHECK(fraction == Number("1.562500", N(10)));
  CHECK(fraction.scale() == 6);
  CHECK(original == Number("1.2500", N(10)));
  CHECK(original.scale() == 4);
}

TEMPLATE_TEST_CASE("Triangular squares stay within exact output bounds",
                   "[bc][number][multiply][square]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Limb = typename TestType::NumType;
  using Kernel = GD::Bc::Details::LimbSquare<TestType>;
  static_assert(Kernel::supported);
  auto const guard = static_cast<Limb>(TestType::base_ - 1);
  for (std::size_t count : {1U, 2U, 3U, 4U, 7U, 16U, 17U, 63U}) {
    for (unsigned pattern : {0U, 1U, 2U}) {
      std::vector<Limb> input(count, guard);
      if (pattern == 1) {
        std::fill(input.begin(), input.end(), Limb{0});
        input.front() = input.back() = guard;
      }
      else if (pattern == 2) {
        std::fill(input.begin(), input.end(), Limb{0});
      }
      auto const snapshot = input;
      std::vector<Limb> output(2 * count + 2, Limb{0});
      output.front() = output.back() = guard;
      auto const result = std::span<Limb>(output).subspan(1, 2 * count);
      Kernel::multiply(input, result);
      auto decimal = [](std::span<Limb const> value) {
        std::string text;
        for (auto it = value.rbegin(); it != value.rend(); ++it) {
          auto const part = std::to_string(*it);
          text.append(TestType::base_log10_ - part.size(), '0');
          text += part;
        }
        auto const first = text.find_first_not_of('0');
        return first == std::string::npos ? std::string("0") : text.substr(first);
      };
      CHECK(decimal(result) == decimal_product(decimal(input), decimal(input)));
      CHECK(input == snapshot);
      CHECK(output.front() == guard);
      CHECK(output.back() == guard);
    }
  }
}

namespace {
// The ordinary product fits uint16_t but a doubled product does not. This
// deliberately nondecimal kernel model exercises the compile-time fallback.
struct NarrowSquareTraits
{
  using NumType = std::uint8_t;
  using WideType = std::uint16_t;
  static constexpr unsigned base_ = 200;
};
}  // namespace

TEST_CASE("Square dispatch retains a generic narrow-accumulator fallback",
          "[bc][number][multiply][square]")
{
  using Kernel = GD::Bc::Details::LimbProduct<NarrowSquareTraits>;
  static_assert(!GD::Bc::Details::LimbSquare<NarrowSquareTraits>::supported);
  std::array<std::uint8_t, 4> input{199, 199, 199, 199};
  std::array<std::uint8_t, 10> output{};
  output.front() = output.back() = 199;
  auto const result = std::span<std::uint8_t>(output).subspan(1, 8);
  Kernel::multiply(input, input, result, {}, 126);
  std::uint64_t value = 0;
  for (auto it = result.rbegin(); it != result.rend(); ++it) {
    value = value * 200 + *it;
  }
  CHECK(value == std::uint64_t{1599999999} * 1599999999);
  CHECK(output.front() == 199);
  CHECK(output.back() == 199);
}

TEMPLATE_TEST_CASE("Blocked schoolbook stays within tile and output bounds",
                   "[bc][number][multiply][blocked]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Limb = typename TestType::NumType;
  using Kernel = GD::Bc::Details::LimbSchoolbook<TestType>;
  auto decimal = [](std::span<Limb const> value) {
    std::string result;
    for (auto it = value.rbegin(); it != value.rend(); ++it) {
      auto const part = std::to_string(*it);
      result.append(TestType::base_log10_ - part.size(), '0');
      result += part;
    }
    auto const first = result.find_first_not_of('0');
    return first == std::string::npos ? std::string("0") : result.substr(first);
  };
  auto const guard = static_cast<Limb>(TestType::base_ - 1);
  for (std::size_t rows : {0U, 1U, 2U, 3U, 4U, 5U, 6U, 7U, 15U, 16U, 17U, 31U, 32U, 33U, 63U}) {
    for (std::size_t columns : {0U, 1U, 3U, 4U, 15U, 16U, 17U, 33U}) {
      for (bool sparse : {false, true}) {
        std::vector<Limb> a(rows, guard), b(columns, guard);
        if (sparse) {
          for (std::size_t i = 0; i < rows; i += 2)
            a[i] = 0;
          for (std::size_t i = 1; i < columns; i += 3)
            b[i] = 0;
        }
        std::vector<Limb> output(rows + columns + 2, 0);
        output.front() = output.back() = guard;
        auto result = std::span<Limb>(output).subspan(1, rows + columns);
        Kernel::multiply(a, b, result);
        CHECK(decimal(result) == decimal_product(decimal(a), decimal(b)));
        CHECK(output.front() == guard);
        CHECK(output.back() == guard);
        CHECK(std::all_of(result.begin(), result.end(),
                          [](auto digit) { return digit < TestType::base_; }));
      }
    }
  }
}

namespace {
struct TightBlockedTraits
{
  using NumType = std::uint8_t;
  using WideType = std::uint16_t;
  static constexpr WideType base_ = 200;
};
}  // namespace
TEST_CASE("Blocked schoolbook retains a narrow-wide scalar fallback", "[bc][number][blocked]")
{
  using Kernel = GD::Bc::Details::LimbSchoolbook<TightBlockedTraits>;
  CHECK_FALSE(Kernel::eligible(5, 7));
  std::vector<std::uint8_t> a(5, 199), b(7, 199), result(14, 0);
  result.front() = result.back() = 251;
  Kernel::multiply(a, b, std::span(result).subspan(1, 12));
  std::vector<std::uint32_t> oracle(12, 0);
  for (std::size_t i = 0; i < 5; ++i)
    for (std::size_t j = 0; j < 7; ++j)
      oracle[i + j] += 199U * 199U;
  for (std::size_t i = 0; i + 1 < oracle.size(); ++i) {
    oracle[i + 1] += oracle[i] / 200;
    oracle[i] %= 200;
  }
  for (std::size_t i = 0; i < 12; ++i)
    CHECK(result[i + 1] == oracle[i]);
  CHECK(result.front() == 251);
  CHECK(result.back() == 251);
}
