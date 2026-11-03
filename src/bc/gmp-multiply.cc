/** Experimental GMP multiplication of base-10^9 coefficients.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "gmp-multiply.hh"

#include <algorithm>
#include <charconv>
#include <string>

#include <gmpxx.h>

namespace {
constexpr std::size_t limb_digits = 9;

auto coefficient(std::span<std::uint32_t const> limbs) -> mpz_class
{
  std::string text(limbs.size() * limb_digits, '0');
  auto* end = text.data() + text.size();
  for (auto limb : limbs) {
    char digits[limb_digits];
    auto const result = std::to_chars(digits, digits + limb_digits, limb);
    std::copy_backward(digits, result.ptr, end);
    end -= limb_digits;
  }
  return mpz_class(text, 10);
}
}

auto GD::Bc::Details::gmp_multiply(std::span<std::uint32_t const> lhs,
                                  std::span<std::uint32_t const> rhs)
  -> std::vector<std::uint32_t>
{
  mpz_class const product = coefficient(lhs) * coefficient(rhs);
  auto const text = product.get_str();
  std::vector<std::uint32_t> result;
  result.reserve(text.size() / limb_digits + 1);
  for (auto end = text.size(); end != 0;) {
    auto const begin = end - std::min(end, limb_digits);
    std::uint32_t limb{};
    std::from_chars(text.data() + begin, text.data() + end, limb);
    result.push_back(limb);
    end = begin;
  }
  return result;
}
