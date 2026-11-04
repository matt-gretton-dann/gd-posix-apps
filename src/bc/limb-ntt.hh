// SPDX-License-Identifier: Apache-2.0
#ifndef SRC_BC_LIMB_NTT_HH_INCLUDED
#define SRC_BC_LIMB_NTT_HH_INCLUDED

#include <algorithm>
#include <bit>
#include <cassert>
#include <cstdint>
#include <span>
#include <vector>

namespace GD::Bc::Details {

class LimbNtt
{
  static constexpr std::uint32_t first_prime = 998244353;
  static constexpr std::uint32_t second_prime = 1004535809;
  static constexpr std::size_t chunk_base = 1000;
  static constexpr std::size_t chunks_per_limb = 3;
  static constexpr std::size_t maximum_transform = 1U << 21;

  template<std::uint32_t Modulus>
  static constexpr auto product(std::uint32_t lhs, std::uint32_t rhs) -> std::uint32_t
  {
    return static_cast<std::uint32_t>(static_cast<std::uint64_t>(lhs) * rhs % Modulus);
  }

  template<std::uint32_t Modulus>
  static constexpr auto power(std::uint32_t base, std::uint32_t exponent) -> std::uint32_t
  {
    std::uint32_t result = 1;
    while (exponent != 0) {
      if ((exponent & 1U) != 0) {
        result = product<Modulus>(result, base);
      }
      base = product<Modulus>(base, base);
      exponent >>= 1U;
    }
    return result;
  }

  template<std::uint32_t Modulus>
  static void transform(std::span<std::uint32_t> values, bool inverse)
  {
    auto const size = values.size();
    for (std::size_t index = 1, reversed = 0; index < size; ++index) {
      auto bit = size / 2;
      while ((reversed & bit) != 0) {
        reversed ^= bit;
        bit /= 2;
      }
      reversed ^= bit;
      if (index < reversed) {
        std::swap(values[index], values[reversed]);
      }
    }

    constexpr auto inverse_root = power<Modulus>(3, Modulus - 2);
    for (std::size_t length = 2; length <= size; length *= 2) {
      auto const half = length / 2;
      auto const step = power<Modulus>(inverse ? inverse_root : 3,
                                       (Modulus - 1) / static_cast<std::uint32_t>(length));
      for (std::size_t start = 0; start < size; start += length) {
        std::uint32_t root = 1;
        for (std::size_t offset = 0; offset < half; ++offset) {
          auto& first = values[start + offset];
          auto& second = values[start + offset + half];
          auto const scaled = product<Modulus>(root, second);
          second = first >= scaled ? first - scaled : first + Modulus - scaled;
          first += scaled;
          if (first >= Modulus) {
            first -= Modulus;
          }
          root = product<Modulus>(root, step);
        }
      }
    }

    if (inverse) {
      auto const factor = power<Modulus>(static_cast<std::uint32_t>(size), Modulus - 2);
      for (auto& value : values) {
        value = product<Modulus>(value, factor);
      }
    }
  }

  static void unpack(std::span<std::uint32_t const> limbs, std::span<std::uint32_t> chunks)
  {
    auto output = chunks.begin();
    for (auto limb : limbs) {
      for (std::size_t index = 0; index < chunks_per_limb; ++index) {
        *output++ = limb % chunk_base;
        limb /= chunk_base;
      }
    }
    std::fill(output, chunks.end(), 0);
  }

  template<std::uint32_t Modulus>
  static void square_modulo(std::span<std::uint32_t const> limbs, std::span<std::uint32_t> values)
  {
    unpack(limbs, values);
    transform<Modulus>(values, false);
    for (auto& value : values) {
      value = product<Modulus>(value, value);
    }
    transform<Modulus>(values, true);
  }

public:
  static auto eligible(std::span<std::uint32_t const> limbs) -> bool
  {
    if (limbs.size() < 16384 || limbs.size() > maximum_transform / (2 * chunks_per_limb)) {
      return false;
    }
    // A transform's cost jumps when its padded size doubles. The native
    // holdout lost near half occupancy and improved on both hosts at
    // about three quarters. Keep sparse inputs on the existing kernel.
    auto const chunks = 2 * chunks_per_limb * limbs.size();
    return chunks >= std::bit_ceil(chunks) * 3 / 4 &&
           static_cast<std::size_t>(std::ranges::count(limbs, 0U)) <= limbs.size() / 2;
  }

  static void square(std::span<std::uint32_t const> limbs, std::span<std::uint32_t> output)
  {
    assert(eligible(limbs));
    assert(output.size() == 2 * limbs.size());
    auto const size = std::bit_ceil(chunks_per_limb * output.size());
    std::vector<std::uint32_t> first(size), second(size);
    square_modulo<first_prime>(limbs, first);
    square_modulo<second_prime>(limbs, second);

    // Both primes admit every radix-2 length through 2^21, with primitive
    // root 3. A coefficient is at most (2^21 / 2) * 999^2, less than
    // their product, so CRT gives the exact nonnegative coefficient.
    constexpr auto inverse = power<second_prime>(first_prime, second_prime - 2);
    std::uint64_t carry = 0;
    std::size_t offset = 0;
    for (auto& limb : output) {
      limb = 0;
      for (std::uint32_t place = 1; place < 1000000000; place *= chunk_base) {
        auto const left = first[offset];
        auto const right = second[offset++];
        auto const difference = right >= left ? right - left : right + second_prime - left;
        carry += left + static_cast<std::uint64_t>(first_prime) *
                          product<second_prime>(difference, inverse);
        limb += static_cast<std::uint32_t>(carry % chunk_base) * place;
        carry /= chunk_base;
      }
    }
    assert(carry == 0);
  }
};

}  // namespace GD::Bc::Details
#endif
