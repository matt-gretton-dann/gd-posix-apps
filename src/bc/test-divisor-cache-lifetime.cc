/** \file test-divisor-cache-lifetime.cc
 * Isolated allocation-failure checks; no production fault-injection hooks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <cstdio>
#include <cstdlib>
#include <sstream>
#include <string>

#include "bc.hh"

namespace {
std::size_t live_allocations = 0;
unsigned attempts = 0;
int fail_at = -1;
bool armed = false;
}  // namespace

auto operator new(std::size_t size) -> void*
{
  if (armed && attempts++ == static_cast<unsigned>(fail_at)) {
    throw std::bad_alloc();
  }
  if (auto* result = std::malloc(size == 0 ? 1 : size)) {
    ++live_allocations;
    return result;
  }
  throw std::bad_alloc();
}
void operator delete(void* pointer) noexcept
{
  if (pointer != nullptr) {
    --live_allocations;
    std::free(pointer);
  }
}
void operator delete(void* pointer, std::size_t) noexcept { ::operator delete(pointer); }
auto operator new[](std::size_t size) -> void* { return ::operator new(size); }
void operator delete[](void* pointer) noexcept { ::operator delete(pointer); }
void operator delete[](void* pointer, std::size_t) noexcept { ::operator delete(pointer); }

namespace {
constexpr char divisor_text[] = "123456789012345678901234567890123456789012345678901";

template<typename Digits>
auto source() -> Digits
{
  Digits value(typename Digits::NumType{0});
  for (auto digit : std::string_view(divisor_text)) {
    value.mac(10, static_cast<typename Digits::NumType>(digit - '0'));
  }
  return value;
}

template<typename Traits>
auto exercise() -> bool
{
  using Digits = GD::Bc::Details::BasicDigits<Traits>;
  // A first observation and continuously changing unique values must not
  // allocate or trigger a COW clone. All mutations fit the existing storage.
  {
    auto value = source<Digits>();
    typename Digits::DivisorCache cache;
    attempts = 0;
    fail_at = 0;
    armed = true;
    bool valid = cache.lookup(value) == nullptr;
    for (unsigned change = 0; change != 100; ++change) {
      value.mac(1, 1);
      valid = valid && cache.lookup(value) == nullptr;
    }
    armed = false;
    if (!valid || attempts != 0 || cache.retained_limb_bytes() != 0) {
      return false;
    }
  }

  // Fail each allocation in promotion. No partially constructed snapshot may
  // survive, and the same observed source must support a subsequent retry.
  for (unsigned padding : {0U, 1U}) {
    bool completed = false;
    for (unsigned failure = 0; failure != 32; ++failure) {
      auto const outer_live = live_allocations;
      bool threw = false;
      {
        auto value = source<Digits>();
        typename Digits::DivisorCache cache;
        if (cache.lookup(value, padding) != nullptr || cache.lookup(value, padding) != nullptr) {
          return false;
        }
        auto const before = live_allocations;
        attempts = 0;
        fail_at = static_cast<int>(failure);
        armed = true;
        typename Digits::PreparedDivisor const* prepared = nullptr;
        try {
          prepared = cache.lookup(value, padding);
        }
        catch (std::bad_alloc const&) {
          threw = true;
        }
        armed = false;
        if (threw) {
          if (cache.retained_limb_bytes() != 0 || live_allocations != before) {
            return false;
          }
          prepared = cache.lookup(value, padding);
        }
        if (prepared == nullptr || cache.retained_limb_bytes() == 0 ||
            cache.retained_limb_bytes() > Digits::DivisorCache::max_retained_limb_bytes) {
          return false;
        }
        std::ostringstream output;
        value.output_base10(output, 0, 0);
        if (output.str() != divisor_text) {
          return false;
        }
      }
      if (live_allocations != outer_live) {
        return false;
      }
      if (!threw) {
        // Confirm that at least one actual promotion allocation was exercised.
        completed = failure > 0;
        break;
      }
    }
    if (!completed) {
      return false;
    }
  }
  return true;
}
}  // namespace

auto main() -> int
{
  try {
    if (exercise<GD::Bc::NumberTraits8>() && exercise<GD::Bc::NumberTraits16>() &&
        exercise<GD::Bc::NumberTraits32>()) {
      std::puts("Divisor admission allocation failures and retries passed");
      return 0;
    }
  }
  catch (...) {
    armed = false;
  }
  std::fputs("Divisor admission allocation invariant failed\n", stderr);
  return 1;
}
