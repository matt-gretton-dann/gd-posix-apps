/** \file test-divisor-cache-lifetime.cc
 * Isolated allocation-failure checks; no production fault-injection hooks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <algorithm>
#include <array>
#include <cstdio>
#include <cstdlib>
#include <sstream>
#include <string>

#include "bc.hh"
#include "test-divisor-cache-fixtures.hh"

namespace {
// Track requests made inside the armed promotion phase, not unrelated lazy
// standard-library allocations (for example persistent locale facets).
std::array<void*, 64> tracked_allocations{};
std::size_t live_allocations = 0;
std::size_t limb_bytes = 0;
unsigned current_padding = 0;
unsigned current_failure = 0;
char const* phase = "initialization";
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
    if (armed) {
      auto const slot = std::find(tracked_allocations.begin(), tracked_allocations.end(), nullptr);
      if (slot == tracked_allocations.end()) {
        std::free(result);
        throw std::bad_alloc();
      }
      *slot = result;
      ++live_allocations;
    }
    return result;
  }
  throw std::bad_alloc();
}
void operator delete(void* pointer) noexcept
{
  if (pointer != nullptr) {
    auto const slot = std::find(tracked_allocations.begin(), tracked_allocations.end(), pointer);
    if (slot != tracked_allocations.end()) {
      *slot = nullptr;
      --live_allocations;
    }
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
  using Limb = typename Digits::NumType;
  limb_bytes = sizeof(Limb);
  auto fail = [](char const* reason) {
    std::fprintf(stderr,
                 "Divisor admission: %s; phase=%s limb_bytes=%zu padding=%u "
                 "failure=%u attempts=%u tracked=%zu\n",
                 reason, phase, limb_bytes, current_padding, current_failure, attempts,
                 live_allocations);
    return false;
  };
  // A first observation and continuously changing unique values must not
  // allocate or trigger a COW clone. All mutations fit the existing storage.
  {
    phase = "first observation and changing unique values";
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
      return fail("unexpected first-use/changing-value allocation or admission");
    }
  }

  // Actual retained capacities can exceed the preflight estimate after COW
  // alignment/normalization. Rejected stable requests must not allocate again.
  {
    phase = "capacity growth and rejected retry";
    auto [value, places] = GD::Bc::Test::capacity_growth_source<Digits>();
    (void)places;
    typename Digits::DivisorCache cache;
    auto const padding = static_cast<Limb>(Digits::base_log10_);
    current_padding = static_cast<unsigned>(padding);
    if (cache.lookup(value, padding) != nullptr || cache.lookup(value, padding) != nullptr) {
      return fail("capacity fixture promoted too early");
    }
    auto const before = live_allocations;
    attempts = 0;
    fail_at = -1;
    armed = true;
    auto const* prepared = cache.lookup(value, padding);
    armed = false;
    if (cache.retained_limb_bytes() > Digits::DivisorCache::max_retained_limb_bytes) {
      return fail("capacity growth exceeded retained budget");
    }
    if (prepared == nullptr) {
      if (live_allocations != before) {
        return fail("rejected capacity retained temporary allocations");
      }
      attempts = 0;
      fail_at = 0;
      armed = true;
      bool valid = true;
      try {
        for (unsigned repeat = 0; repeat != 4; ++repeat) {
          valid = valid && cache.lookup(value, padding) == nullptr;
        }
      }
      catch (std::bad_alloc const&) {
        valid = false;
      }
      armed = false;
      if (!valid || attempts != 0 || live_allocations != before) {
        return fail("stable rejected capacity repeated preparation");
      }
    }
    // A changed value must invalidate a rejection as well as an admitted
    // snapshot. Padding changes must start a fresh admission sequence.
    value.mac(1, 1);
    if (cache.lookup(value, padding) != nullptr) {
      return fail("changed capacity source reused old preparation");
    }
    (void)cache.lookup(value, padding);
    (void)cache.lookup(value, padding);
    if (cache.retained_limb_bytes() > Digits::DivisorCache::max_retained_limb_bytes) {
      return fail("changed capacity source exceeded retained budget");
    }
    if (cache.lookup(value, Limb{0}) != nullptr || cache.lookup(value, Limb{0}) != nullptr) {
      return fail("padding change reused old preparation");
    }
    (void)cache.lookup(value, Limb{0});
    if (cache.retained_limb_bytes() > Digits::DivisorCache::max_retained_limb_bytes) {
      return fail("changed padding exceeded retained budget");
    }
    phase = "near-budget maximal padding preflight";
    current_padding = static_cast<unsigned>(std::numeric_limits<Limb>::max());
    current_failure = 0;
    attempts = 0;
    fail_at = 0;
    armed = true;
    bool valid = true;
    try {
      for (unsigned repeat = 0; repeat != 3; ++repeat) {
        valid = valid && cache.lookup(value, std::numeric_limits<Limb>::max()) == nullptr;
      }
    }
    catch (std::bad_alloc const&) {
      valid = false;
    }
    armed = false;
    if (!valid || attempts != 0 || cache.retained_limb_bytes() != 0 || live_allocations != before) {
      return fail("near-budget maximal padding allocated or retained storage");
    }
    // Exact-growth vector implementations can admit the bounded fixture.
    // Either result must release every armed allocation on clear.
    cache.clear();
    if (live_allocations != before) {
      return fail("capacity clear retained an armed allocation");
    }
    auto small = source<Digits>();
    if (cache.lookup(small) != nullptr || cache.lookup(small) != nullptr ||
        cache.lookup(small) == nullptr) {
      return fail("capacity rejection/clear prevented later admission");
    }
  }

  // Fail each allocation in promotion. No partially constructed snapshot may
  // survive, and the same observed source must support a subsequent retry.
  for (Limb padding : {Limb{0}, Limb{1}}) {
    phase = "promotion";
    current_padding = padding;
    bool completed = false;
    for (unsigned failure = 0; failure != 32; ++failure) {
      current_failure = failure;
      auto const outer_live = live_allocations;
      bool threw = false;
      {
        auto value = source<Digits>();
        typename Digits::DivisorCache cache;
        if (cache.lookup(value, padding) != nullptr || cache.lookup(value, padding) != nullptr) {
          return fail("promoted before three stable observations");
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
            return fail("failed promotion retained a snapshot or allocation");
          }
          prepared = cache.lookup(value, padding);
        }
        if (prepared == nullptr || cache.retained_limb_bytes() == 0 ||
            cache.retained_limb_bytes() > Digits::DivisorCache::max_retained_limb_bytes) {
          return fail("promotion/retry did not retain a bounded prepared value");
        }
        std::ostringstream output;
        value.output_base10(output, 0, 0);
        if (output.str() != divisor_text) {
          return fail("promotion changed the source coefficient");
        }
      }
      if (live_allocations != outer_live) {
        return fail("promotion allocation survived cache destruction");
      }
      if (!threw) {
        // Confirm that at least one actual promotion allocation was exercised.
        completed = failure > 0;
        break;
      }
    }
    if (!completed) {
      return fail("no complete allocation-failure sequence");
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
    std::fprintf(stderr, "Unexpected allocation/exception: phase=%s limb_bytes=%zu attempts=%u\n",
                 phase, limb_bytes, attempts);
  }
  std::fputs("Divisor admission allocation invariant failed\n", stderr);
  return 1;
}
