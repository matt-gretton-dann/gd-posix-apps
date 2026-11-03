/** \file test-number-cached.cc
 * Independent scaled arithmetic guards before VM divisor reuse.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <array>
#include <limits>
#include <string_view>

#include "bc.hh"

TEMPLATE_TEST_CASE("Repeated scaled division preserves signs scales and aliases",
                   "[bc][number][cached-division][oracle]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Limb = typename Number::NumType;
  struct Case
  {
    std::string_view a, b, q, r;
    unsigned scale, remainder_scale;
  };
  // Python Fraction: x=A/10^sa, y=B/10^sb; q=floor(x/y*10^t)/10^t,
  // r=x-q*y. A=10^99+314159265358979323846264338327950288419716939937510;
  // B=10^50+271828182845904523536028747135266249775724709369995.
  // sa=0/7/17, sb=0/3, t=0/2/13. Divisors exceed inline storage at every width.
  std::array<Case, 18> const cases{
    {{"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "39937510",
      "371828182845904523536028747135266249775724709369995",
      "2689414213699951207488407581781637256348553598350",
      "106579772315813677406589618233841824779998168429260", 0, 0},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "39937510",
      "371828182845904523536028747135266249775724709369995",
      "2689414213699951207488407581781637256348553598350.28",
      "2467881118960410816501569035967274842795249805661.40", 2, 2},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "39937510",
      "371828182845904523536028747135266249775724709369995",
      "2689414213699951207488407581781637256348553598350.2866371545590",
      "20030013635693036163384263417465823337.3876679427950", 13, 13},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "39937510",
      "371828182845904523536028747135266249775724709369.995",
      "2689414213699951207488407581781637256348553598350286",
      "236912021884983675285396553155677344140901549441.430", 0, 3},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "39937510",
      "371828182845904523536028747135266249775724709369.995",
      "2689414213699951207488407581781637256348553598350286.63",
      "2660266692063825457698442460459606782194982538.33315", 2, 5},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "39937510",
      "371828182845904523536028747135266249775724709369.995",
      "2689414213699951207488407581781637256348553598350286.6371545590538",
      "25657398583372797145916821588499099.4536785786892690", 13, 16},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "3.9937510",
      "371828182845904523536028747135266249775724709369995",
      "268941421369995120748840758178163725634855",
      "133796804832333285807954644884380354372230378795968.9937510", 0, 7},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "3.9937510",
      "371828182845904523536028747135266249775724709369995",
      "268941421369995120748840758178163725634855.35",
      "3656940836266702570344583387037166950726730516470.7437510", 2, 7},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "3.9937510",
      "371828182845904523536028747135266249775724709369995",
      "268941421369995120748840758178163725634855.3598350286637",
      "5746941214249379294624123051274303376.4909572743185", 13, 13},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "3.9937510",
      "371828182845904523536028747135266249775724709369.995",
      "268941421369995120748840758178163725634855359",
      "310487190653561858520324662819770702745208132140.7887510", 0, 7},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "3.9937510",
      "371828182845904523536028747135266249775724709369.995",
      "268941421369995120748840758178163725634855359.83",
      "1869798891461103985420802697499715431356623363.6929010", 2, 7},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502884197169"
      "3.9937510",
      "371828182845904523536028747135266249775724709369.995",
      "268941421369995120748840758178163725634855359.8350286637154",
      "20787198422449632169280345391203129.9447967500205770", 13, 16},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502."
      "88419716939937510",
      "371828182845904523536028747135266249775724709369995", "26894142136999512074884075817816",
      "138529603821581179172032983441708398475395626448582.88419716939937510", 0, 17},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502."
      "88419716939937510",
      "371828182845904523536028747135266249775724709369995", "26894142136999512074884075817816.37",
      "953176168596505463702347001659886058377483981684.73419716939937510", 2, 17},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502."
      "88419716939937510",
      "371828182845904523536028747135266249775724709369995",
      "26894142136999512074884075817816.3725634855359",
      "31048719065356185852032466281977070274.52081321407887510", 13, 17},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502."
      "88419716939937510",
      "371828182845904523536028747135266249775724709369.995", "26894142136999512074884075817816372",
      "209519802904696416630289507389353558826034562944.74419716939937510", 0, 17},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502."
      "88419716939937510",
      "371828182845904523536028747135266249775724709369.995",
      "26894142136999512074884075817816372.56",
      "1296020510989883450113408993604458951628725697.54699716939937510", 2, 17},
     {"10000000000000000000000000000000000000000000000003141592653589793238462643383279502."
      "88419716939937510",
      "371828182845904523536028747135266249775724709369.995",
      "26894142136999512074884075817816372.5634855359835",
      "1065797723158136774065896182338418.24779998168429260", 13, 17}}};
  for (auto const& test : cases) {
    for (bool negative_a : {false, true}) {
      for (bool negative_b : {false, true}) {
        Number lhs(test.a, 10), rhs(test.b, 10);
        Number expected_q(test.q, 10), expected_r(test.r, 10);
        if (negative_a) {
          lhs.negate();
          expected_r.negate();
        }
        if (negative_b)
          rhs.negate();
        if (negative_a != negative_b)
          expected_q.negate();
        typename Number::DivisorCache requests_cache;
        typename Number::DivisorCache alias_cache;
        auto const saved_lhs = lhs;
        auto const saved_rhs = rhs;
        INFO("sa=" << lhs.scale() << " sb=" << rhs.scale() << " t=" << test.scale
                   << " negative_a=" << negative_a << " negative_b=" << negative_b);
        for (unsigned repeat = 0; repeat != 5; ++repeat) {
          auto q = lhs;
          auto r = lhs;
          q.divide_reusing(rhs, static_cast<Limb>(test.scale), requests_cache);
          r.modulo_reusing(rhs, static_cast<Limb>(test.scale), requests_cache);
          REQUIRE(q == expected_q);
          REQUIRE(r == expected_r);
          REQUIRE(q.scale() == test.scale);
          REQUIRE(r.scale() == test.remainder_scale);
          REQUIRE(lhs == saved_lhs);
          REQUIRE(rhs == saved_rhs);
          q = lhs;
          r = lhs;
          q.divide_reusing(q, static_cast<Limb>(test.scale), alias_cache);
          r.modulo_reusing(r, static_cast<Limb>(test.scale), alias_cache);
          REQUIRE(q == Number(1));
          REQUIRE(r == Number(0));
          REQUIRE(q.scale() == test.scale);
          REQUIRE(r.scale() == lhs.scale() + test.scale);
        }
        REQUIRE(requests_cache.retained_limb_bytes() > 0);
        REQUIRE(alias_cache.retained_limb_bytes() > 0);
      }
    }
  }
}

TEMPLATE_TEST_CASE("Admitted number divisors handle zero and smaller dividends",
                   "[bc][number][cached-division][oracle]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Limb = typename Number::NumType;
  Number const divisor("123456789012345678901234567890123456789012345678901", 10);
  typename Number::DivisorCache cache;
  for (unsigned repeat = 0; repeat != 5; ++repeat) {
    for (auto literal : {"0", "0.00", "7", "7.25"}) {
      Number const value(literal, 10);
      auto q = value;
      auto r = value;
      q.divide_reusing(divisor, Limb{3}, cache);
      r.modulo_reusing(divisor, Limb{3}, cache);
      REQUIRE(q == Number(0));
      REQUIRE(q.scale() == 3);
      REQUIRE(r == value);
      REQUIRE(r.scale() == 3);
    }
  }
  REQUIRE(cache.retained_limb_bytes() > 0);
}

TEMPLATE_TEST_CASE("Try reuse leaves misses unchanged and preserves self aliases on hits",
                   "[bc][number][cached-division][oracle]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Limb = typename Number::NumType;
  for (bool remainder : {false, true}) {
    for (bool negative : {false, true}) {
      Number divisor("123456789012345678901234567890123456789012345678.901", 10);
      if (negative) {
        divisor.negate();
      }
      typename Number::DivisorCache cache;
      auto attempt = [&](Number& lhs, Number const& rhs) {
        return remainder ? lhs.try_modulo_reusing(rhs, Limb{7}, cache)
                         : lhs.try_divide_reusing(rhs, Limb{7}, cache);
      };
      Number const dividend("987654321098765432109876543210987654321098765432109876.54", 10);
      Number const saved_divisor = divisor;
      // Neither early observation may change either numeric operand.
      for (unsigned observation = 0; observation != 2; ++observation) {
        auto lhs = dividend;
        REQUIRE_FALSE(attempt(lhs, divisor));
        REQUIRE(lhs == dividend);
        REQUIRE(lhs.scale() == dividend.scale());
        REQUIRE(divisor == saved_divisor);
        REQUIRE(divisor.scale() == saved_divisor.scale());
      }
      auto lhs = dividend;
      auto expected = dividend;
      if (remainder) {
        expected.modulo(divisor, Limb{7});
      }
      else {
        expected.divide(divisor, Limb{7});
      }
      REQUIRE(attempt(lhs, divisor));
      REQUIRE(lhs == expected);
      REQUIRE(lhs.scale() == expected.scale());
      // The exact same object is both operands. Result mutation must not
      // change the RHS scale/sign/limb metadata used to finish the operation.
      auto alias = divisor;
      REQUIRE(attempt(alias, alias));
      REQUIRE(alias == Number(remainder ? 0 : 1));
      REQUIRE(alias.scale() == (remainder ? divisor.scale() + 7 : 7));
      REQUIRE(divisor == saved_divisor);
      Number const zero("0.00", 10);
      lhs = dividend;
      REQUIRE_FALSE(attempt(lhs, zero));
      REQUIRE(lhs == dividend);
      REQUIRE(lhs.scale() == dividend.scale());
      REQUIRE(cache.retained_limb_bytes() == 0);
    }
  }
}

TEMPLATE_TEST_CASE("Shared heap storage reports ownership without making a copy",
                   "[bc][number][cached-division]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  Number value("123456789012345678901234567890123456789012345678901", 10);
  REQUIRE(value.has_heap_storage());
  REQUIRE_FALSE(value.has_shared_heap_storage());
  {
    auto copy = value;
    REQUIRE(value.has_shared_heap_storage());
    REQUIRE(copy.has_shared_heap_storage());
  }
  REQUIRE_FALSE(value.has_shared_heap_storage());
  REQUIRE_FALSE(Number(7).has_shared_heap_storage());
}

TEMPLATE_TEST_CASE("Try remainder scale overflow leaves numeric operands unchanged",
                   "[bc][number][cached-division][oracle]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Limb = typename Number::NumType;
  Number const divisor("123456789012345678901234567890123456789012345678.901", 10);
  Number const dividend("987654321098765432109876543210987654321098765432109876.54", 10);
  typename Number::DivisorCache cache;
  for (unsigned observation = 0; observation != 3; ++observation) {
    auto lhs = dividend;
    REQUIRE(lhs.try_divide_reusing(divisor, Limb{7}, cache) == (observation == 2));
  }
  REQUIRE(cache.retained_limb_bytes() > 0);
  auto lhs = dividend;
  // Public VM scales cannot reach this sum, but the generic numeric try API
  // must still leave ordinary fallback safe at the trait's full scale width.
  REQUIRE_FALSE(lhs.try_modulo_reusing(divisor, std::numeric_limits<Limb>::max(), cache));
  REQUIRE(lhs == dividend);
  REQUIRE(lhs.scale() == dividend.scale());
  REQUIRE(divisor.scale() == 3);
  REQUIRE(cache.retained_limb_bytes() == 0);
}
