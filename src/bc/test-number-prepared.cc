/** \file test-number-prepared.cc
 * Independent integer oracles for reusable divisor snapshots.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <array>
#include <cstdint>
#include <limits>
#include <sstream>
#include <string>
#include <string_view>

#include "bc.hh"
#include "test-divisor-cache-fixtures.hh"

namespace {
template<typename Digits>
auto coefficient_text(std::string_view value) -> Digits
{
  Digits result(static_cast<typename Digits::NumType>(0));
  for (char digit : value) {
    result.mac(10, static_cast<typename Digits::NumType>(digit - '0'));
  }
  return result;
}

template<typename Digits>
auto coefficient(std::uint64_t value) -> Digits
{
  return coefficient_text<Digits>(std::to_string(value));
}

template<typename Digits>
auto decimal(Digits const& value) -> std::string
{
  if (value.is_zero()) {
    return "0";
  }
  std::ostringstream output;
  value.output_base10(output, 0, 0);
  auto result = output.str();
  for (auto at = result.find("\\\n"); at != std::string::npos; at = result.find("\\\n")) {
    result.erase(at, 2);
  }
  return result;
}

template<typename Digits>
void unprepared(Digits& value, Digits const& divisor, bool remainder)
{
  using C = GD::Bc::Details::ComparisonResult;
  auto const relation = value.compare(divisor, 0);
  if (relation == C::less_than) {
    if (!remainder) {
      value.reset();
    }
  }
  else if (relation == C::equality) {
    value = coefficient<Digits>(remainder ? 0 : 1);
  }
  else if (remainder) {
    value.modulo(divisor);
  }
  else {
    value.divide(divisor);
  }
}
}  // namespace

TEMPLATE_TEST_CASE("Repeated coefficient division preserves divisor snapshots",
                   "[bc][number][prepared][oracle]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  constexpr std::array<std::uint64_t, 16> dividends{0,
                                                    1,
                                                    2,
                                                    99,
                                                    100,
                                                    101,
                                                    9999,
                                                    10000,
                                                    10001,
                                                    99999999,
                                                    1000000000,
                                                    3128583549859727ULL,
                                                    6687218857121369ULL,
                                                    8623134770622992ULL,
                                                    1626423182298612ULL,
                                                    std::numeric_limits<std::uint64_t>::max()};
  constexpr std::array<std::uint64_t, 11> divisors{
    1, 2, 7, 97, 101, 10000, 34970, 63964738, 819692222, 81326081630ULL, 1000000000000000001ULL};
  for (auto b : divisors) {
    auto original = coefficient<Digits>(b);
    auto const snapshot = original;
    // A later cache must preserve this value even if the source is mutated
    // or its last external owner disappears between requests.
    original.mac(1, 1);
    original.reset();
    for (auto a : dividends) {
      INFO("A=" << a << " B=" << b << " radix=" << Digits::base_);
      for (unsigned repeat = 0; repeat != 3; ++repeat) {
        auto quotient = coefficient<Digits>(a);
        auto remainder = quotient;
        unprepared(quotient, snapshot, false);
        unprepared(remainder, snapshot, true);
        REQUIRE(decimal(quotient) == std::to_string(a / b));
        REQUIRE(decimal(remainder) == std::to_string(a % b));
        REQUIRE(decimal(snapshot) == std::to_string(b));
      }
    }
  }
}

TEMPLATE_TEST_CASE("Prepared divisors preserve an immutable coefficient",
                   "[bc][number][prepared][oracle]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  constexpr std::array<std::uint64_t, 8> divisors{
    1, 97, 101, 34970, 63964738, 819692222, 81326081630ULL, 1000000000000000001ULL};
  for (auto b : divisors) {
    auto source = coefficient<Digits>(b);
    auto prepared = source.prepare_divisor();
    auto copied = prepared;
    auto moved = std::move(copied);
    source.mac(1, 1);
    source.reset();
    std::uint64_t state = 197420;
    for (unsigned i = 0; i != 300; ++i) {
      // Fixed unsigned arithmetic provides a reproducible native-width oracle.
      state = state * 6364136223846793005ULL + 1442695040888963407ULL;
      auto const a = i == 0 ? 0 : i == 1 ? b : i == 2 ? b - 1 : state;
      INFO("A=" << a << " B=" << b << " radix=" << Digits::base_);
      auto quotient = coefficient<Digits>(a);
      auto remainder = quotient;
      quotient.divide(i % 2 == 0 ? prepared : moved);
      remainder.modulo(i % 2 == 0 ? moved : prepared);
      REQUIRE(decimal(quotient) == std::to_string(a / b));
      REQUIRE(decimal(remainder) == std::to_string(a % b));
    }
    // The dividend can share the original snapshot without changing it.
    auto alias = coefficient<Digits>(b);
    auto const aliased = alias.prepare_divisor();
    alias.divide(aliased);
    REQUIRE(decimal(alias) == "1");
    alias = coefficient<Digits>(b);
    alias.modulo(aliased);
    REQUIRE(decimal(alias) == "0");
  }
  Digits absent;
  Digits empty(static_cast<typename Digits::NumType>(0));
  REQUIRE_THROWS_AS(absent.prepare_divisor(), std::domain_error);
  REQUIRE_THROWS_AS(empty.prepare_divisor(), std::domain_error);
  auto const nonzero = coefficient<Digits>(97).prepare_divisor();
  absent.divide(nonzero);
  empty.modulo(nonzero);
  REQUIRE(decimal(absent) == "0");
  REQUIRE(decimal(empty) == "0");
}

TEST_CASE("Prepared eight-bit divisors match exhaustive native quotients",
          "[bc][number][prepared][oracle]")
{
  using Digits = GD::Bc::Details::BasicDigits<GD::Bc::NumberTraits8>;
  for (unsigned b = 1; b != 256; ++b) {
    auto const prepared = coefficient<Digits>(b).prepare_divisor();
    for (unsigned a = 0; a != 256; ++a) {
      auto quotient = coefficient<Digits>(a);
      auto remainder = quotient;
      quotient.divide(prepared);
      remainder.modulo(prepared);
      INFO("A=" << a << " B=" << b);
      REQUIRE(decimal(quotient) == std::to_string(a / b));
      REQUIRE(decimal(remainder) == std::to_string(a % b));
    }
  }
}

TEMPLATE_TEST_CASE("Prepared division handles quotient estimate boundaries",
                   "[bc][number][prepared][oracle]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  std::uint64_t const base = Digits::base_;
  auto block = [](std::uint64_t value) {
    auto text = std::to_string(value);
    return std::string(Digits::base_log10_ - text.size(), '0') + text;
  };
  auto const high = block(base / 2);
  auto const zero = block(0);
  auto const one = block(1);
  auto const maximum = block(base - 1);
  struct Case
  {
    std::string u, v, q, r;
  };
  // Decimal block identities force q_hat == base, two decrements and add-back
  // at each limb width, including dividends beyond the native integer range.
  std::array<Case, 3> const cases{
    {{high + one + zero + maximum, high + one + one, maximum, high + one + zero},
     {high + zero + zero + zero, high + maximum + one, block(base - 2),
      block(2) + block(base - 3) + block(2)},
     {block(base / 2 - 1) + high + zero + block(base - 2), high + zero + one, block(base - 2),
      high + zero + zero}}};
  for (auto const& test : cases) {
    auto const divisor = coefficient_text<Digits>(test.v).prepare_divisor();
    for (unsigned repeat = 0; repeat != 3; ++repeat) {
      auto q = coefficient_text<Digits>(test.u);
      auto r = q;
      q.divide(divisor);
      r.modulo(divisor);
      REQUIRE(decimal(q) == test.q);
      REQUIRE(decimal(r) == test.r.substr(test.r.find_first_not_of('0')));
    }
  }
  // Scaling a three-limb dividend by the normalization factor can promote its
  // inline storage; the kernel must capture spans only after that promotion.
  auto const width = Digits::base_log10_;
  auto const divisor =
    coefficient_text<Digits>("1" + std::string(width - 1, '0') + "1").prepare_divisor();
  auto q = coefficient_text<Digits>(std::string(3 * width, '9'));
  auto r = q;
  q.divide(divisor);
  r.modulo(divisor);
  REQUIRE(decimal(q) == std::string(width, '9') + std::string(width, '0'));
  REQUIRE(decimal(r) == std::string(width, '9'));
}

TEMPLATE_TEST_CASE("Prepared large coefficients match frozen Python divmod",
                   "[bc][number][prepared][oracle]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  struct Case
  {
    std::string_view u, v, q, r;
  };
  // Python random.Random(197420), decimal widths 70/130/280, divisor half
  // width with leading 1 or 9; expected results are Python integer divmod.
  // Long divisors exercise both normalization extremes at every limb width.
  std::array<Case, 6> const cases{
    {{"9213589371315020995297670591516593015296914695341292732740434051879745",
      "16935123744087458444898927484879897", "544052084327506112969703184276158771",
      "812323260980821720805832613753158"},
     {"9287729682161588213038477255907197743703613451472899810013191981520977",
      "93493160326398148833351875523004125", "99341274267944095474313110151799943",
      "75557271305223730535099320617756102"},
     {"95148520379444967928668484084317617800589687816730271835075950715738628893269129728649347744"
      "13703404889342480970278459204383436935",
      "15898233635847711063735778035860096050823984796532876551566011177",
      "598484854096632764736120946575675302226749161602996775647448981381",
      "10783223711689915082822804885521176081902580836090427274472541498"},
     {"93462234905510195609505938685956909090447578945842362025742597287259387182306194631312882414"
      "82441402343351026712066002360059531276",
      "93653252693051084426413679670394485241510599539817614405535855920",
      "99796037209548983428935711800230598833904981679292857314015074945",
      "78005199825371944844722805852057128588494623689449142756537606876"},
     {"93766995172761966938629009292930196189661062545973905277820692128866428715714683441877710661"
      "31871770027062026474783098458717108298772597609829060515537554123694693278116766828402213053"
      "58666296859857135813671212916840357027354872468167674359957612246053393946839510652791191657"
      "7589",
      "13295008079054494594545490780170205311212515786664212359142389927090847884475155403190417912"
      "634334354328639672621955003494866440849952475173",
      "70527971562865289190160547422300075842300918468339688821603649878881380715505559426289852810"
      "1334644999743788815632011017725735197835505507466",
      "84347379415047014094565058362468179506805491921522978361545541441484822843829610068919829335"
      "00231498635517322853662328847527315511785435971"},
     {"90538365614869936134288822288077286309259268953765651624638118413255924249929025485741647809"
      "24704344763288614820970287536980504624735927966700430385537488161091527758574343672417692894"
      "86163592884992148965925241337820824823400640498257302478067186195912186587717514561218885566"
      "3467",
      "93368044720105646196925891603183399118075314123114562981496932422817055556563742081358268405"
      "768883854730343492772753327828944417375987133889",
      "96969328088942646679619001042308286755085043818508240078578840333926119370040554829675198490"
      "125122523838774256251974634897367626338476835742",
      "70827430338358336296713530627042015797902894705173123593845959015624650746860990533854197676"
      "693102781754523741447873473239180325736441002829"}}};
  for (auto const& test : cases) {
    auto const divisor = coefficient_text<Digits>(test.v).prepare_divisor();
    auto q = coefficient_text<Digits>(test.u);
    auto r = q;
    q.divide(divisor);
    r.modulo(divisor);
    REQUIRE(decimal(q) == test.q);
    REQUIRE(decimal(r) == test.r);
  }
}

TEMPLATE_TEST_CASE("Preparation trims padded quotients without changing sources",
                   "[bc][number][prepared][storage]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  auto const width = Digits::base_log10_;
  // (B^3 + 1) / (B^2 + 1) = B - 1. Algorithm D reserves two quotient
  // limbs, leaving the high one zero until a consumer explicitly tidies it.
  auto source = coefficient_text<Digits>("1" + std::string(3 * width - 1, '0') + "1");
  source.divide(coefficient_text<Digits>("1" + std::string(2 * width - 1, '0') + "1"));
  auto const before = source.coefficient_limbs();
  auto const prepared = source.prepare_divisor();
  REQUIRE(prepared.coefficient_limbs() == 1);
  REQUIRE(source.coefficient_limbs() == before);
  auto q = coefficient_text<Digits>(std::string(2 * width, '9'));
  auto r = q;
  q.divide(prepared);
  r.modulo(prepared);
  REQUIRE(decimal(q) == "1" + std::string(width - 1, '0') + "1");
  REQUIRE(decimal(r) == "0");
}

TEMPLATE_TEST_CASE("Divisor admission observes changing values without retaining them",
                   "[bc][number][prepared][divisor-cache]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  typename Digits::DivisorCache cache;
  auto source = coefficient_text<Digits>("123456789012345678901234567890123456789012345678901");
  REQUIRE(source.has_heap_storage());
  for (unsigned change = 0; change != 100; ++change) {
    REQUIRE(cache.lookup(source) == nullptr);
    REQUIRE(cache.retained_limb_bytes() == 0);
    source.mac(1, 1);
  }
  REQUIRE(cache.lookup(source) == nullptr);
  auto const* prepared = cache.lookup(source);
  REQUIRE(prepared != nullptr);
  REQUIRE(cache.retained_limb_bytes() > 0);
  REQUIRE(cache.retained_limb_bytes() <= Digits::DivisorCache::max_retained_limb_bytes);
  auto const shared = source;
  REQUIRE(cache.lookup(shared) == prepared);
  // The admitted raw snapshot protects the old value through a subsequent COW
  // mutation, even when normalized/padded limbs use a different storage owner.
  source.mac(1, 1);
  auto q = coefficient_text<Digits>("10000000000000000000000000000000000000000000000000000000000000"
                                    "000000000000000000000000000000000012345");
  auto r = q;
  q.divide(*prepared);
  r.modulo(*prepared);
  REQUIRE(decimal(q) == "81000000729000006633900060368490549353263999114636");
  REQUIRE(decimal(r) == "115536123190652949658059264750651051517155743053709");
  REQUIRE(cache.lookup(source) == nullptr);
  REQUIRE(cache.retained_limb_bytes() == 0);
  REQUIRE(cache.lookup(source) == nullptr);
  REQUIRE(cache.lookup(source) != nullptr);
  REQUIRE(cache.lookup(source, 1) == nullptr);  // Different effective divisor.
  REQUIRE(cache.lookup(source, 1) == nullptr);
  auto const* padded = cache.lookup(source, 1);
  REQUIRE(padded != nullptr);
  q = coefficient_text<Digits>("1000000000000000000000000000000000000000000000000000000000000000000"
                               "0000000000000000000000000000012345");
  r = q;
  q.divide(*padded);
  r.modulo(*padded);
  REQUIRE(decimal(q) == "8100000072900000663390006036849054935326399911463");
  REQUIRE(decimal(r) == "775276856535727016431566611722901242897965818013085");
  source.mac(1, 1);
  // The padded prepared owner differs from the raw source. Admission must
  // retain that raw snapshot too, so this mutation changes its identity.
  REQUIRE(cache.lookup(source, 1) == nullptr);
  REQUIRE(cache.retained_limb_bytes() == 0);
  cache.clear();
  REQUIRE(cache.retained_limb_bytes() == 0);
  REQUIRE(cache.lookup(source) == nullptr);
}

TEMPLATE_TEST_CASE("Divisor admission bounds retained capacity after shrinking",
                   "[bc][number][prepared][divisor-cache]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using Limb = typename Digits::NumType;
  typename Digits::DivisorCache cache;
  auto source = coefficient<Digits>(1);
  // Small trait scale fields require bounded increments, not wrapped shifts.
  auto shift = [&](unsigned total, bool grow) {
    while (total != 0) {
      auto const part = std::min(total, 250U);
      if (grow)
        source.mul_pow10(static_cast<Limb>(part));
      else
        source.div_pow10(static_cast<Limb>(part));
      total -= part;
    }
  };
  shift(50000, true);
  REQUIRE(source.has_heap_storage());
  REQUIRE(cache.lookup(source) == nullptr);
  REQUIRE(cache.lookup(source) == nullptr);
  shift(49900, false);
  REQUIRE(source.has_heap_storage());
  REQUIRE(source.coefficient_limbs() < 100);
  // A used-size-only bound would admit this tiny value and pin its large owner.
  REQUIRE(cache.lookup(source) == nullptr);
  REQUIRE(cache.lookup(source) == nullptr);
  REQUIRE(cache.retained_limb_bytes() == 0);
  REQUIRE(cache.lookup(coefficient<Digits>(0)) == nullptr);
  REQUIRE(cache.lookup(coefficient<Digits>(97)) == nullptr);
}

TEMPLATE_TEST_CASE("Admitted divisor survives its last external source owner",
                   "[bc][number][prepared][divisor-cache]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  typename Digits::DivisorCache cache;
  typename Digits::PreparedDivisor const* prepared = nullptr;
  {
    auto source = coefficient_text<Digits>("100000000000000000000000000000000000000000000000007");
    REQUIRE(cache.lookup(source, 1) == nullptr);
    REQUIRE(cache.lookup(source, 1) == nullptr);
    prepared = cache.lookup(source, 1);
    REQUIRE(prepared != nullptr);
  }
  auto q = coefficient_text<Digits>("2000000000000000000000000000000000000000000000000141");
  auto r = q;
  q.divide(*prepared);
  r.modulo(*prepared);
  REQUIRE(decimal(q) == "2");
  REQUIRE(decimal(r) == "1");
  // Reconstructing the same number has a different ownership lifetime.
  auto replacement =
    coefficient_text<Digits>("100000000000000000000000000000000000000000000000007");
  REQUIRE(cache.lookup(replacement, 1) == nullptr);
}

TEMPLATE_TEST_CASE("Divisor admission bounds capacity grown during padding",
                   "[bc][number][prepared][divisor-cache]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using Limb = typename Digits::NumType;
  auto [source, places] = GD::Bc::Test::capacity_growth_source<Digits>();
  typename Digits::DivisorCache cache;
  auto const padding = static_cast<Limb>(Digits::base_log10_);
  REQUIRE(cache.lookup(source, padding) == nullptr);
  REQUIRE(cache.lookup(source, padding) == nullptr);
  auto const* prepared = cache.lookup(source, padding);
  REQUIRE(cache.retained_limb_bytes() <= Digits::DivisorCache::max_retained_limb_bytes);
  REQUIRE((prepared == nullptr) == (cache.retained_limb_bytes() == 0));
  REQUIRE(cache.lookup(source, padding) == prepared);
  REQUIRE(decimal(source) == "1" + std::string(places, '0'));

  auto numerator = source;
  numerator.mul_pow10(padding);
  if (prepared != nullptr) {
    numerator.divide(*prepared);
  }
  else {
    auto denominator = source;
    denominator.mul_pow10(padding);
    unprepared(numerator, denominator, false);
  }
  REQUIRE(decimal(numerator) == "1");
  cache.clear();
  REQUIRE(cache.retained_limb_bytes() == 0);
  REQUIRE(cache.lookup(source, padding) == nullptr);
}

TEMPLATE_TEST_CASE("Divisor admission rejects a heap owner shrunk to one limb",
                   "[bc][number][prepared][divisor-cache]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using Limb = typename Digits::NumType;
  auto source = coefficient_text<Digits>("100000000000000000000000000000000000000000000000001");
  source.div_pow10(Limb{50});
  REQUIRE(source.has_heap_storage());
  REQUIRE(source.coefficient_limbs() == 1);
  REQUIRE(decimal(source) == "1");
  typename Digits::DivisorCache cache;
  for (unsigned repeat = 0; repeat != 5; ++repeat) {
    REQUIRE(cache.lookup(source) == nullptr);
    REQUIRE(cache.retained_limb_bytes() == 0);
  }
  source.mul_pow10(static_cast<Limb>(3 * Digits::base_log10_));
  REQUIRE(cache.lookup(source) == nullptr);
  REQUIRE(cache.lookup(source) == nullptr);
  REQUIRE(cache.lookup(source) != nullptr);
  REQUIRE(cache.retained_limb_bytes() <= Digits::DivisorCache::max_retained_limb_bytes);
}
