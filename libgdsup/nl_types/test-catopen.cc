/** \file   libgdsup/nl_types/test-catopen.cc
 *  \brief  Unit tests for catclose()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/stdlib.h"
#include "gd/string.h"

#include <catch2/catch.hpp>

#include <cerrno>

#include "nl_types/nl_types.h"
#include "nl_types/test-config.h"

TEST_CASE("catgets", "[nl_types][catopen][catgets]")
{
  errno = 0;
  nl_catd const cat = catopen(LARGE_FILE, 0);
  REQUIRE(cat != CATD_ERROR);
  REQUIRE(errno == 0);

  char const* failure = "FAILED";
  constexpr int set1 = 1;
  constexpr int hello = 1;
  constexpr int goodbye = 2;
  constexpr int missing1 = 3;
  constexpr int set2 = 2;
  constexpr int fred = 1;
  constexpr int george = 5;
  constexpr int missing2 = 3;
  constexpr int set3 = 3;
  constexpr int missing3 = 1;

  char* result = catgets(cat, set1, hello, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(::strcmp(result, "Hello world") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, set1, goodbye, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(::strcmp(result, "Goodbye world") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, set1, missing1, failure);  // NOLINT(concurrency-mt-unsafe)
  /* In failure case we get the exact string we passed in.  */
  REQUIRE(result == failure);
  REQUIRE(errno == ENOMSG);

  errno = 0;
  result = catgets(cat, set2, fred, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(::strcmp(result, "Fred") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, set2, george, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(::strcmp(result, "George") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, set2, missing2, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(result == failure);
  REQUIRE(errno == ENOMSG);

  errno = 0;
  result = catgets(cat, set3, missing3, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(result == failure);
  REQUIRE(errno == ENOMSG);

  errno = 0;
  REQUIRE(catclose(cat) != CATD_ERROR);
  REQUIRE(errno == 0);
}

TEST_CASE("catgets bad catalogues", "[nl_types][catopen][catgets]")
{
  char const* failure = "FAILED make this at least 24-bytes long";

  errno = 0;
  REQUIRE(catgets(CATD_ERROR, 1, 1, failure) == failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(errno == EBADF);

  errno = 0;
  // NOLINTNEXTLINE
  REQUIRE(catgets(reinterpret_cast<nl_catd>(failure), 1, 1, failure) ==
          failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(errno == EBADF);
}

TEST_CASE("catopen bad catalogues", "[nl_types][catopen]")
{
  // Fail to find a non-existent file by full-path
  errno = 0;
  nl_catd result = catopen(NON_EXISTANT_FILE, 0);
  REQUIRE(result == CATD_ERROR);
  REQUIRE(errno != 0);

  // Fail to find a non-existent file by nls path lookup.
  // NOLINTNEXTLINE(concurrency-mt-unsafe)
  REQUIRE(setenv("NLSPATH", TEST_NLSPATH_ROOT __DIR_SEPSTR "%N" __DIR_SEPSTR "%L", 1) == 0);
  errno = 0;
  result = catopen("non-existent-file", 0);
  REQUIRE(result == CATD_ERROR);
  REQUIRE(errno != 0);
}

TEST_CASE("catopen NLSPATH lookup", "[nl_types][catopen]")
{
  // NOLINTNEXTLINE(concurrency-mt-unsafe)
  REQUIRE(setenv("NLSPATH", TEST_NLSPATH_ROOT __DIR_SEPSTR "%L.%N.msg", 1) == 0);
  REQUIRE(setenv("LANG", "en_US.UTF-8", 1) == 0);  // NOLINT(concurrency-mt-unsafe)

  errno = 0;
  nl_catd const result = catopen("messages", 0);
  REQUIRE(result != CATD_ERROR);
  REQUIRE(errno == 0);

  char const* failure = "FAILED";
  char* s = catgets(result, NL_SETD, 1, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(strcmp(s, "en_US.UTF-8") == 0);
  REQUIRE(errno == 0);

  REQUIRE(catclose(result) != CATD_ERROR);
  REQUIRE(errno == 0);
}

namespace {
#ifdef _WIN32
#  define LC_MESSAGES LC_ALL  // NOLINT
#endif

void test_locale_lookup(char const* locale)
{
  char const* l2 = setlocale(LC_MESSAGES, locale);  // NOLINT(concurrency-mt-unsafe)
  if (l2 == nullptr || strcmp(l2, locale) != 0) {
    INFO("SKIP: test_locale_lookup(" << locale << ") - locale does not exist ("
                                     << (l2 == nullptr ? "(NULL)" : l2) << ").\n");
    REQUIRE(0 == 0);
    return;
  }

  errno = 0;
  nl_catd const result = catopen("messages", NL_CAT_LOCALE);
  REQUIRE(result != CATD_ERROR);
  REQUIRE(errno == 0);

  char const* failure = "FAILED";
  char* s = catgets(result, NL_SETD, 1, failure);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(strcmp(s, locale) == 0);
  REQUIRE(errno == 0);

  REQUIRE(catclose(result) != CATD_ERROR);
  REQUIRE(errno == 0);
}
}  // namespace

TEST_CASE("catopen NLSPATH lookup 2", "[nl_types][catopen]")
{
  // NOLINTNEXTLINE(concurrency-mt-unsafe)
  REQUIRE(setenv("NLSPATH",
                 TEST_NLSPATH_ROOT __DIR_SEPSTR
                 "%N.msg" __PATH_SEPSTR TEST_NLSPATH_ROOT __DIR_SEPSTR "%l" __DIR_SEPSTR
                 "%t" __DIR_SEPSTR "%c" __DIR_SEPSTR
                 "%N.msg" __PATH_SEPSTR TEST_NLSPATH_ROOT __DIR_SEPSTR "%N.msg",
                 1) == 0);
  REQUIRE(setenv("LANG", "fr_FR", 1) == 0);  // NOLINT(concurrency-mt-unsafe)

  test_locale_lookup("en_US.UTF-8");
  test_locale_lookup("en_US");
}
