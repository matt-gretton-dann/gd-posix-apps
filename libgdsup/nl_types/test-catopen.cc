/** \file   libgdsup/nl_types/test-catopen.cc
 *  \brief  Unit tests for catclose()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/stdlib.h"
#include "gd/string.h"

#include <catch2/catch.hpp>

#include <errno.h>

#include "nl_types/nl_types.h"
#include "nl_types/test-config.h"

TEST_CASE("catgets", "[nl_types][catopen][catgets]")
{
  errno = 0;
  nl_catd cat = catopen(LARGE_FILE, 0);
  REQUIRE(cat != CATD_ERROR);
  REQUIRE(errno == 0);

  char const* failure = "FAILED";
  char* result = catgets(cat, 1, 1, failure);
  REQUIRE(::strcmp(result, "Hello world") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, 1, 2, failure);
  REQUIRE(::strcmp(result, "Goodbye world") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, 1, 3, failure);
  /* In failure case we get the exact string we passed in.  */
  REQUIRE(result == failure);
  REQUIRE(errno == ENOMSG);

  errno = 0;
  result = catgets(cat, 2, 1, failure);
  REQUIRE(::strcmp(result, "Fred") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, 2, 5, failure);
  REQUIRE(::strcmp(result, "George") == 0);
  REQUIRE(errno == 0);

  result = catgets(cat, 2, 3, failure);
  REQUIRE(result == failure);
  REQUIRE(errno == ENOMSG);

  errno = 0;
  result = catgets(cat, 3, 1, failure);
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
  REQUIRE(catgets(CATD_ERROR, 1, 1, failure) == failure);
  REQUIRE(errno == EBADF);

  errno = 0;
  REQUIRE(catgets((nl_catd)failure, 1, 1, failure) == failure);
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
  REQUIRE(setenv("NLSPATH", TEST_NLSPATH_ROOT __DIR_SEPSTR "%N" __DIR_SEPSTR "%L", 1) == 0);
  errno = 0;
  result = catopen("non-existent-file", 0);
  REQUIRE(result == CATD_ERROR);
  REQUIRE(errno != 0);
}

TEST_CASE("catopen NLSPATH lookup", "[nl_types][catopen]")
{
  REQUIRE(setenv("NLSPATH", TEST_NLSPATH_ROOT __DIR_SEPSTR "%L.%N.msg", 1) == 0);
  REQUIRE(setenv("LANG", "en_US.UTF-8", 1) == 0);

  errno = 0;
  nl_catd result = catopen("messages", 0);
  REQUIRE(result != CATD_ERROR);
  REQUIRE(errno == 0);

  char const* failure = "FAILED";
  char* s = catgets(result, NL_SETD, 1, failure);
  REQUIRE(strcmp(s, "en_US.UTF-8") == 0);
  REQUIRE(errno == 0);

  REQUIRE(catclose(result) != CATD_ERROR);
  REQUIRE(errno == 0);
}

namespace {
#ifdef _WIN32
#  define LC_MESSAGES LC_ALL
#endif

void test_locale_lookup(char const* locale)
{
  char const* l2 = setlocale(LC_MESSAGES, locale);
  if (l2 == NULL || strcmp(l2, locale) != 0) {
    INFO("SKIP: test_locale_lookup(" << locale << ") - locale does not exist ("
                                     << (l2 == NULL ? "(NULL)" : l2) << ").\n");
    REQUIRE(0 == 0);
    return;
  }

  errno = 0;
  nl_catd result = catopen("messages", NL_CAT_LOCALE);
  REQUIRE(result != CATD_ERROR);
  REQUIRE(errno == 0);

  char const* failure = "FAILED";
  char* s = catgets(result, NL_SETD, 1, failure);
  REQUIRE(strcmp(s, locale) == 0);
  REQUIRE(errno == 0);

  REQUIRE(catclose(result) != CATD_ERROR);
  REQUIRE(errno == 0);
}
}  // namespace

TEST_CASE("catopen NLSPATH lookup 2", "[nl_types][catopen]")
{
  REQUIRE(setenv("NLSPATH",
                 TEST_NLSPATH_ROOT __DIR_SEPSTR
                 "%N.msg" __PATH_SEPSTR TEST_NLSPATH_ROOT __DIR_SEPSTR "%l" __DIR_SEPSTR
                 "%t" __DIR_SEPSTR "%c" __DIR_SEPSTR
                 "%N.msg" __PATH_SEPSTR TEST_NLSPATH_ROOT __DIR_SEPSTR "%N.msg",
                 1) == 0);
  REQUIRE(setenv("LANG", "fr_FR", 1) == 0);

  test_locale_lookup("en_US.UTF-8");
  test_locale_lookup("en_US");
}
