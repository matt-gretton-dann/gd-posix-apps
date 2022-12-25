#include "gd/string.h"
#include "gd/unistd.h"

#include <catch2/catch.hpp>

#include <array>

template<std::size_t N>
using ArgV = std::array<char*, N>;

template<std::size_t N>
void check_getopt(ArgV<N> const& args, char const* stropt, int ec)
{
  int const c = getopt(N - 1, args.data(), stropt);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(ec == c);
}

template<std::size_t N>
void check_getopt(ArgV<N> const& args, char const* stropt, int ec, char const* eoptarg)
{
  int const c = getopt(N - 1, args.data(), stropt);  // NOLINT(concurrency-mt-unsafe)
  REQUIRE(ec == c);
  REQUIRE(strcmp(optarg, eoptarg) == 0);
}

// NOLINTNEXTLINE
TEST_CASE("getopt-test1", "[unistd][getopt]")
{
  // This test is the first example from
  // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html
  __gd_getopt_reset();
  constexpr std::size_t argc = 6;
  ArgV<argc> const argv = {strdup("cmd"),   strdup("-ao"),   strdup("arg"),
                           strdup("path1"), strdup("path2"), nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, 'a');
  check_getopt(argv, stropt, 'o', "arg");
  check_getopt(argv, stropt, -1);
  REQUIRE(optind == 3);
}

TEST_CASE("getopt-test2", "[unistd][getopt]")
{
  // This test is the second example from
  // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html
  __gd_getopt_reset();
  constexpr std::size_t argc = 7;
  ArgV<argc> const argv = {strdup("cmd"),   strdup("-a"),    strdup("-o"), strdup("arg"),
                           strdup("path1"), strdup("path2"), nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, 'a');
  check_getopt(argv, stropt, 'o', "arg");
  check_getopt(argv, stropt, -1);
  REQUIRE(optind == 4);
}

TEST_CASE("getopt-test3", "[unistd][getopt]")
{
  // This test is the third example from
  // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html
  __gd_getopt_reset();
  constexpr std::size_t argc = 7;
  ArgV<argc> const argv = {strdup("cmd"),   strdup("-o"),    strdup("arg"), strdup("-a"),
                           strdup("path1"), strdup("path2"), nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, 'o', "arg");
  check_getopt(argv, stropt, 'a');
  check_getopt(argv, stropt, -1);
  REQUIRE(optind == 4);
}

TEST_CASE("getopt-test4", "[unistd][getopt]")
{
  // This test is the fourth example from
  // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html
  __gd_getopt_reset();
  constexpr std::size_t argc = 8;
  ArgV<argc> const argv = {strdup("cmd"), strdup("-a"),    strdup("-o"),    strdup("arg"),
                           strdup("--"),  strdup("path1"), strdup("path2"), nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, 'a');
  check_getopt(argv, stropt, 'o', "arg");
  check_getopt(argv, stropt, -1);
  REQUIRE(optind == 5);
}

TEST_CASE("getopt-test5", "[unistd][getopt]")
{
  // This test is the fifth example from
  // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html
  __gd_getopt_reset();
  constexpr std::size_t argc = 6;
  ArgV<argc> const argv = {strdup("cmd"),   strdup("-a"),    strdup("-oarg"),
                           strdup("path1"), strdup("path2"), nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, 'a');
  check_getopt(argv, stropt, 'o', "arg");
  check_getopt(argv, stropt, -1);
  REQUIRE(optind == 3);
}

TEST_CASE("getopt-test6", "[unistd][getopt]")
{
  // This test is the sixth example from
  // https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html
  __gd_getopt_reset();
  constexpr std::size_t argc = 5;
  ArgV<argc> const argv = {strdup("cmd"), strdup("-aoarg"), strdup("path1"), strdup("path2"),
                           nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, 'a');
  check_getopt(argv, stropt, 'o', "arg");
  check_getopt(argv, stropt, -1);
  REQUIRE(optind == 2);
}

TEST_CASE("getopt-test-unrecognised", "[unistd][getopt]")
{
  __gd_getopt_reset();
  constexpr std::size_t argc = 3;
  ArgV<argc> const argv = {strdup("cmd"), strdup("-z"), nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, '?');
  REQUIRE(optopt == 'z');
}

TEST_CASE("getopt-test-missing-argument1", "[unistd][getopt]")
{
  __gd_getopt_reset();
  constexpr std::size_t argc = 3;
  ArgV<argc> const argv = {strdup("cmd"), strdup("-o"), nullptr};
  char const* stropt = ":abf:o:";

  check_getopt(argv, stropt, ':');
  REQUIRE(optopt == 'o');
}

TEST_CASE("getopt-test-missing-argument2", "[unistd][getopt]")
{
  __gd_getopt_reset();
  constexpr std::size_t argc = 3;
  ArgV<argc> const argv = {strdup("cmd"), strdup("-o"), nullptr};
  char const* stropt = "abf:o:";

  check_getopt(argv, stropt, '?');
  REQUIRE(optopt == 'o');
}
