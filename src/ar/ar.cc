/** \file   ar.cc
 *  \brief  Main program for ar
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "ar.hh"

#include "gd/stdlib.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "ar-messages.hh"

#include <assert.h>
#include <iostream>
#include <vector>

using Msg = GD::Ar::Msg;

namespace {
/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  constexpr auto usage = Msg::usage;
  std::cerr << GD::program_name() << ": "
            << GD::Ar::Messages::get().format(GD::Ar::Set::ar, msg, args...) << '\n'
            << GD::Ar::Messages::get().format(GD::Ar::Set::ar, usage, GD::program_name()) << '\n';
  ::exit(1);
}

void do_toc(GD::Ar::Member const& member) { std::cout << member.name() << '\n'; }

enum class Action { none, del, move, print, quick, replace, toc, extract };
enum class Position { end, before, after };
}  // namespace

int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  int c;
  Action action = Action::none;
  Position pos = Position::end;
  std::string pos_file;
  std::string archive;
  std::vector<std::string> files;
  [[maybe_unused]] bool suppress_diagnostics = false;
  [[maybe_unused]] bool force_ranlib = false;
  [[maybe_unused]] bool verbose = false;
  [[maybe_unused]] bool update_only = false;
  [[maybe_unused]] bool allow_replacement = true;
  [[maybe_unused]] bool truncate_names = false;

  auto set_action = [&action, &c](Action act) {
    if (action != Action::none) {
      error(Msg::cannot_specify_more_than_one_action, c);
    }
    action = act;
  };

  while ((c = ::getopt(argc, argv, ":CTabcdimpqrstuvx")) != -1) {
    switch (c) {
    case 'C':
      allow_replacement = false;
      break;
    case 'T':
      truncate_names = true;
      break;
    case 'a':
      pos = Position::after;
      break;
    case 'b':
    case 'i':
      pos = Position::before;
      break;
    case 'c':
      suppress_diagnostics = true;
      break;
    case 'd':
      set_action(Action::del);
      break;
    case 'm':
      set_action(Action::move);
      break;
    case 'p':
      set_action(Action::print);
      break;
    case 'q':
      set_action(Action::quick);
      break;
    case 'r':
      set_action(Action::replace);
      break;
    case 's':
      force_ranlib = true;
      break;
    case 't':
      set_action(Action::toc);
      break;
    case 'u':
      update_only = true;
      break;
    case 'v':
      verbose = true;
      break;
    case 'x':
      set_action(Action::extract);
      break;
    case ':':
    case '?':
    default:
      error(Msg::unrecognised_option, (char)optopt);
      break;
    }
  }

  if (action == Action::none) {
    error(Msg::missing_action);
  }

  if (pos != Position::end) {
    if (optind >= argc) {
      error(Msg::missing_position_name);
    }
    pos_file = argv[optind++];
  }

  if (optind >= argc) {
    error(Msg::missing_archive_name);
  }
  auto file = GD::Ar::InputFile(argv[optind++]);

  auto ar_begin = GD::Ar::read_archive_begin(file);
  auto ar_end = GD::Ar::read_archive_end<GD::Ar::InputFile>();

  std::copy(argv + optind, argv + argc, std::back_inserter(files));

  switch (action) {
  case Action::toc:
    while (ar_begin != ar_end) {
      do_toc(*ar_begin);
      ++ar_begin;
    }
    break;
  default:
  case Action::none:
    break;
  }
  return EXIT_SUCCESS;
}
