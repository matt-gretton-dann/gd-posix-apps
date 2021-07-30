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
#include <ctime>
#include <iomanip>
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

template<typename It>
void do_print(GD::Ar::Member const& member, It files_begin, It files_end, bool verbose)
{
  std::string name = member.name();
  if (files_begin != files_end) {
    auto found = std::find(files_begin, files_end, member.name());
    if (found == files_end) {
      return;
    }
    name = *found;
  }

  if (verbose) {
    std::cout << "\n<" << name << ">\n\n";
  }

  auto data = member.data();
  auto raw_data = data.data();
  auto count = data.size_bytes();
  while (count != 0) {
    auto res = ::write(STDOUT_FILENO, raw_data,
                       std::min((std::size_t)std::numeric_limits<::ssize_t>::max(), count));
    if (res == -1) {
      if (errno != EINTR) {
        throw std::system_error(errno, std::generic_category(), "Whilst reading.");
      }
    }
    else {
      raw_data += res;
      count -= res;
    }
  }
}

template<typename It>
void do_toc(GD::Ar::Member const& member, It files_begin, It files_end)
{
  /* POSIX says that we should print the name of the files from the command-line if we are
   * filtering.  */
  if (files_begin == files_end) {
    std::cout << member.name() << '\n';
  }
  else {
    auto found = std::find(files_begin, files_end, member.name());
    if (found != files_end) {
      std::cout << *found << '\n';
    }
  }
}

std::string to_mode_string(mode_t mode)
{
  std::string result(9, '-');

  if (mode & S_IRUSR) {
    result[0] = 'r';
  }
  if (mode & S_IWUSR) {
    result[1] = 'w';
  }
  if ((mode & S_ISUID)) {
    result[2] = (mode & S_IXUSR) ? 's' : 'S';
  }
  else if (mode & S_IXUSR) {
    result[2] = 'x';
  }
  if (mode & S_IRGRP) {
    result[3] = 'r';
  }
  if (mode & S_IWGRP) {
    result[4] = 'w';
  }
  if ((mode & S_ISGID)) {
    result[5] = (mode & S_IXGRP) ? 's' : 'S';
  }
  else if (mode & S_IXGRP) {
    result[5] = 'x';
  }
  if (mode & S_IROTH) {
    result[6] = 'r';
  }
  if (mode & S_IWOTH) {
    result[7] = 'w';
  }
  if ((mode & S_ISVTX) && S_ISDIR(mode)) {
    result[8] = (mode & S_IXOTH) ? 't' : 'T';
  }
  else if (mode & S_IXOTH) {
    result[8] = 'x';
  }

  return result;
}

template<typename It>
void do_verbose_toc(GD::Ar::Member const& member, It files_begin, It files_end)
{
  /* POSIX says that we should print the name of the files from the command-line if we are
   * filtering.  */
  std::string name = member.name();
  if (files_begin != files_end) {
    auto found = std::find(files_begin, files_end, member.name());
    if (found == files_end) {
      return;
    }
    name = *found;
  }
  auto m = member.mtime();
  std::tm* mtime = std::localtime(&m);
  std::cout << to_mode_string(member.mode()) << ' ' << member.uid() << '/' << member.gid() << ' '
            << member.size_bytes() << ' ' << std::put_time(mtime, "%b %e %H:%M %Y") << ' ' << name
            << '\n';
}

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

  switch (action) {
  case Action::print:
    std::for_each(ar_begin, ar_end, [argv, argc, verbose](GD::Ar::Member const& member) {
      do_print(member, argv + optind, argv + argc, verbose);
    });
    break;
  case Action::toc:
    std::for_each(ar_begin, ar_end, [argv, argc, verbose](GD::Ar::Member const& member) {
      if (verbose) {
        do_verbose_toc(member, argv + optind, argv + argc);
      }
      else {
        do_toc(member, argv + optind, argv + argc);
      }
    });
    break;
  default:
  case Action::none:
    break;
  }
  return EXIT_SUCCESS;
}
