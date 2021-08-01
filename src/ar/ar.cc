/** \file   ar.cc
 *  \brief  Main program for ar
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "ar.hh"

#include "gd/filesystem.hh"
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

#include "ar-files.hh"

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

template<typename It, typename SV>
It find_name(It begin, It end, SV const& name)
{
  return std::find_if(begin, end, [name](auto e) { return fs::path(e).filename() == name; });
}

template<typename ArIt, typename FIt>
void do_quick_append(fs::path const& archive, mode_t mode, ArIt ar_begin, ArIt ar_end,
                     FIt files_begin, FIt files_end, bool verbose)
{
  auto out_it = GD::Ar::archive_inserter(archive, mode, ar_begin, ar_end);
  std::for_each(files_begin, files_end, [&out_it, verbose](auto fname) {
    auto file = GD::Ar::InputFile(fname);
    if (verbose) {
      std::cout << "a - " << fname << '\n';
    }
    *out_it++ = file;
  });
  *out_it++ = out_it.commit_tag();
}

template<typename It>
void do_extract(GD::Ar::Member const& member, It files_begin, It files_end, bool verbose,
                bool allow_replacement, bool truncate_names)
{
  auto name = member.name();

  if (files_begin != files_end && find_name(files_begin, files_end, name) == files_end) {
    return;
  }

  if (truncate_names) {
    /* Truncate the name to the longest possible.  We ignore an error here and just assume the name
     * is valid.
     */
    auto max_len = ::pathconf(".", _PC_NAME_MAX);
    if (max_len != -1) {
      name = name.substr(0, max_len);
    }
  }

  if (fs::exists(name) && !allow_replacement) {
    return;
  }

  auto wf = GD::Ar::TxnWriteFile(fs::path(name), member.mode());
  wf.write(member.data());
  wf.commit();

  if (verbose) {
    std::cout << "x - " << name << '\n';
  }
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

constexpr bool is_read_only_action(Action action)
{
  return action == Action::print || action == Action::toc || action == Action::extract;
}

}  // namespace

int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  int c;
  Action action = Action::none;
  Position pos = Position::end;
  std::string pos_file;
  bool message_on_creation = true;
  [[maybe_unused]] bool force_ranlib = false;
  bool verbose = false;
  [[maybe_unused]] bool update_only = false;
  bool allow_replacement = true;
  bool truncate_names = false;

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
      message_on_creation = false;
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

  std::optional<GD::Ar::InputFile> file = std::nullopt;
  auto archive = fs::path(argv[optind++]);
  mode_t mode = umask(0);
  umask(mode);
  if (!fs::exists(archive)) {
    if (is_read_only_action(action)) {
      throw std::runtime_error("Missing input archive.");
    }
    if (message_on_creation) {
      std::cerr << "Creating file: " << archive << '\n';
    }
    mode = (~mode) & 0644;
  }
  else {
    file.emplace(GD::Ar::InputFile(archive));
    mode = file->mode();
  }

  auto ar_begin = file.has_value() ? GD::Ar::read_archive_begin(file.value())
                                   : GD::Ar::read_archive_end<GD::Ar::InputFile>();
  auto ar_end = GD::Ar::read_archive_end<GD::Ar::InputFile>();

  switch (action) {
  case Action::print:
    std::for_each(ar_begin, ar_end, [argv, argc, verbose](GD::Ar::Member const& member) {
      do_print(member, argv + optind, argv + argc, verbose);
    });
    break;
  case Action::quick:
    do_quick_append(archive, mode, ar_begin, ar_end, argv + optind, argv + argc, verbose);
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
  case Action::extract:
    std::for_each(
      ar_begin, ar_end,
      [argv, argc, verbose, allow_replacement, truncate_names](GD::Ar::Member const& member) {
        do_extract(member, argv + optind, argv + argc, verbose, allow_replacement, truncate_names);
      });
    break;
  default:
  case Action::none:
    abort();
  }
  return EXIT_SUCCESS;
}
