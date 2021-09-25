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

/** \brief  Actions we will do on an archive.  Comes from command line options. */
enum class Action {
  none,     ///< No action
  del,      ///< Delete (-d)
  move,     ///< Move (-m)
  print,    ///< Print (-p)
  quick,    ///< Quick qppend (-q)
  replace,  ///< Replace (-r)
  toc,      ///< Table of contents (-t)
  extract   ///< Extract (-x)
};

/** \brief  Where to put new archive members.  */
enum class Position {
  end,     ///< At end of archive
  before,  ///< Before a named member (-b, -i)
  after    ///< After a named member (-a)
};

/** \brief  Archive management flags */
enum class Flags : unsigned {
  message_on_creation = 0x1,  ///< Give a message on archive creation
  force_ranlib = 0x2,         ///< Force regeneration of symbol table
  verbose = 0x4,              ///< Be vebose
  update_newer = 0x8,         ///< Only update if file is newer than member.
  allow_replacement = 0x10,   ///< Allow replacement of existing members
  truncate_names = 0x20       ///< Truncate names if needed.
};

Flags operator|(Flags l, Flags r)
{
  return static_cast<Flags>(static_cast<unsigned>(l) | static_cast<unsigned>(r));
}

Flags operator&(Flags l, Flags r)
{
  return static_cast<Flags>(static_cast<unsigned>(l) & static_cast<unsigned>(r));
}

Flags operator~(Flags f) { return static_cast<Flags>(~static_cast<unsigned>(f)); }

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

/** \brief        Find if a given name is in the list of files.
 *  \tparam It    Iterator type
 *  \tparam SV    String view type
 *  \param  begin First iterator to examine
 *  \param  end   One past the end
 *  \param  name  Name to check
 *
 * Returns iterator to file whose filename component matches \a name, or \a end if none found.
 */
template<typename It, typename SV>
It find_name(It begin, It end, SV const& name)
{
  return std::find_if(begin, end, [name](auto e) { return fs::path(e).filename() == name; });
}

/** \brief              Do the delete action
 *  \tparam ArIt        Archive iterator type
 *  \tparam FIt         File Iterator type
 *  \param  archive     Filename of output archive.
 *  \param  mode        Mode to create archive with
 *  \param  ar_begin    First archive member to process
 *  \param  ar_end      One past end of archive members
 *  \param  files_begin First file name to process
 *  \param  files_end   First past end of file names
 *  \param  flags       Flags.
 */
template<typename ArIt, typename FIt>
void do_delete(fs::path const& archive, mode_t mode, ArIt ar_begin, ArIt ar_end, FIt files_begin,
               FIt files_end, Flags flags)
{
  GD::Ar::Format format = (ar_begin != ar_end) ? ar_begin.format() : GD::Ar::Format::gnu;
  bool verbose = (flags & Flags::verbose) == Flags::verbose;
  auto out_it = GD::Ar::archive_inserter(archive, format, mode);
  std::remove_copy_if(ar_begin, ar_end, out_it, [files_begin, files_end, verbose](auto member) {
    auto found = find_name(files_begin, files_end, member.name());
    if (found != files_end && verbose) {
      std::cout << "d - " << *found << '\n';
    }
    return found != files_end;
  });
  *out_it++ = out_it.commit_tag();
}

/** \brief              Do the move action
 *  \tparam ArIt        Archive iterator type
 *  \tparam FIt         File Iterator type
 *  \param  archive     Filename of output archive.
 *  \param  mode        Mode to create archive with
 *  \param  ar_begin    First archive member to process
 *  \param  ar_end      One past end of archive members
 *  \param  files_begin First file name to process
 *  \param  files_end   First past end of file names
 *  \param  pos         Where to insert moved members (end, before, after)
 *  \param  posname     Name of member to insert beside (if pos is before or after)
 *  \param  flags       Flags.
 */
template<typename ArIt, typename FIt>
void do_move(fs::path const& archive, mode_t mode, ArIt ar_begin, ArIt ar_end, FIt files_begin,
             FIt files_end, Position pos, std::optional<std::string> const& posname, Flags flags)
{
  GD::Ar::Format format = (ar_begin != ar_end) ? ar_begin.format() : GD::Ar::Format::gnu;
  bool verbose = (flags & Flags::verbose) == Flags::verbose;
  auto out_it = GD::Ar::archive_inserter(archive, format, mode);
  std::vector<GD::Ar::Member> moved;
  std::vector<GD::Ar::Member> tail;
  bool pending = false;
  std::for_each(
    ar_begin, ar_end,
    [&pending, &out_it, &moved, &tail, files_begin, files_end, posname, pos, verbose](auto member) {
      auto found = find_name(files_begin, files_end, member.name());
      if (found != files_end) {
        if (verbose) {
          std::cout << "m - " << *found << '\n';
        }
        moved.push_back(member);
      }
      else if (pending) {
        tail.push_back(member);
      }
      else if (pos != Position::end && member.name() == fs::path(*posname).filename()) {
        pending = true;
        if (pos == Position::after) {
          *out_it++ = member;
        }
        else {
          tail.push_back(member);
        }
      }
      else {
        *out_it++ = member;
      }
    });
  std::copy(moved.begin(), moved.end(), out_it);
  std::copy(tail.begin(), tail.end(), out_it);
  *out_it++ = out_it.commit_tag();
}

/** \brief              Do the replace action
 *  \tparam ArIt        Archive iterator type
 *  \tparam FIt         File Iterator type
 *  \param  archive     Filename of output archive.
 *  \param  mode        Mode to create archive with
 *  \param  ar_begin    First archive member to process
 *  \param  ar_end      One past end of archive members
 *  \param  files_begin First file name to process
 *  \param  files_end   First past end of file names
 *  \param  pos         Where to insert moved members (end, before, after)
 *  \param  posname     Name of member to insert beside (if pos is before or after)
 *  \param  flags       Flags.
 */
template<typename ArIt, typename FIt>
void do_replace(fs::path const& archive, mode_t mode, ArIt ar_begin, ArIt ar_end, FIt files_begin,
                FIt files_end, Position pos, std::optional<std::string> const& posname, Flags flags)
{
  GD::Ar::Format format = (ar_begin != ar_end) ? ar_begin.format() : GD::Ar::Format::gnu;
  bool verbose = (flags & Flags::verbose) == Flags::verbose;
  bool update_newer = (flags & Flags::update_newer) == Flags::update_newer;
  auto out_it = GD::Ar::archive_inserter(archive, format, mode);
  std::vector<std::string> files;
  std::copy(files_begin, files_end, std::back_inserter(files));

  for (; ar_begin != ar_end; ++ar_begin) {
    auto member = *ar_begin;
    if (pos == Position::before && member.name() == *posname) {
      break;
    }
    auto found = find_name(files.begin(), files.end(), member.name());
    bool updated = false;
    if (found != files.end()) {
      auto file = GD::Ar::InputFile(*found);
      if (!update_newer || file.mtime() > member.mtime()) {
        *out_it++ = file;
        if (verbose) {
          std::cout << "r - " << *found << '\n';
        }
        updated = true;
      }
      while ((found = find_name(files.erase(found), files.end(), member.name())) != files.end()) {
        std::cerr << "Warning: Skipping due to duplicate member name " << *found << '\n';
      }
    }
    if (!updated) {
      *out_it++ = member;
    }
    if (pos == Position::after && member.name() == *posname) {
      break;
    }
  }

  std::vector<GD::Ar::Member> tail;
  std::copy(ar_begin, ar_end, std::back_inserter(tail));

  for (auto const& file : files) {
    auto found = std::find_if(tail.begin(), tail.end(),
                              [basename = fs::path(file).filename()](auto const& member) {
                                return member.name() == basename;
                              });
    if (found == tail.end()) {
      auto f = GD::Ar::InputFile(file);
      *out_it++ = f;
      if (verbose) {
        std::cout << "a - " << file << '\n';
      }
    }
  }

  for (auto const& member : tail) {
    auto found = find_name(files.begin(), files.end(), member.name());
    bool updated = false;
    if (found != files.end()) {
      auto file = GD::Ar::InputFile(*found);
      if (!update_newer || file.mtime() > member.mtime()) {
        *out_it++ = file;
        if (verbose) {
          std::cout << "r - " << *found << '\n';
        }
        updated = true;
      }
      while ((found = find_name(files.erase(found), files.end(), member.name())) != files.end()) {
        std::cerr << "Warning: Skipping due to duplicate member name " << *found << '\n';
      }
    }
    if (!updated) {
      *out_it++ = member;
    }
  }

  *out_it++ = out_it.commit_tag();
}

/** \brief              Do the replace action
 *  \tparam ArIt        Archive iterator type
 *  \tparam FIt         File Iterator type
 *  \param  archive     Filename of output archive.
 *  \param  mode        Mode to create archive with
 *  \param  ar_begin    First archive member to process
 *  \param  ar_end      One past end of archive members
 *  \param  files_begin First file name to process
 *  \param  files_end   First past end of file names
 *  \param  flags       Flags.
 */
template<typename ArIt, typename FIt>
void do_quick_append(fs::path const& archive, mode_t mode, ArIt ar_begin, ArIt ar_end,
                     FIt files_begin, FIt files_end, Flags flags)
{
  GD::Ar::Format format = (ar_begin != ar_end) ? ar_begin.format() : GD::Ar::Format::gnu;
  bool verbose = (flags & Flags::verbose) == Flags::verbose;
  auto out_it = GD::Ar::archive_inserter(archive, format, mode);
  std::copy(ar_begin, ar_end, out_it);
  std::for_each(files_begin, files_end, [&out_it, verbose](auto fname) {
    if (verbose) {
      std::cout << "a - " << fname << '\n';
    }
    auto file = GD::Ar::InputFile(fname);
    *out_it++ = file;
  });
  *out_it++ = out_it.commit_tag();
}

/** \brief        Extract a member
 *  \param fname  Filename of member
 *  \param member  Member details
 *  \param flag   Flags
 */
void do_extract(std::string const& fname, GD::Ar::Member const& member, Flags flags)
{
  auto name = fname;
  bool truncate_names = (flags & Flags::truncate_names) == Flags::truncate_names;
  bool allow_replacement = (flags & Flags::allow_replacement) == Flags::allow_replacement;
  bool verbose = (flags & Flags::verbose) == Flags::verbose;

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

/** \brief        Print a member
 *  \param fname  Filename of member
 *  \param member  Member details
 *  \param flag   Flags
 */
void do_print(std::string const& fname, GD::Ar::Member const& member, Flags flags)
{
  if ((flags & Flags::verbose) == Flags::verbose) {
    std::cout << "\n<" << fname << ">\n\n";
  }

  auto data = member.data();
  auto raw_data = data.data();
  auto count = data.size_bytes();
  while (count != 0) {
    auto res = ::write(STDOUT_FILENO, raw_data,
                       std::min((std::size_t)std::numeric_limits<::ssize_t>::max(), count));
    if (res == -1) {
      if (errno != EINTR) {
        error(Msg::stdout_write_failed, std::strerror(errno));
      }
    }
    else {
      raw_data += res;
      count -= res;
    }
  }
}

/** \brief  Convert a mode_t to a mode string.  */
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

/** \brief        Print the ToC for a member
 *  \param fname  Filename of member
 *  \param member  Member details
 *  \param flag   Flags
 */
void do_toc(std::string const& fname, GD::Ar::Member const& member, Flags flags)
{
  if ((flags & Flags::verbose) == Flags::verbose) {
    auto m = member.mtime();
    std::tm* mtime = std::localtime(&m);
    std::cout << to_mode_string(member.mode()) << ' ' << member.uid() << '/' << member.gid() << ' '
              << member.size_bytes() << ' ' << std::put_time(mtime, "%b %e %H:%M %Y") << ' '
              << fname << '\n';
  }
  else {
    std::cout << fname << '\n';
  }
}

/** \brief              Do a 'simple' action
 *  \tparam ArIt        Archive iterator type
 *  \tparam FIt         File Iterator type
 *  \param  archive     Filename of output archive.
 *  \param  mode        Mode to create archive with
 *  \param  ar_begin    First archive member to process
 *  \param  ar_end      One past end of archive members
 *  \param  files_begin First file name to process
 *  \param  files_end   First past end of file names
 *  \param  flags       Flags.
 *  \param  act_fn      Action function to do on members that we want to process
 */
template<typename ArIt, typename FileIt, typename ActFn>
void do_action(fs::path const& archive, mode_t mode, ArIt ar_begin, ArIt ar_end, FileIt files_begin,
               FileIt files_end, Flags flags, ActFn act_fn)
{
  std::vector<GD::Ar::Member> members;
  while (ar_begin != ar_end) {
    auto const member = *ar_begin++;
    if ((flags & Flags::force_ranlib) == Flags::force_ranlib) {
      members.push_back(member);
    }
    if (files_begin != files_end) {
      auto found = find_name(files_begin, files_end, member.name());
      if (found != files_end) {
        act_fn(*found, member, flags);
      }
    }
    else {
      act_fn(member.name(), member, flags);
    }
  }

  if (!members.empty()) {
    GD::Ar::Format format = members.front().format();
    auto out_it = GD::Ar::archive_inserter(archive, format, mode);
    std::copy(members.begin(), members.end(), out_it);
    *out_it++ = out_it.commit_tag();
  }
}

/** \brief Is \a action a readonly action?  */
constexpr bool is_read_only_action(Action action)
{
  return action == Action::print || action == Action::toc || action == Action::extract;
}

struct State
{
  Action action = Action::none;
  Position pos = Position::end;
  Flags flags = Flags::message_on_creation | Flags::allow_replacement;
  std::optional<std::string> pos_file;
  fs::path archive;
  char** file_begin;
  char** file_end;
};

State process_command_line(int argc, char** argv)
{
  State state;
  int c = 0;
  auto set_action = [&state, &c](Action act) {
    if (state.action != Action::none) {
      error(Msg::cannot_specify_more_than_one_action, c);
    }
    state.action = act;
  };

  while ((c = ::getopt(argc, argv, ":CTabcdimpqrstuvx:")) != -1) {
    switch (c) {
    case 'C':
      state.flags = state.flags | ~Flags::allow_replacement;
      break;
    case 'T':
      state.flags = state.flags | Flags::truncate_names;
      break;
    case 'a':
      state.pos = Position::after;
      break;
    case 'b':
    case 'i':
      state.pos = Position::before;
      break;
    case 'c':
      state.flags = state.flags | ~Flags::message_on_creation;
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
      state.flags = state.flags | Flags::force_ranlib;
      break;
    case 't':
      set_action(Action::toc);
      break;
    case 'u':
      state.flags = state.flags | Flags::update_newer;
      break;
    case 'v':
      state.flags = state.flags | Flags::verbose;
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

  if (state.action == Action::none) {
    error(Msg::missing_action);
  }

  if (state.pos != Position::end) {
    if (optind >= argc) {
      error(Msg::missing_position_name);
    }
    state.pos_file = argv[optind++];
  }

  if (optind >= argc) {
    error(Msg::missing_archive_name);
  }

  state.archive = fs::path(argv[optind++]);
  state.file_begin = argv + optind;
  state.file_end = argv + argc;

  return state;
}
}  // namespace

int main(int argc, char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  State state = process_command_line(argc, argv);

  std::optional<GD::Ar::InputFile> file = std::nullopt;
  mode_t mode = umask(0);
  umask(mode);
  if (!fs::exists(state.archive)) {
    if (is_read_only_action(state.action)) {
      error(Msg::missing_input_archive, state.archive.native());
    }
    if ((state.flags & Flags::message_on_creation) == Flags::message_on_creation) {
      std::cerr << GD::Ar::Messages::get().format(GD::Ar::Set::ar, Msg::creating_archive,
                                                  state.archive.native());
    }
    mode = (~mode) & 0644;
  }
  else {
    file.emplace(GD::Ar::InputFile(state.archive));
    mode = file->mode();
  }

  auto ar_begin = file.has_value() ? GD::Ar::read_archive_begin(file.value())
                                   : GD::Ar::read_archive_end<GD::Ar::InputFile>();
  auto ar_end = GD::Ar::read_archive_end<GD::Ar::InputFile>();

  switch (state.action) {
  case Action::del:
    do_delete(state.archive, mode, ar_begin, ar_end, state.file_begin, state.file_end, state.flags);
    break;
  case Action::move:
    do_move(state.archive, mode, ar_begin, ar_end, state.file_begin, state.file_end, state.pos,
            state.pos_file, state.flags);
    break;
  case Action::print:
    do_action(state.archive, mode, ar_begin, ar_end, state.file_begin, state.file_end, state.flags,
              do_print);
    break;
  case Action::quick:
    do_quick_append(state.archive, mode, ar_begin, ar_end, state.file_begin, state.file_end,
                    state.flags);
    break;
  case Action::replace:
    do_replace(state.archive, mode, ar_begin, ar_end, state.file_begin, state.file_end, state.pos,
               state.pos_file, state.flags);
    break;
  case Action::toc:
    do_action(state.archive, mode, ar_begin, ar_end, state.file_begin, state.file_end, state.flags,
              do_toc);
    break;
  case Action::extract:
    do_action(state.archive, mode, ar_begin, ar_end, state.file_begin, state.file_end, state.flags,
              do_extract);
    break;
  default:
  case Action::none:
    abort();
  }
  return EXIT_SUCCESS;
}
