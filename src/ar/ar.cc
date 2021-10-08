/** \file   ar.cc
 *  \brief  Main program for ar
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "ar.hh"

#include "gd/filesystem.hh"
#include "gd/span.hh"
#include "gd/stdlib.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "ar-messages.hh"

#include <cassert>
#include <clocale>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <vector>

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

auto operator|(Flags l, Flags r) noexcept -> Flags
{
  return static_cast<Flags>(static_cast<unsigned>(l) | static_cast<unsigned>(r));
}

auto operator&(Flags l, Flags r) noexcept -> Flags
{
  return static_cast<Flags>(static_cast<unsigned>(l) & static_cast<unsigned>(r));
}

auto operator~(Flags f) noexcept -> Flags { return static_cast<Flags>(~static_cast<unsigned>(f)); }

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
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
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
auto find_name(It begin, It end, SV const& name) -> It
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
  *out_it++ = GD::Ar::WriteIterator<GD::TxnWriteFile>::commit_tag();
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
      if (pos != Position::end && member.name() == fs::path(*posname).filename()) {
        pending = true;
        if (pos == Position::after) {
          *out_it++ = member;
        }
        else {
          tail.push_back(member);
        }
      }
      else if (found != files_end) {
        if (verbose) {
          std::cout << "m - " << *found << '\n';
        }
        moved.push_back(member);
      }
      else if (pending) {
        tail.push_back(member);
      }
      else {
        *out_it++ = member;
      }
    });
  std::copy(moved.begin(), moved.end(), out_it);
  std::copy(tail.begin(), tail.end(), out_it);
  *out_it++ = GD::Ar::WriteIterator<GD::TxnWriteFile>::commit_tag();
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
  auto anchor = posname.has_value() ? fs::path(*posname).filename() : "";
  std::vector<std::string> files;
  std::copy(files_begin, files_end, std::back_inserter(files));

  for (; ar_begin != ar_end; ++ar_begin) {
    auto member = *ar_begin;
    auto found = find_name(files.begin(), files.end(), member.name());
    bool updated = false;
    bool is_anchor = (posname.has_value() && member.name() == anchor);

    if (pos == Position::before && is_anchor) {
      break;
    }
    if (found != files.end()) {
      auto file = GD::InputFile(*found);
      if (!update_newer || file.mtime() > member.mtime()) {
        *out_it++ = file;
        if (verbose) {
          std::cout << "r - " << *found << '\n';
        }
        updated = true;
      }
      files.erase(found);
      found = find_name(files.begin(), files.end(), member.name());
      while (found != files.end()) {
        std::cerr << "Warning: Skipping due to duplicate member name " << *found << '\n';
        files.erase(found);
        found = find_name(files.begin(), files.end(), member.name());
      }
    }
    if (!updated) {
      *out_it++ = member;
    }

    if (pos == Position::after && is_anchor) {
      ++ar_begin;
      break;
    }
  }

  std::vector<GD::Ar::Member> tail;
  std::copy(ar_begin, ar_end, std::back_inserter(tail));

  for (auto const& file : files) {
    auto basename = fs::path(file).filename();
    if (basename == anchor) {
      continue;
    }
    auto found = std::find_if(tail.begin(), tail.end(),
                              [basename = fs::path(file).filename()](auto const& member) {
                                return member.name() == basename;
                              });
    if (found == tail.end()) {
      auto f = GD::InputFile(file);
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
      auto file = GD::InputFile(*found);
      if (!update_newer || file.mtime() > member.mtime()) {
        *out_it++ = file;
        if (verbose) {
          std::cout << "r - " << *found << '\n';
        }
        updated = true;
      }
      files.erase(found);
      found = find_name(files.begin(), files.end(), member.name());
      while (found != files.end()) {
        std::cerr << "Warning: Skipping due to duplicate member name " << *found << '\n';
        files.erase(found);
        found = find_name(files.begin(), files.end(), member.name());
      }
    }
    if (!updated) {
      *out_it++ = member;
    }
  }

  *out_it++ = GD::Ar::WriteIterator<GD::TxnWriteFile>::commit_tag();
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
    auto file = GD::InputFile(fname);
    *out_it++ = file;
  });
  *out_it++ = GD::Ar::WriteIterator<GD::TxnWriteFile>::commit_tag();
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

  auto wf = GD::TxnWriteFile(fs::path(name), member.mode());
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
    // Use printf to ensure we sync with the write below.
    std::printf("\n<%s>\n\n", fname.c_str());
    std::fflush(stdout);
  }

  auto data = member.data();
  auto const* raw_data = data.data();
  auto count = data.size_bytes();
  while (count != 0) {
    auto res =
      ::write(STDOUT_FILENO, raw_data,
              std::min(static_cast<std::size_t>(std::numeric_limits<::ssize_t>::max()), count));
    if (res == -1) {
      if (errno != EINTR) {
        error(Msg::stdout_write_failed, std::strerror(errno));  // NOLINT(concurrency-mt-unsafe)
      }
    }
    else {
      raw_data += res;  // NOLINT(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      count -= res;
    }
  }
}

/** \brief  Convert a mode_t to a mode string.  */
auto to_mode_string(mode_t mode) -> std::string
{
  std::string result;

  result.push_back(((mode & S_IRUSR) != 0U) ? 'r' : '-');
  result.push_back(((mode & S_IWUSR) != 0U) ? 'w' : '-');
  switch (mode & (S_ISUID | S_IXUSR)) {
  case S_ISUID:
    result.push_back('s');
    break;
  case S_ISUID | S_IXUSR:
    result.push_back('S');
    break;
  case S_IXUSR:
    result.push_back('x');
    break;
  default:
    result.push_back('-');
    break;
  }
  result.push_back(((mode & S_IRGRP) != 0U) ? 'r' : '-');
  result.push_back(((mode & S_IWGRP) != 0U) ? 'w' : '-');
  switch (mode & (S_ISGID | S_IXGRP)) {
  case S_ISGID:
    result.push_back('s');
    break;
  case S_ISGID | S_IXGRP:
    result.push_back('S');
    break;
  case S_IXGRP:
    result.push_back('x');
    break;
  default:
    result.push_back('-');
    break;
  }
  result.push_back(((mode & S_IROTH) != 0U) ? 'r' : '-');
  result.push_back(((mode & S_IWOTH) != 0U) ? 'w' : '-');
  if (((mode & S_ISVTX) != 0U) && S_ISDIR(mode)) {
    result.push_back(((mode & S_IXOTH) != 0U) ? 't' : 'T');
  }
  else {
    result.push_back(((mode & S_IXOTH) != 0U) ? 'x' : '-');
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
    std::tm* mtime = std::localtime(&m);  // NOLINT(concurrency-mt-unsafe)
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
    *out_it++ = GD::Ar::WriteIterator<GD::TxnWriteFile>::commit_tag();
  }
}

/** \brief Is \a action a readonly action?  */
constexpr auto is_read_only_action(Action action) -> bool
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
  GD::Span::span<char*> files;
};

auto process_command_line(GD::Span::span<char*> args) -> State
{
  State state;
  int c = 0;
  auto set_action = [&state, &c](Action act) {
    if (state.action != Action::none) {
      error(Msg::cannot_specify_more_than_one_action, c);
    }
    state.action = act;
  };

  // NOLINTNEXTLINE(concurrency-mt-unsafe)
  while ((c = ::getopt(static_cast<int>(args.size()), args.data(), ":CTabcdimpqrstuvx:")) != -1) {
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
      state.flags = state.flags & ~Flags::message_on_creation;
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
      error(Msg::unrecognised_option, static_cast<char>(optopt));
      break;
    }
  }

  if (state.action == Action::none) {
    error(Msg::missing_action);
  }

  if (state.pos != Position::end) {
    if (static_cast<std::size_t>(optind) >= args.size()) {
      error(Msg::missing_position_name);
    }
    state.pos_file = args[optind++];
  }

  if (static_cast<std::size_t>(optind) >= args.size()) {
    error(Msg::missing_archive_name);
  }

  state.archive = fs::path(args[optind++]);
  state.files = args.subspan(optind);

  return state;
}
}  // namespace

auto main(int argc, char** argv) -> int
try {
  std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  GD::Span::span<char*> args(argv, argc);
  GD::program_name(args[0]);
  std::ios::sync_with_stdio(true);

  State state = process_command_line(args);

  std::optional<GD::InputFile> file = std::nullopt;
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
    mode = (~mode) & (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  }
  else {
    file.emplace(GD::InputFile(state.archive));
    mode = file->mode();
  }

  auto ar_begin = file.has_value() ? GD::Ar::read_archive_begin(file.value())
                                   : GD::Ar::read_archive_end<GD::InputFile>();
  auto ar_end = GD::Ar::read_archive_end<GD::InputFile>();

  switch (state.action) {
  case Action::del:
    do_delete(state.archive, mode, ar_begin, ar_end, state.files.begin(), state.files.end(),
              state.flags);
    break;
  case Action::move:
    do_move(state.archive, mode, ar_begin, ar_end, state.files.begin(), state.files.end(),
            state.pos, state.pos_file, state.flags);
    break;
  case Action::print:
    do_action(state.archive, mode, ar_begin, ar_end, state.files.begin(), state.files.end(),
              state.flags, do_print);
    break;
  case Action::quick:
    do_quick_append(state.archive, mode, ar_begin, ar_end, state.files.begin(), state.files.end(),
                    state.flags);
    break;
  case Action::replace:
    do_replace(state.archive, mode, ar_begin, ar_end, state.files.begin(), state.files.end(),
               state.pos, state.pos_file, state.flags);
    break;
  case Action::toc:
    do_action(state.archive, mode, ar_begin, ar_end, state.files.begin(), state.files.end(),
              state.flags, do_toc);
    break;
  case Action::extract:
    do_action(state.archive, mode, ar_begin, ar_end, state.files.begin(), state.files.end(),
              state.flags, do_extract);
    break;
  default:
  case Action::none:
    abort();
  }
  return EXIT_SUCCESS;
}
catch (std::exception& e) {
  error(Msg::uncaught_std_exception, e.what());
}
catch (...) {
  error(Msg::uncaught_exception);
}
