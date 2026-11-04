/** \file   bc.cc
 *  \brief  Main program for bc
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

#include "gd/stdlib.h"
#include "gd/sys/stat.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "bc-messages.hh"

#include <array>
#include <cassert>
#include <cerrno>
#include <clocale>
#include <iostream>
#include <memory>
#include <variant>

#include "find-multiply-split-point.hh"

namespace GD::Bc::Library {
auto text() -> std::string_view;
#if defined(GD_BC_PREPARSED_LIBRARY)
void load(VM& vm);
#endif
}

using Msg = GD::Bc::Msg;

namespace {
/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
#if ENABLE_EXTENSIONS
  constexpr auto usage = Msg::usage_extensions;
#else
  constexpr auto usage = Msg::usage;
#endif
  std::cerr << GD::program_name() << ": "
            << GD::Bc::Messages::get().format(GD::Bc::Set::bc, msg, args...) << '\n'
            << GD::Bc::Messages::get().format(GD::Bc::Set::bc, usage, GD::program_name()) << '\n';
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
}

enum class InputResult { eof, quit };

#if !defined(_WIN32)
// The CLI can own the sole input pipe for a complete run. Read only what the
// pipe currently supplies, so a writer need not fill a block or close before
// the parser executes an item. Terminals and explicit operands keep stdio.
class PipeReader final : public GD::Bc::Reader
{
public:
  PipeReader() : Reader("-") {}

  auto peek() -> int override
  {
    auto const bytes = contiguous();
    return bytes.empty() ? EOF : static_cast<unsigned char>(bytes.front());
  }

  auto contiguous() -> std::string_view override
  {
    if (position_ == end_ && !at_end_) {
      ssize_t count;
      do {
        count = ::read(STDIN_FILENO, bytes_.data(), bytes_.size());
      } while (count < 0 && (errno == EINTR || errno == EAGAIN));
      if (count < 0) {
        std::cerr << error(Msg::file_error, "(standard input)");
        std::exit(1);  // NOLINT(concurrency-mt-unsafe)
      }
      position_ = 0;
      end_ = static_cast<std::size_t>(count);
      at_end_ = count == 0;
    }
    return {bytes_.data() + position_, end_ - position_};
  }

  void chew_prefix(std::size_t size) override
  {
    assert(size <= end_ - position_);
    advance_location({bytes_.data() + position_, size});
    position_ += size;
  }

private:
  void do_chew() override
  {
    if (position_ != end_) {
      ++position_;
    }
  }

  std::array<char, 4096> bytes_;
  std::size_t position_{0};
  std::size_t end_{0};
  bool at_end_{false};
};

auto stdin_is_pipe() -> bool
{
  struct stat info {};
  return ::fstat(STDIN_FILENO, &info) == 0 && S_ISFIFO(info.st_mode);
}
#endif

/** Execute complete input items until EOF or read-time quit.
 * Quit discards the current item, including partial function/branch code. Output
 * from earlier items is preserved. Runtime halt is handled separately by the VM.
 */
auto execute(GD::Bc::VM& vm, std::unique_ptr<GD::Bc::Reader>&& r) -> InputResult
{
  GD::Bc::Parser parser(std::make_unique<GD::Bc::Lexer>(std::move(r)), true);

  while (true) {
    auto instructions = parser.parse();
    if (parser.seen_quit()) {
      return InputResult::quit;
    }
    if (!instructions || !vm.execute(*instructions)) {
      return InputResult::eof;
    }
  }
}
}  // namespace

auto main(int argc, char** argv) -> int
{
#if GD_BC_PROFILE
  GD::Bc::Profile::initialize_report();
#endif
  (void)std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  GD::Span::span<char*> const args(argv, argc);
  GD::program_name(args[0]);

  int c = 0;
  bool load_library = false;
  bool save_specials = false;
#if ENABLE_EXTENSIONS
  char const* opts = ":glq";
#else
  char const* opts = ":l";
#endif
  while ((c = ::getopt(argc, argv, opts)) != -1) {  // NOLINT(concurrency-mt-unsafe)
    switch (c) {
    case 'g':
      save_specials = true;
      break;
    case 'l':
      load_library = true;
      break;
    case 'q':
      break;
    case ':':
    case '?':
    default:
      error(Msg::unrecognised_option, static_cast<char>(optopt));
      break;
    }
  }

  GD::Bc::VM vm(std::cout, std::clog, save_specials, GD::Bc::VM::ExecutionChecks::verified);

  /* Configure where we do multiplication splits.  */
  GD::Bc::Number::multiply_split_point(BC_MULTIPLY_SPLIT_POINT);

  if (load_library) {
#if defined(GD_BC_PREPARSED_LIBRARY)
    GD::Bc::Library::load(vm);
#else
    auto r = std::make_unique<GD::Bc::StringReader>(GD::Bc::Library::text());
    execute(vm, std::move(r));
#endif
  }

  // EOF advances to the next input; quit must stop before opening it.
  for (auto it = args.begin() + optind; it != args.end(); ++it) {
    std::unique_ptr<GD::Bc::Reader> r;
    if (std::string_view(*it) == "-") {
      r = std::make_unique<GD::Bc::StdinReader>();
    }
    else {
      r = std::make_unique<GD::Bc::FileReader>(*it);
    }
    if (execute(vm, std::move(r)) == InputResult::quit) {
      return 0;
    }
  }
#if !defined(_WIN32)
  if (optind == argc && stdin_is_pipe()) {
    execute(vm, std::make_unique<PipeReader>());
    return 0;
  }
#endif
  auto r = std::make_unique<GD::Bc::StdinReader>();
  execute(vm, std::move(r));

  return 0;
}
