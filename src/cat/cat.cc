#include "gd/bits/defines.h"
#include "gd/span.hh"
#include "gd/string.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "cat-messages.hh"

#include <clocale>
#include <cstdio>
#include <iostream>

template<typename... Ts>
void report_error(GD::Cat::Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Cat::Messages::get().format(GD::Cat::Set::cat, msg, args...) << '\n';
}

auto do_cat(std::string_view fname, bool unbuffered) -> bool
{
  GD::InputFile fp(fname);
  if (fp.error()) {
    return false;
  }
  if (unbuffered) {
    fp.setbuf();
  }

  do {
    /* Read a byte. */
    int c = fp.getc();
    if (c == EOF) {
      return !fp.error();
    }

    /* Write the byte. */
    while (fputc(c, stdout) == EOF) {
      if (errno == EINTR || errno == EAGAIN) {
        ::clearerr(stdout);
      }
      else {
        // NOLINTNEXTLINE(concurrency-mt-unsafe)
        report_error(GD::Cat::Msg::file_write_error, errno, std::strerror(errno));
        return false;
      }
    }
  } while (true);
}

auto main(int argc, char** argv) -> int
{
  std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
  GD::Std::span<char*> args(argv, argc);
  GD::program_name(args[0]);

  int c = 0;
  bool unbuffered = false;
  bool error = false;
  // NOLINTNEXTLINE(concurrency-mt-unsafe)
  while ((c = ::getopt(static_cast<int>(args.size()), args.data(), ":u")) != -1) {
    switch (c) {
    case 'u':
      unbuffered = true;
      break;
    case ':':
    case '?':
    default:
      report_error(GD::Cat::Msg::unrecognised_option, static_cast<char>(optopt));
      error = true;
      break;
    }
  }

  if (error) {
    std::cerr << GD::Cat::Messages::get().format(GD::Cat::Set::cat, GD::Cat::Msg::usage,
                                                 GD::program_name())
              << '\n';
    return EXIT_FAILURE;
  }

  if (unbuffered) {
    setvbuf(stdout, nullptr, _IONBF, 0);
  }

  bool success = GD::for_each_file(
    args.begin() + optind, args.end(),
    [unbuffered](std::string_view fname) -> bool { return do_cat(fname, unbuffered); });

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
