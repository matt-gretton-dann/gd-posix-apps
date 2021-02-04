#include "gd/string.h"
#include "gd/unistd.h"
#include "message-data.hh"
#include "util/file.hh"
#include "util/utils.hh"

#include <iostream>
#include <stdio.h>

template<typename... Ts>
void report_error(GD::Cat::Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Cat::Messages::get().format(GD::Cat::Set::cat, msg, args...) << '\n';
}

class InputFile
{
public:
  InputFile(std::string_view filename, bool unbuffered) : filename_(filename), is_stdin_(false)
  {
    if (filename_ == "-") {
      filename_ = GD::Cat::Messages::get().get(GD::Cat::Set::cat, GD::Cat::Msg::stdin_name);
      is_stdin_ = true;
      file_ = stdin;
    }
    else {
      file_ = ::fopen(filename_.data(), "r");
      if (file_ == nullptr) {
        report_error(GD::Cat::Msg::file_open_error, filename_, errno, ::strerror(errno));
        return;
      }
    }
    if (unbuffered) {
      setvbuf(file_, nullptr, _IONBF, 0);
    }
  }

  FILE* handle() { return file_; }
  std::string_view filename() const { return filename_; }

private:
  std::string filename_;
  FILE* file_;
  bool is_stdin_;
};

bool do_cat(std::string_view fname, bool unbuffered)
{
  InputFile fp(fname, unbuffered);
  if (fp.handle() == nullptr) {
    return false;
  }

  do {
    /* Read a byte. */
    int c = fgetc(fp.handle());
    if (c == EOF) {
      if (feof(fp.handle())) {
        return true;
      }
      else {
        assert(ferror(fp.handle()));
        if (errno == EINTR || errno == EAGAIN) {
          ::clearerr(fp.handle());
          continue;
        }
        else {
          report_error(GD::Cat::Msg::file_read_error, fp.filename(), errno, ::strerror(errno));
          return false;
        }
      }
    }

    /* Write the byte. */
    while (fputc(c, stdout) == EOF) {
      if (errno == EINTR || errno == EAGAIN) {
        ::clearerr(stdout);
      }
      else {
        report_error(GD::Cat::Msg::file_write_error, errno, ::strerror(errno));
        return false;
      }
    }
  } while (true);
}
int main(int argc, char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  int c;
  bool unbuffered = false;
  bool error = false;
  while ((c = ::getopt(argc, argv, ":u")) != -1) {
    switch (c) {
    case 'u':
      unbuffered = true;
      break;
    case ':':
    case '?':
    default:
      report_error(GD::Cat::Msg::unrecognised_option, (char)optopt);
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

  bool success =
    GD::for_each_file(argc - optind, argv + optind, [unbuffered](std::string_view fname) -> bool {
      return do_cat(fname, unbuffered);
    });

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
