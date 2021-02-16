#include "gd/fcntl.h"
#include "gd/stdlib.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "compress-messages.hh"

#include <assert.h>
#include <functional>
#include <iostream>
#include <string>
#include <variant>

#include <string_view>
#include <unordered_map>

using Msg = GD::Compress::Msg;

/** \brief      Error message reporting
 *  \param msg  Message code
 *  \param args Arguments to the message.
 */
template<typename... Ts>
void error(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Compress::Messages::get().format(GD::Compress::Set::compress, msg, args...)
            << '\n';
}

/** \brief  Helper for std::visit */
template<class... Ts>
struct overloaded : Ts...
{
  using Ts::operator()...;
};
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

/** \brief  Command line parser
 *
 * \subsection Usage
 *
 * There are two stages to usage.  Firstly building the option list.  Secondly parsing.
 *
 * Building the option list is done by calling the various `add_option()` overloads, which add
 * support for flag and argument options.
 *
 * Parsing is done by calling `parse_args()` and giving `argc` and `argv` as passed into `main()`.
 * This returns either:
 *  * the index in the `argv` vector of the first operand to be processed,
 *  * `argc` if there are no operands to process.
 *  * a value greater than `argc` if there was an error.
 *
 * Errors during parsing are reported and a usage message is displayed.
 */
class CmdlineParser
{
public:
  /** \brief  Function type that parses a flag option.
   *
   * Types of OptFn must be able to be called with a `char` parameter indicating the option letter
   * given on the command line, and return a success flag.
   */
  using OptFn = std::function<bool(char)>;

  /** \brief  Function type that parses an argument option.
   *
   * Types of OptFn must be able to be called with a `char` parameter indicating the option letter
   * given on the command line, and a `char const*` parameter giving the argument.  They return a
   * success flag.
   */
  using ArgFn =
    std::function<bool(char, char const*)>; /* Function that process option with argument.  */

  /** \brief  Default constructor. */
  CmdlineParser() : args_(":")
  {
    /* We cheat and handle the 'error' characters ':' and '?' as normal options, except they don't
     * appear in the \c args_ list.
     */
    opts_.insert({':', [&](char) { return missing_argument(optopt); }});
    opts_.insert({'?', [&](char) { return invalid_option(optopt); }});
  }

  /** \brief    Add support for a flag option.
   *  \param c  Option character
   *  \param fn Function which handles processing option.
   *
   * `fn` should be able to be called as `bool fn(char c)` where the parameter is the character of
   * the flag.  The return value should be `true` for success and `false` otherwise.  If the parsing
   * fails `fn` should report any errors, but not the usage message.
   */
  void add_option(char c, OptFn fn)
  {
    auto [it, success] = opts_.insert({c, fn});
    assert(success);
    args_.push_back(c);
  }

  /** \brief    Add support for an argument option.
   *  \param c  Option character
   *  \param fn Function which handles processing option.
   *
   * `fn` should be able to be called as `bool fn(char c, char const* a)` where the first parameter
   * is the character of the flag, and the second argument is the argument for the option.  The
   * return value should be `true` for success and `false` otherwise.  If the parsing fails `fn`
   * should report any errors, but not the usage message.
   */
  void add_option(char c, ArgFn fn)
  {
    auto [it, success] = opts_.insert({c, fn});
    assert(success);
    args_.push_back(c);
    args_.push_back(':');
  }

  /** \brief      Add support for a flag option (helper - just set the flag).
   *  \param c    Option character
   *  \param flag Variable to set (by non-const reference)
   *
   * Will set \a flag to \c true whenever \a c is passed as an option on the command line.
   */
  void add_option(char c, bool& flag)
  {
    auto fn = [&flag](char) {
      flag = true;
      return true;
    };
    add_option(c, fn);
  }

  /** \brief       Parse the command line
   *  \param  argc Argument count
   *  \param  argv Argument vector
   *  \return      See below.
   *
   * The return value is one of three values:
   *
   *  * < \a argc Index of first operand after all arguments have been processed.
   *  * == \a argc No operands to process.
   *  * > \a argc An error occured.
   */
  int parse_args(int argc, char** argv) const
  {
    int c;
    bool success = true;
    while ((c = ::getopt(argc, argv, args_.data())) != -1) {
      auto it = opts_.find(c);
      assert(it != opts_.end());
      success &= std::visit(overloaded{[c](OptFn fn) { return fn(static_cast<char>(c)); },
                                       [c](ArgFn fn) { return fn(static_cast<char>(c), optarg); }},
                            it->second);
    }

    if (!success) {
      std::cerr << GD::Compress::Messages::get().format(Msg::usage, GD::program_name()) << '\n';
    }

    return success ? optind : argc + 1;
  }

private:
  /** \brief  Variant of all supported function types.  */
  using AllFns = std::variant<OptFn, ArgFn>;
  /** \brief  Map type from character to function to call.  */
  using OptMap = std::unordered_map<int, AllFns>;

  /** \brief    Handle a missing argument.
   *  \param  c Option with the missing argument.
   *  \return   \c false
   */
  bool missing_argument(char c) const
  {
    error(Msg::missing_argument, c);
    return false;
  }

  /** \brief    Handle an invalid option
   *  \param  c Invalid option.
   *  \return   \c false
   */
  bool invalid_option(char c) const
  {
    error(Msg::invalid_option, c);
    return false;
  }

  std::string args_; /**< Argument string.  */
  OptMap opts_;      /**< Option map.  */
};

/** \brief  Basic options structure. */
struct Options
{
  int bits_ = 14;                  /**< Max number of bits to output.  */
  bool use_stdout_ = false;        /**< Force use of standard output?  */
  bool verbose_ = false;           /**< Report compression ratios?  */
  bool force_compression_ = false; /**< Force compression?  */

  /** \brief      Set the bits value.
   *  \param  arg String form of value
   *  \return     Success flag.
   */
  bool set_bits(char const* arg)
  {
    char* eptr;
    errno = 0;
    auto t = ::strtol(arg, &eptr, 10);
    if (errno == ERANGE) {
      error(Msg::argument_out_of_range, 'b', arg);
      return false;
    }
    if (*eptr != '\0') {
      error(Msg::argument_not_integer, 'b', arg);
      return false;
    }
    if (t < 9 || t > 16) {
      error(Msg::bits_out_of_range, t);
      return false;
    }
    if (t > 14) {
      error(Msg::warn_bits_not_portable, t);
    }
    bits_ = t;
    return true;
  }
};

/** \brief  Write out a file consisting of bits.
 *
 * This implements the lower level file writing part of compress.
 *
 * Usage should be to call `write_bits()` to write sequences of bits, and then `commit()` or
 * `cancel()` to handle making sure everything is in the correct place.
 */
class Writer
{
public:
  /** \brief  Construct a writer that will output to standard output.  */
  explicit Writer()
      : tmp_fname_(), dest_fname_(), bytes_output_(0), file_(STDOUT_FILENO), three_bytes_(0),
        bit_offset_(0), need_confirmation_(false)
  {
    GD::make_stdout_binary();
  }

  /** \brief       Construct a writer that will output to the given filename.
   *  \param fname File name to output to
   *  \param force Set to true to force output to the file even if it already exists.
   */
  Writer(std::string_view fname, bool force)
      : tmp_fname_(), dest_fname_(fname), bytes_output_(0), file_(-1), three_bytes_(0),
        bit_offset_(0), need_confirmation_(!force)
  {
    assert(!fname.empty());

    /* We try to write directly to the destination file.  Initially give restrictive permissions so
     * that we can upgrade them later if successful.  */
    file_ = open(dest_fname_.data(), O_WRONLY | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);

    if (file_ == -1) {
      /* That didn't work - so instead we write to a temporary file, and later on move it over the
       * top of the existing file.
       */
      if (need_confirmation_ && !isatty(STDIN_FILENO)) {
        error(Msg::file_already_exists, dest_fname_);
      }

      auto [file, tmp] = GD::mkstemp(dest_fname_);
      file_ = file;
      tmp_fname_ = tmp;
      if (file_ == -1) {
        return;
      }
    }
  }

  /** \brief  Can't copy.  */
  Writer(Writer const&) = delete;
  /** \brief  Can't copy.  */
  Writer& operator=(Writer const&) = delete;

  /** \brief  Move constructor.   */
  Writer(Writer&& rhs)
      : tmp_fname_(std::move(rhs.tmp_fname_)), dest_fname_(std::move(rhs.dest_fname_)),
        bytes_output_(rhs.bytes_output_), file_(rhs.file_), three_bytes_(rhs.three_bytes_),
        bit_offset_(rhs.bit_offset_), need_confirmation_(rhs.need_confirmation_)
  {
    rhs.file_ = -1;
  }

  /** \brief  Move assignment operator.   */
  Writer& operator=(Writer&& rhs)
  {
    if (this != &rhs) {
      /* Clear any current state. */
      cancel();

      /* Copy. */
      tmp_fname_ = std::move(rhs.tmp_fname_);
      dest_fname_ = std::move(rhs.dest_fname_);
      bytes_output_ = rhs.bytes_output_;
      file_ = rhs.file_;
      three_bytes_ = rhs.three_bytes_;
      bit_offset_ = rhs.bit_offset_;
      need_confirmation_ = rhs.need_confirmation_;

      /* Disable RHS. */
      rhs.file_ = -1;
    }

    return *this;
  }

  /** \brief Destructor.  */
  ~Writer() { cancel(); }

  /** \brief  Cancel the operation.
   *
   * This will close and delete any open file.  We assume it is always successful (and ignore any
   * errors).
   */
  void cancel()
  {
    if (file_ != -1 && file_ != STDOUT_FILENO) {
      close(file_);
      file_ = -1;

      /* Delete the temporary file if we're using it otherwise delete the destination file.  */
      if (!tmp_fname_.empty()) {
        GD::unlink(tmp_fname_.data(), false);
        tmp_fname_.clear();
      }
      else if (!dest_fname_.empty()) {
        GD::unlink(dest_fname_.data(), false);
        dest_fname_.clear();
      }
    }
  }

  /** \brief       Write the lowest \a bits bits of \a code to the file.
   *  \param  code Value to write out
   *  \param  bit  Number of bits in value (<= 16)
   *  \return      Success flag.
   */
  bool write_bits(uint16_t code, unsigned int bits)
  {
    assert(bits <= 16);
    assert(code < (1 << bits));
    assert(file_ != -1);
    assert(bit_offset_ < 8);

    three_bytes_ |= (code << bit_offset_);
    bit_offset_ += bits;

    while (bit_offset_ >= 8) {
      uint8_t b = static_cast<uint8_t>(three_bytes_ & 0xff);
      if (!write_byte(b)) {
        return false;
      }
      three_bytes_ >>= 8;
      bit_offset_ -= 8;
    }

    assert(three_bytes_ < (1U << bit_offset_));
    assert(bit_offset_ < 8);
    return true;
  }

  /** \brief  Commit the changes - renaming the temporary file if necessary.
   *  \return Success flag (no need)
   */
  bool commit()
  {
    assert(file_ != -1);

    if (!flush()) {
      return false;
    }
    if (file_ != STDOUT_FILENO) {
      close(file_);
      if (!rename()) {
        return false;
      }
    }
    file_ = -1;

    return true;
  }

  /** \brief  Are we in a good state?
   *  \return True if we're good (i.e. we have an open file.)
   */
  bool good() const noexcept { return file_ != -1; }

  /** \brief  Number of bytes output so far
   *  \return Byte count - will not include any pending bits.
   */
  std::size_t bytes_output() const noexcept { return bytes_output_; }

private:
  /** \brief  Flush any remaining bits out.
   * \return Success flag.
   */
  bool flush()
  {
    if (bit_offset_ != 0) {
      assert(bit_offset_ < 8);
      uint8_t b = static_cast<uint8_t>(three_bytes_ & 0xff);
      if (!write_byte(b)) {
        return false;
      }
    }

    return true;
  }

  /** \brief  Rename the temporary file as the destination file.
   *  \return Success flag.
   *
   * This is successful if no renaming needs to be done, or if the renaming happens.
   *
   * If confirmation is asked for an not given that is not success.
   *
   * On exit the temporary file will have been deleted, whether or not the function is successful.
   */
  bool rename()
  {
    if (!tmp_fname_.empty()) {
      if (need_confirmation_) {
        if (!GD::confirm_action(GD::Compress::Messages::get().format(
              GD::Compress::Set::compress, Msg::confirm_replace, dest_fname_))) {
          GD::unlink(tmp_fname_.data());
          return false;
        }
      }

      if (!GD::rename(tmp_fname_.data(), dest_fname_.data())) {
        GD::unlink(tmp_fname_.data());
        return false;
      }
    }

    return true;
  }

  /** \brief       Write a byte
   *  \param  byte Byte to write.
   *  \return      Success flag.
   */
  bool write_byte(uint8_t byte)
  {
    size_t n = 1;
    while (n > 0) {
      ssize_t r;
      while ((r = ::write(file_, &byte, n)) == -1) {
        if (errno != EINTR && errno != EAGAIN) {
          error(Msg::file_write_error);
          return false;
        }
      }
      assert(r >= 0);
      size_t amt = static_cast<size_t>(r);
      assert(amt <= n);
      n -= amt;
      bytes_output_ += amt;
    }

    return true;
  }

  std::string tmp_fname_;    /**< Temporary file name, if empty not in use. */
  std::string dest_fname_;   /**< Destination file name, if empty using standard output. */
  std::size_t bytes_output_; /**< Number of bytes output.   */
  int file_;                 /**< Output file descriptor. */
  uint32_t three_bytes_;     /**< Current Pending three bytes of output. */
  unsigned bit_offset_;      /**< Bit offset within three_bytes_ for insertion of next code.  */
  bool need_confirmation_;   /**< Do we need to get confirmation of rename?  */
};

bool do_compress(Options const& opts, std::string_view fname)
{
  /* Map from string -> code, initialised with '\xNN' -> 'NN' for all 1-byte chars. */
  std::unordered_map<std::string, uint16_t> index;
  for (uint16_t i = 0; i < 256; ++i) {
    index.insert({std::string(1, static_cast<char>(i)), i});
  }

  GD::InputFile input(fname, "rb");
  Writer output =
    opts.use_stdout_ ? Writer() : Writer(std::string(fname) + ".Z", opts.force_compression_);

  if (!output.good()) {
    return false;
  }

  /* Header magic values.  */
  static constexpr uint16_t magic = 0x9d1f;
  static constexpr bool do_reset = true; /* true as it will save space even if we never use it. */

  /* Header.  */
  output.write_bits(magic, 16);     /* Magic idetifier. */
  output.write_bits(opts.bits_, 5); /* Max number of bits. (who would use 31?)  */
  output.write_bits(0, 2);          /* Two SBZ.  */
  output.write_bits(do_reset, 1);   /* Do we have a reset symbol?  */

  int c = input.getc();
  if (c == EOF) {
    return !input.error();
  }

  uint32_t next_code = 256 + do_reset; /* Next code to assign (reset if used will be 256). */
  int current_bits = 9;                /* Current number of bits per code. */
  size_t input_bytes = 1;              /* How many bytes read so far - c from above. */
  size_t code_count = 0;               /* How many code points we have emitted. */
  const uint32_t max_code = (1 << opts.bits_) - 1;                    /* Maximum allowed code. */
  auto current_it = index.find(std::string(1, static_cast<char>(c))); /* Current code point. */
  assert(current_it != index.end());

  while ((c = input.getc()) != EOF) {
    /* For each character. */
    char ch = static_cast<char>(c);
    ++input_bytes;

    std::string next_str = current_it->first + ch;
    auto it = index.find(next_str);
    if (it != index.end()) {
      /* Adding ch to the current string still ends up with a pre-existing code.
       */
      current_it = it;
    }
    else {
      /* String not seen before.  */

      if (next_code == ((1U << current_bits) + 1)) {
        /* We need to up the number of bits we use for each code.  This also means we need to align
         * the number of codes emitted so far to a multiple of 8. (Historic reasons).
         */
        while ((code_count & 7) != 0) {
          if (!output.write_bits(0, current_bits)) {
            return false;
          }
          ++code_count;
        }
        ++current_bits;
      }

      /* Output the current code. */
      if (!output.write_bits(current_it->second, current_bits)) {
        return false;
      }
      ++code_count;

      /* Insert the string we have currently into the index.  */
      if (next_code <= max_code) {
        index.insert({next_str, next_code++});
      }

      /* And reset the index to just the character ch. */
      current_it = index.find(std::string(1, ch));
      assert(current_it != index.end());
    }
  }

  /* Write the current code out. */
  if (!output.write_bits(current_it->second, current_bits)) {
    return false;
  }

  if (input_bytes > output.bytes_output() || opts.force_compression_ || opts.use_stdout_) {
    if (output.commit()) {
      if (opts.verbose_) {
        std::cerr << GD::Compress::Messages::get().format(
          Msg::compression_ratio, input.filename(), input_bytes, output.bytes_output(),
          (output.bytes_output() * 100.0) / input_bytes);
      }

      if (!input.is_stdin() && !opts.use_stdout_) {
        GD::unlink(input.filename(), false);
      }
      return true;
    }
  }

  return false;
}

int main(int argc, char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  Options opts;

  CmdlineParser parser;
  parser.add_option('c', opts.use_stdout_);
  parser.add_option('f', opts.force_compression_);
  parser.add_option('v', opts.verbose_);
  parser.add_option('b', [&opts](char, char const* arg) { return opts.set_bits(arg); });

  int ind = parser.parse_args(argc, argv);

  if (ind > argc) {
    return EXIT_FAILURE;
  }

  argc -= ind;
  argv += ind;

  if (opts.use_stdout_ && argc > 1) {
    error(Msg::one_file_with_c_option);
    return EXIT_FAILURE;
  }

  if (opts.use_stdout_) {
    GD::make_stdout_binary();
  }

  bool result = GD::for_each_file(
    argc, argv, [&opts](std::string_view fname) { return do_compress(opts, fname); });

  return result ? EXIT_SUCCESS : EXIT_FAILURE;
}
