/** \file   ar.hh
 *  \brief  Header file for ar
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef SRC_AR_AR_HH_INCLUDED_
#define SRC_AR_AR_HH_INCLUDED_

#include "gd/fcntl.h"
#include "gd/stdlib.h"
#include "gd/string.h"
#include "gd/sys/stat.h"
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include <assert.h>
#include <ctime>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <span>
#include <stddef.h>
#include <stdexcept>
#include <string>
#include <system_error>
#include <type_traits>
#include <vector>

/** \brief  Namespcae for archive library.
 *
 * This handles reading & writing `ar` format files.
 *
 * To read an archive do:
 *
 * \begincode
 * <IFType> file = ...; // Construct an object that provides the `IFType` concept.
 * auto it = GD::Ar::read_archive_begin(file);
 * while (it != GD::Ar::read_archive_end()) {
 *   do_something_with_archive(*it);
 *   ++it;
 * }
 * \endcode
 *
 * To write to an archive do:
 *
 * \begincode
 * <OFType> file = ...; // Construct an object that provides the `OFType` and `FileInfo` concepts.
 * auto it = GD::Ar::write_archive_begin(file);
 * for (auto f = first_file_to_write; more_to_write; f = next_file) {
 *   *it = f;
 * }
 * *it = GD::Ar::write_archive_commit();
 * \endcode
 */
namespace GD::Ar {

/** \brief Enumeration of supported archive formats.  */
enum class Format {
  svr4,      ///< SVR4
  bsd,       ///< BSD,
  darwin,    ///< macOS, Darwin
  gnu,       ///< GNU
  gnu_thin,  ///< GNU "thin" archive
  win32,     ///< Win32
};

class Member;

/** \brief  ID for a member. */
enum class MemberID : std::size_t {};

/** \brief  Type used to copy symbol names around.  */
using Symbols = std::shared_ptr<std::vector<std::string>>;

/** \brief  Type used to map MemberIDs to symbols.  */
using SymbolMap = std::map<MemberID, Symbols>;

/** \brief  Internal Namespace - API not stable. */
namespace Details {

/** \brief        Get the symbol table name for a given format.
 *  \param format Format to get symbol table name for.
 *
 * \note For SVR4 the returned name is without the trailing short name terminator (/).
 */
constexpr char const* symbol_table_name(Format format)
{
  switch (format) {
  case Format::bsd:
    return "__.SYMDEF";
  case Format::darwin:
    return "__.SYMDEF SORTED";
  case Format::gnu:
  case Format::gnu_thin:
  case Format::svr4:
  case Format::win32:
    return "";
  default:
    std::abort();
  }
}

/** \brief        Get the string table name for a given format.
 *  \param format Format to get string table name for.
 *
 * \note For SVR4 the returned name is without the trailing short name terminator (/).
 */
constexpr char const* string_table_name(Format format)
{
  switch (format) {
  case Format::bsd:
  case Format::darwin:
    return "";
  case Format::gnu:
  case Format::gnu_thin:
  case Format::svr4:
  case Format::win32:
    return "/";
  default:
    std::abort();
  }
}

/** \brief  Can the format have two symbol tables? */
constexpr bool has_two_symbol_tables(Format format) { return format == Format::win32; }

/** \brief  Does the format have the long name inline after the main header? */
constexpr bool inline_long_names(Format format)
{
  return format == GD::Ar::Format::bsd || format == GD::Ar::Format::darwin;
}

/** \brief  What is the fina; character of an out of line long name? */
constexpr char long_name_terminator(GD::Ar::Format format)
{
  switch (format) {
  case Format::bsd:
  case Format::darwin:
    assert(false);
    return std::numeric_limits<char>::max();
  case Format::svr4:
  case Format::gnu:
  case Format::gnu_thin:
    return '/';
  case Format::win32:
    return '\0';
  default:
    std::abort();
  }
}

/** \brief  What is the final character of a name that is stored inline in the name field.  */
constexpr char short_name_terminator(GD::Ar::Format format)
{
  switch (format) {
  case GD::Ar::Format::bsd:
  case GD::Ar::Format::darwin:
    return ' ';
  case GD::Ar::Format::svr4:
  case GD::Ar::Format::gnu:
  case GD::Ar::Format::gnu_thin:
  case GD::Ar::Format::win32:
    return '/';
  default:
    abort();
  }
}

/** \brief  Get the prefix that identifies a long name.  */
constexpr char const* long_name_prefix(GD::Ar::Format format)
{
  switch (format) {
  case GD::Ar::Format::bsd:
  case GD::Ar::Format::darwin:
    return "#1/";
  case GD::Ar::Format::svr4:
  case GD::Ar::Format::gnu:
  case GD::Ar::Format::gnu_thin:
  case GD::Ar::Format::win32:
    return "/";
  default:
    abort();
  }
}

/** \brief               Interpret a symbol table member into a symbol table map for the archive.
 *  \param  symbol_table Symbol table member.
 *  \return              Map of members to symbols.
 */
SymbolMap get_symbols(Member symbol_table);

/** \brief                Read the second symbol table
 *  \param  symbol_table1 the symbol map from reading the first symbol table.
 *  \param  symbol_table2 The member containing the second symbol table.
 *  \return               Updated symbol table.
 */
SymbolMap get_symbols(SymbolMap const& symbol_table1, Member symbol_table2);

/** \brief  Header of an archive member.  */
class MemberHeader
{
public:
  /** \brief             Construct header from file with known archive format
   *  \tparam FType      Class that implements IFType concept
   *  \param  file       Reference to input file
   *  \param  name       Name part of the header
   *  \param  format     Format of archive
   *  \param  long_names Ttring table of long names (if used)
   *
   * Constructor expects file to contain enough information to read whole of header from the
   * current offset.
   */
  template<typename IFType>
  MemberHeader(IFType& file, std::string const& name, Format format, Member const* long_names)
  {
    name_ = name;
    read_header(file);
    format_ = format;
    update_name(file, long_names);
  }

  /** \brief        Construct header from file with unknown archive format
   *  \tparam FType Class that implements IFType concept
   *  \param  file  Reference to input file
   *  \param  name Name part of the header.
   */
  template<typename IFType>
  MemberHeader(IFType& file, std::string const& name)
      : MemberHeader(file, name, Format::bsd, nullptr)
  {
    update_format();
  }

  ~MemberHeader() = default;
  MemberHeader(MemberHeader const&) = default;
  MemberHeader(MemberHeader&&) noexcept = default;
  MemberHeader& operator=(MemberHeader const&) = default;
  MemberHeader& operator=(MemberHeader&&) noexcept = default;

  /** Get archive member name.  */
  std::string const& name() const noexcept;

  /** Get archive member modification time (secs since 1 Jan 1970).  */
  std::time_t mtime() const noexcept;

  /** \brief  Get archive member User ID.
   *  \return Returns -1 if no user ID was set.
   */
  uid_t uid() const noexcept;

  /** \brief  Get archive member Group ID.
   *  \return Returns -1 if no group ID was set.
   */
  gid_t gid() const noexcept;

  /** \brief  Get archive member mode. */
  mode_t mode() const noexcept;

  /** \brief  Get archive member size.  */
  size_t size() const noexcept;

  /** \brief  Get header size.  */
  size_t header_size() const noexcept;

  /** \brief  Get the archive format.  */
  Format format() const noexcept;

  /** Equality comparison.  */
  bool operator==(MemberHeader const& rhs) const noexcept;

private:
  /** \brief                    Check there is no trailing non-space in a range.
   *  \tparam It                Iterator type. *it must be cv-convertible to char.
   *  \param  begin             Begin of iteration range
   *  \param  end               End of iteration range
   *  \throw std::runtime_error Thrown if we encounter non-space in the range [\a begin, \a end).
   */
  template<typename It>
  static void check_no_trailing_nonspace(It begin, It end)
  {
    if (std::find_if_not(begin, end, [](char c) { return c == ' '; }) != end) {
      throw std::runtime_error("Non-trailing space in member header.");
    }
  }

  /** \brief                    Read a number in the string \a v
   *  \tparam T                 Type of number to return
   *  \tparam Base              Base of number to read.
   *  \param v                  String of value to read.
   *  \param def                Default value to return if string is all white-space.
   *  \return                   Value read.
   *  \throw std::runtime_error Number is not in a valid format.
   *
   * Valid numbers start with zero or more space, followed by the number, terminated by zero or
   * more space.
   */
  template<typename T, char Base = 10>
  static T to_number(std::string_view v, T def = 0)
  {
    static_assert(Base <= 10);

    if (v.length() > std::numeric_limits<T>::digits) {
      throw std::logic_error("String can overflow digit type.");
    }

    /* Skip any leading whitespace. */
    auto it = std::find_if_not(v.begin(), v.end(), [](char c) { return c == ' '; });
    if (it == v.end()) {
      return def;
    }

    /* Convert to unsigned integer.  */
    T result = 0;
    it = std::find_if_not(it, v.end(), [&result](char c) {
      if (c >= '0' && c < '0' + Base) {
        result *= Base;
        result += c - '0';
        return true;
      }
      return false;
    });

    check_no_trailing_nonspace(it, v.end());
    return result;
  }

  /** \brief        Read a string from a file.
   *  \tparam FType Class that implements IFType concept
   *  \param  file  Reference to input file
   *  \param  len   Length of string to read.
   *  \return       Read string
   *  \throw        std::runtime_error If file contains less than \a len bytes to read.
   */
  template<typename IFType>
  std::string read_str(IFType& file, std::size_t len)
  {
    std::string s(len, '\0');
    file.read(std::span<char>(s.data(), len));
    header_size_ += len;
    return s;
  }

  /** \brief        Read an unsigned integer
   *  \tparam T     Type of integer to read.
   *  \tparam FType Class that implements IFType concept
   *  \tparam Base  Base input number is in (0-10).  Default 10.
   *  \param  file  Reference to input file
   *  \param  len   Number of digits to read.
   *  \param  def   Default value to return if string is all spaces.
   *  \return       Value read.
   *
   * It excpeted that the integer will match the following regex:
   *   " *[0-9]* *"
   * That is zero or more spaces followed by zero or more digits followed by zero or more spaces.
   */
  template<typename T, typename FType, unsigned Base = 10>
  T read_uint(FType& file, std::size_t len, T def = 0)
  {
    std::string v = read_str(file, len);
    return to_number<T, Base>(v, def);
  }

  /** \brief                     Read the core header.
   *  \tparam FType              Class that implements IFType concept
   *  \param  file               Reference to input file
   *  \throw  std::runtime_error Header is not valid for some reason.
   */
  template<typename FType>
  void read_header(FType& file)
  {
    header_size_ = name_len;
    mtime_ = read_uint<std::time_t>(file, mtime_len);
    uid_ = read_uint<uid_t>(file, uid_len, -1);
    gid_ = read_uint<uid_t>(file, gid_len, -1);
    mode_ = read_uint<mode_t, FType, 8>(file, mode_len);
    size_ = read_uint<std::size_t>(file, size_len);
    std::string fmag = read_str(file, fmag_len);
    if (fmag[0] != 96 || fmag[1] != 10) {
      throw std::runtime_error("Bad end of header magic");
    }
  }

  /** \brief             Update the name of the member archive
   *  \tparam FType      Class that implements IFType concept
   *  \param  file       Reference to input file
   *  \param  format     Archive format.
   *  \param  long_names Member with string table of long names, may be null.
   *
   * Updates the member archive name to its full name handling however the format produces long
   * names.
   */
  template<typename FType>
  void update_name(FType& file, Member const* long_names);

  /** \brief  Guess the value for format_ given the member header.  */
  void update_format();

public:
  static constexpr std::size_t name_len = 16;    ///< Length of name field
  static constexpr std::size_t mtime_len = 12;   ///< Length of the mtime field
  static constexpr std::size_t uid_len = 6;      ///< Length of the User-ID field
  static constexpr std::size_t gid_len = 6;      ///< Length of the group-id field
  static constexpr std::size_t mode_len = 8;     ///< Length of the mode field
  static constexpr std::size_t size_len = 10;    ///< Length of the size field.
  static constexpr std::size_t fmag_len = 2;     ///< Length of the header terminator field.
  static constexpr std::size_t header_len = 60;  ///< Length of header in table.

private:
  std::string name_;         ///< Member name
  std::time_t mtime_;        ///< Member modification time (seconds since 1 Jan 1970)
  uid_t uid_;                ///< User ID
  gid_t gid_;                ///< Group ID
  mode_t mode_;              ///< Mode
  std::size_t size_;         ///< Size of member
  std::size_t header_size_;  ///< Size of header.
  Format format_;            ///< Header format.
};

/** \brief Inequality operator.  */
bool operator!=(MemberHeader const& lhs, MemberHeader const& rhs) noexcept;
}  // namespace Details

/** \brief Archive member.
 *
 * Implements IFType concept.  Can be treated as an input file.
 */
class Member
{
public:
  /** \brief Data type we store - used to save typing.  */
  using Data = std::shared_ptr<std::vector<std::byte> const>;
  /** \brief         Constructor.
   *  \param header  Header
   *  \param id      ID of member
   *  \param data    Archive data.
   *  \param symbols Symbols in the symbol map.
   */
  Member(Details::MemberHeader&& header, MemberID id, Data data, Symbols symbols);

  ~Member() = default;
  Member(Member const&) = default;
  Member(Member&&) = default;
  Member& operator=(Member const&) = default;
  Member& operator=(Member&&) = default;

  /** \brief Member file name.  */
  std::string const& name() const noexcept;

  /** \brief Member modification time.  */
  std::time_t mtime() const noexcept;

  /** \brief Member User ID.  */
  uid_t uid() const noexcept;

  /** \brief Member Group ID.  */
  gid_t gid() const noexcept;

  /** \brief Member mode.  */
  mode_t mode() const noexcept;

  /** \brief Member ID.  */
  MemberID id() const noexcept;

  /** \brief Format.  */
  Format format() const noexcept;

  /** \brief Equality comparison.  */
  bool operator==(Member const& rhs) const noexcept;

  /** \brief Object is seekable.  */
  static constexpr bool seekable = true;

  /** \brief Do we definitely know the exact size of the file?  */
  static constexpr bool size_fixed = true;

  /** \brief Span containing all the data in the member.  */
  std::span<std::byte const> data() const noexcept;

  /** \brief  Get the symbols.  */
  Symbols symbols() const noexcept;

  /** \brief Size of member header in bytes.  */
  std::size_t size_bytes() const noexcept;

  /** \brief  Current offset in bytes.  */
  std::size_t offset_bytes() const noexcept;

  /** \brief Set the current offset
   *  \param offset new offset
   *  \throw std::runtime_error \a offset >= size_bytes().
   */
  void offset_bytes(std::size_t offset);

  /** \brief  Have we reached the end of the file?  */
  bool eof() const noexcept { return offset_ == data_->size(); };

  /** \brief Read into \a dest.  */
  template<typename T, std::size_t Count>
  void read(std::span<T, Count> dest)
  {
    read_at(dest, offset_);
    offset_ += dest.size_bytes();
  }

  /** \brief Read from offset \a offset into \a dest. */
  template<typename T, std::size_t Count>
  void read_at(std::span<T, Count> dest, std::size_t offset) const
  {
    if (size_bytes() < offset) {
      throw std::runtime_error("Offset not within member data.");
    }
    if (size_bytes() - offset < dest.size_bytes()) {
      throw std::runtime_error("Trying to read too many bytes");
    }
    memcpy(dest.data(), data_->data() + offset, dest.size_bytes());
  }

  /** \brief  Read as many bytes as possible into \a dest.
   *  \return Number of bytes read.
   */
  template<typename T, std::size_t Count>
  std::size_t read_upto(std::span<T, Count> dest)
  {
    auto count = std::min(dest.size_bytes(), data_->size() - offset_);
    memcpy(dest.data(), data_->data() + offset_, count);
    offset_ += count;
    return count;
  }

private:
  Details::MemberHeader header_;  ///< Header
  MemberID id_;                   ///< ID
  std::size_t offset_;            ///< Current offset
  Data data_;                     ///< Data.
  Symbols symbols_;               ///< Symbols.
};

bool operator!=(Member const& lhs, Member const& rhs) noexcept;

/** \brief        Forward iterator used to iterate through reading an archive
 *  \tparam FType underlying file type being read from.
 *
 * Use read_archive_begin() and read_archive_end() to get appropriate begin and end iterators.
 */
template<typename FType>
class ReadIterator
{
public:
  using iterator_category = std::input_iterator_tag;  ///< Iterator category.
  using value_type = Member const;                    ///< Member type.
  using reference = Member const&;                    ///< Reference to iterator
  using pointer = Member const*;                      ///< Pointer to iterator.
  using difference_type = std::ptrdiff_t;             ///< Difference type.

  ~ReadIterator() = default;
  ReadIterator(ReadIterator&& rhs) = default;
  ReadIterator& operator=(ReadIterator&& rhs) noexcept = default;
  ReadIterator(ReadIterator const&) = default;
  ReadIterator& operator=(ReadIterator const&) noexcept = default;

  Format format() const { return state_->format_; }

  /** \brief  Equality comparison operator.  */
  bool operator==(ReadIterator const& rhs) const
  {
    if (member_.has_value()) {
      return rhs.member_.has_value() && *member_ == *rhs.member_;
    }
    return !rhs.member_.has_value();
  }

  /** \brief Pointer dereference.  */
  reference operator*() const
  {
    assert(member_ != std::nullopt);
    return *member_;
  }

  /** \brief  Member access. */
  pointer operator->() const
  {
    assert(member_ != std::nullopt);
    return &member_.value();
  }

  /** \brief Pre-increment.  */
  ReadIterator& operator++()
  {
    read_next_member();
    return *this;
  }

  /** \brief Post-increment.  */
  ReadIterator operator++(int)
  {
    auto temp = *this;
    ++(*this);
    return temp;
  }

private:
  /** \brief Tag used to indicate we want to construct end.  */
  enum class EndTag : int {};

  template<typename FType1>
  friend ReadIterator<typename std::remove_reference<FType1>::type> read_archive_begin(FType1&& f);

  template<typename FType1>
  friend ReadIterator<FType1> read_archive_end();

  /** \brief                Construct iterator begin element.
   *  \param  file          File to read from
   *  \param  offset        Current offset in the archive
   *  \param  format        Format of the archive.
   *  \param  first_member  Pointer to first normal member of the archive
   *  \param  symbol_table1 Pointer to first symbol table
   *  \param  symbol_table2 Pointer to second symbol table
   *  \param  string_table  Pointer to string table.
   *
   * All of \a first_member, \a symbol_table1, \a symbol_table2, and \a string_table may be null.
   */
  ReadIterator(FType&& file, std::size_t offset, Format format,
               std::optional<Member const> first_member, SymbolMap&& symbol_map,
               std::optional<Member const> string_table)
      : state_(std::make_shared<State>(std::move(file), offset, format, std::move(symbol_map),
                                       string_table)),
        member_(first_member)
  {
    if (member_ == std::nullopt) {
      read_next_member();
    }
  }

  /** \brief  Construct the end tag.  */
  explicit ReadIterator(EndTag) : state_(nullptr), member_(std::nullopt) {}

  /** \brief Read the next member from the file. */
  void read_next_member()
  {
    if (state_->file_.eof()) {
      member_.reset();
      state_ = nullptr;
      return;
    }

    /* Ensure we're aligned before we try to read.  */
    if (state_->offset_ & 1) {
      char c;
      state_->file_.read(std::span(&c, 1));
      ++state_->offset_;
    }

    auto id = MemberID(state_->offset_);
    std::string name(Details::MemberHeader::name_len, '\0');
    auto len = state_->file_.read_upto(std::span<char>(name));
    if (len == 0) {
      member_.reset();
      state_ = nullptr;
      return;
    }
    else if (len < Details::MemberHeader::name_len) {
      throw std::runtime_error("Incorrect header.");
    }
    auto hdr = Details::MemberHeader(
      state_->file_, name, state_->format_,
      state_->string_table_.has_value() ? &state_->string_table_.value() : nullptr);
    auto data = std::make_shared<std::vector<std::byte>>(hdr.size());
    state_->file_.read(std::span<std::byte>(*data));
    state_->offset_ += hdr.header_size() + hdr.size();
    auto it = state_->symbol_map_.find(id);
    auto syms = it != state_->symbol_map_.end() ? it->second : nullptr;
    member_.emplace(Member(std::move(hdr), id, data, syms));
  }

  /** \brief  State structure. */
  struct State
  {
    State(FType&& file, std::size_t offset, Format format, SymbolMap&& symbol_map,
          std::optional<Member const> string_table)
        : file_(std::move(file)), offset_(offset), format_(format), symbol_map_(symbol_map),
          string_table_(string_table)
    {
    }
    ~State() = default;
    State(State const&) = delete;
    State(State&&) noexcept = delete;
    State& operator=(State const&) = delete;
    State& operator=(State&&) noexcept = delete;

    FType file_;
    std::size_t offset_;
    Format format_;
    SymbolMap symbol_map_;
    std::optional<Member const> string_table_;
  };

  std::shared_ptr<State> state_;
  std::optional<Member const> member_;
};

template<typename FType>
bool operator!=(ReadIterator<FType> const& lhs, ReadIterator<FType> const& rhs)
{
  return !(lhs == rhs);
}

/** \brief  Output iterator to write to an archive.
 *  \tparam OFType File type to write to.
 *
 * \subsection Usage
 *
 * General usage looks like:
 *
 * \code
 * WriteFile archive("archive.a");
 * auto it = GD::Ar::archive_inserter(std::move(archive), format, mode);
 * *it++ = member; // Write members from another iterator
 * *it++ = file; // Write files into archive.
 * // Write more things as approprate.
 * // Commit changes at the end:
 * *it++ = it.commit_tag();
 * \endcode
 *
 * Not commiting changes will mean nothing gets written out to the file.
 */
template<typename OFType>
class WriteIterator
{
public:
  using iterator_category = std::output_iterator_tag;  ///< Iterator category.
  using value_type = void;                             ///< Member type.
  using reference = void;                              ///< Reference to iterator
  using pointer = void;                                ///< Pointer to iterator.
  using difference_type = void;                        ///< Difference type.

  /** \brief Tag ot use to say we've finished writing to the output iterator.  */
  enum class CommitTag : int {};

  /** Proxy class.  This is what gets returned by *it.  */
  class Proxy
  {
  public:
    /** \brief    Constructor
     *  \param wi Write iterator we're proxying.
     */
    explicit Proxy(WriteIterator& wi) : wi_(wi) {}
    ~Proxy() = default;
    Proxy(Proxy const&) = delete;
    Proxy(Proxy&&) noexcept = delete;
    Proxy& operator=(Proxy const&) = delete;
    Proxy& operator=(Proxy&&) noexcept = delete;

    /** \brief      Write a member from another archive into this archive.
     *  \param  obj Object to write.
     *  \return     \c *this
     */
    Proxy& operator=(Member const& obj)
    {
      if (obj.symbols() != nullptr) {
        wi_.add_symbols(obj.symbols());
      }
      wi_.write_member(obj,
                       [&obj](auto it) { std::copy(obj.data().begin(), obj.data().end(), it); });
      return *this;
    }

    /** \brief      Write a file into this archive.
     *  \param  obj Object to write.
     *  \return     \c *this
     */
    template<typename IFType>
    Proxy& operator=(IFType& obj)
    {
      wi_.write_member(obj, [&obj](auto it) {
        std::vector<std::byte> data(4096);
        while (!obj.eof()) {
          auto amount = obj.read_upto(std::span(data));
          std::copy(data.begin(), data.begin() + amount, it);
        }
      });
      return *this;
    }

    /** \brief  Commit this archive.
     *  \return \c *this.
     */
    Proxy& operator=(WriteIterator::CommitTag)
    {
      wi_.commit();
      return *this;
    }

  private:
    WriteIterator& wi_;  ///< Iterator we're writing to.
  };

  /** \brief        Constructur
   *  \param file   File to write to
   *  \param format Archive format to use.
   */
  WriteIterator(OFType&& file, Format format)
      : state_(std::make_shared<State>(std::move(file), format))
  {
  }

  ~WriteIterator() = default;
  WriteIterator(WriteIterator const&) = default;
  WriteIterator(WriteIterator&&) noexcept = default;
  WriteIterator& operator=(WriteIterator const&) = default;
  WriteIterator& operator=(WriteIterator&&) noexcept = default;

  /** \brief Dereference iterator. */
  Proxy operator*() { return Proxy(*this); }

  /** \brief Move forward.  */
  WriteIterator operator++(int) { return *this; }

  /** \brief Move forward.  */
  WriteIterator& operator++() { return *this; }

  /** \brief  Get the commit tag.  */
  static WriteIterator::CommitTag commit_tag() { return CommitTag(); }

private:
  friend class Proxy;

  /** \brief  Get the current offset.  */
  std::size_t offset() const noexcept { return state_->data_.size(); }

  /** \brief      Write \a span into \a out.
   *  \param out  Output object to store into.
   *  \param span Data to write.
   *
   * \a out should implement size() and push_back() methods.
   */
  template<typename Store, typename T, std::size_t E>
  void add_data(Store& out, std::span<T, E> span)
  {
    auto raw_span = std::as_bytes(span);
    out.reserve(out.size() + raw_span.size_bytes());
    std::copy(raw_span.begin(), raw_span.end(), std::back_inserter(out));
  }

  /** \brief     Write a string into an output object.
   *  \param out Output store.
   *  \param val String to write.
   *  \param len How long output should be.
   *
   * Output is padded with spaces to reach \a len, or truncated.
   */
  template<typename Store>
  void add_str(Store& out, std::string const& val, std::size_t len)
  {
    std::string output = val + std::string(len, ' ');
    add_data(out, std::span(output.begin(), output.begin() + len));
  }

  /** \brief  Write a number at a particular offset.
   *  \tparam It    Iterator
   *  \tparam T     Type of number
   *  \tparam Base  Output base.
   *  \param  where Where to insert number
   *  \param  val   Value to output
   *  \param  len   Number of characters to output.
   *
   * Output string to right padded with ' '.
   */
  template<typename It, typename T, unsigned Base = 10>
  void add_number_at(It where, T val, std::size_t len)
  {
    auto res = std::string();
    while (val != 0) {
      res += std::string(1, '0' + (val % Base));
      val /= Base;
    }
    std::reverse(res.begin(), res.end());
    if (res.empty()) {
      res = "0";
    }
    if (res.size() > len) {
      throw std::runtime_error("Number too big for output.");
    }
    res += std::string(len, ' ');
    std::transform(res.begin(), res.begin() + len, where, [](char c) { return std::byte(c); });
  }

  /** \brief        Write a number at the end of a store
   *  \tparam Store Output store type
   *  \tparam T     Type of number
   *  \tparam Base  Output base.
   *  \param  out   Output.
   *  \param  val   Value to output
   *  \param  len   Number of characters to output.
   *
   * Output string to right padded with ' '.
   */
  template<typename Store, typename T, unsigned Base = 10>
  void add_number(Store& out, T val, std::size_t len)
  {
    auto it = std::back_inserter(out);
    add_number_at<decltype(it), T, Base>(it, val, len);
  }

  /** \brief       Write an archive members name.
   *  \param  out  Output to write to
   *  \param  name Member name
   *  \param  len  Length of buffer to write to.
   *  \return      Value to write at end of header if name is too long (may be empty).
   */
  template<typename Store>
  std::string add_name(Store& out, std::string const& name, std::size_t len)
  {
    if (Details::inline_long_names(state_->format_) &&
        (name.size() > len || name.find(' ') != std::string::npos)) {
      /* This is an inline long name: Either name is too long or contains a space. */
      auto prefix = std::string(Details::long_name_prefix(state_->format_));
      add_str(out, prefix, prefix.size());
      auto name_len = name.size();
      add_number(out, name_len, len - prefix.size());
      return name;
    }

    if (!Details::inline_long_names(state_->format_) && name.size() > len - 1) {
      /* This is an out of line long name.  */
      auto prefix = std::string(Details::long_name_prefix(state_->format_));
      add_str(out, prefix, prefix.size());
      auto name_offset = state_->string_table_.size();
      std::transform(name.begin(), name.end(), std::back_inserter(state_->string_table_),
                     [](char c) { return static_cast<std::byte>(c); });
      state_->string_table_.push_back(
        static_cast<std::byte>(Details::long_name_terminator(state_->format_)));
      add_number(out, name_offset, len - prefix.size());
      return std::string();
    }

    /* Inline name.  */
    auto out_name = name + Details::short_name_terminator(state_->format_);
    add_str(out, out_name, len);
    return std::string();
  }

  /** \brief     Output a User or Group ID.
   *  \param out Store to output to.
   *  \param id  ID
   *  \param len Length.
   *
   * If \a if is \c ~T(0) then all spaces are output.
   */
  template<typename T, typename Store>
  void add_ugid(Store& out, T id, std::size_t len)
  {
    if (id == ~T(0)) {
      add_str(out, "", len);
    }
    else {
      add_number(out, id, len);
    }
  }

  /** \brief     Output file mode
   *  \param out Store to output to.
   *  \param id  mode
   *  \param len Length.
   *
   * If \a if is \c ~T(0) then all spaces are output.
   */
  template<typename T>
  void add_mode(T& out, mode_t mode, std::size_t len)
  {
    add_number<T, mode_t, 8>(out, mode & 07777, len);
  }

  /** \brief   Add any necessary padding.
   *  \param out Store to output to.
   */
  template<typename T>
  void add_padding(T& out)
  {
    if (offset() & 1) {
      out.push_back(std::byte('\n'));
    }
  }

  /** \brief  Commit the archive.
   *
   * This does symbol map and string table generation.
   */
  void commit()
  {
    static std::string header("!<arch>\n");
    state_->file_.write(std::span(header));
    std::vector<std::byte> string_table_data;
    /* Generate the string table.  We need to know how long it is here so that we can put the
     * correct offsets in the symbol table IDs.
     */
    if (!state_->string_table_.empty()) {
      write_member(
        string_table_data, SpecialMemberHeader::string_table(state_->format_),
        [data = state_->string_table_](auto it) { std::copy(data.begin(), data.end(), it); });
    }

    std::vector<std::byte> symbol_table_data;
    if (!state_->symbol_map_.empty()) {
      write_member(symbol_table_data, SpecialMemberHeader::symbol_table(state_->format_),
                   [&](auto it) {
                     write_symbol_map(it, state_->symbol_map_,
                                      header.size() + Details::MemberHeader::header_len +
                                        string_table_data.size());
                   });
    }
    /* Write the file. */
    state_->file_.write(std::span(symbol_table_data));
    state_->file_.write(std::span(string_table_data));
    state_->file_.write(std::span(state_->data_));
    state_->file_.commit();
    /* And don't allow any further.  */
    state_ = nullptr;
  }

  template<typename It>
  static void write_symbol_map(It inserter, SymbolMap const& symbol_map,
                               std::uint32_t offset_addend)
  {
    auto [count, string_size] =
      std::accumulate(symbol_map.begin(), symbol_map.end(),
                      std::make_pair(std::uint32_t{0}, std::uint32_t{0}), [](auto acc, auto value) {
                        acc.first += (value.second != nullptr) ? value.second->size() : 0;
                        acc.second = std::accumulate(
                          value.second->begin(), value.second->end(), acc.second,
                          [](auto acc2, auto value2) { return acc2 + value2.size() + 1; });
                        return acc;
                      });

    if (count > std::numeric_limits<std::uint32_t>::max()) {
      throw std::runtime_error("Too many symbols.");
    }

    offset_addend += 4 + count * 4 + string_size;
    if (offset_addend & 1) {
      ++offset_addend;
    }

    write_be(inserter, count);

    std::for_each(symbol_map.begin(), symbol_map.end(), [&inserter, offset_addend](auto it) {
      if (it.second == nullptr) {
        return;
      }
      std::uint32_t offset = static_cast<std::uint32_t>(it.first) + offset_addend;
      for (std::size_t i = 0; i < it.second->size(); ++i) {
        write_be(inserter, offset);
      }
    });
    std::for_each(symbol_map.begin(), symbol_map.end(), [&inserter](auto it) {
      if (it.second == nullptr) {
        return;
      }
      std::for_each(it.second->begin(), it.second->end(), [&inserter](auto it2) {
        std::transform(it2.begin(), it2.end(), inserter,
                       [](char c) { return static_cast<std::byte>(c); });
        *inserter++ = std::byte{0};
      });
    });

    /* Add a trailing NUL to pad to even number of bytes if necessary.  */
    if (string_size & 1) {
      *inserter++ = std::byte{0};
    }
  }

  struct SpecialMemberHeader
  {
    static SpecialMemberHeader string_table(Format format)
    {
      return SpecialMemberHeader(Details::string_table_name(format));
    }
    static SpecialMemberHeader symbol_table(Format format)
    {
      return SpecialMemberHeader(Details::symbol_table_name(format));
    }

    std::string name() const { return name_; }
    time_t mtime() const { return 0; }
    mode_t mode() const { return 0; }
    uid_t uid() const { return 0; }
    gid_t gid() const { return 0; }

  private:
    explicit SpecialMemberHeader(std::string const& name) : name_(name) {}
    std::string name_;
  };

  void add_symbols(Symbols symbols)
  {
    auto id = static_cast<MemberID>(offset());
    [[maybe_unused]] auto [it, success] = state_->symbol_map_.insert(std::make_pair(id, symbols));
    assert(success);
  }

  template<typename T, typename DataWriter, typename Store>
  void write_member(Store& out, T const& obj, DataWriter dw)
  {
    auto long_name =
      add_name(out, fs::path{obj.name()}.filename(), Details::MemberHeader::name_len);
    add_number(out, obj.mtime(), Details::MemberHeader::mtime_len);
    add_ugid(out, obj.uid(), Details::MemberHeader::uid_len);
    add_ugid(out, obj.gid(), Details::MemberHeader::gid_len);
    add_mode(out, obj.mode(), Details::MemberHeader::mode_len);
    auto size_offset = out.size();
    add_str(out, "", Details::MemberHeader::size_len);
    add_str(out, "`\n", Details::MemberHeader::fmag_len);
    auto data_begin_offset = out.size();
    add_data(out, std::span(long_name));
    dw(std::back_inserter(out));
    auto data_end_offset = out.size();
    add_number_at(out.begin() + size_offset, data_end_offset - data_begin_offset,
                  Details::MemberHeader::size_len);
    add_padding(out);
  }

  template<typename T, typename DataWriter>
  void write_member(T const& obj, DataWriter dw)
  {
    write_member(state_->data_, obj, dw);
  }

  struct State
  {
    State(OFType&& file, Format format) : file_(std::move(file)), format_(format) {}

    ~State() = default;
    State(State const&) = delete;
    State(State&&) = delete;
    State& operator=(State const&) = delete;
    State& operator=(State&&) = delete;

    OFType file_;
    Format format_;
    std::vector<std::byte> string_table_;
    std::vector<std::byte> data_;
    SymbolMap symbol_map_;
  };

  std::shared_ptr<State> state_;
};

/** \brief  Return the end of the archive reading iteration.  */
template<typename FType>
ReadIterator<FType> read_archive_end()
{
  return ReadIterator<FType>(typename ReadIterator<FType>::EndTag());
}

/** \brief  Start reading through an archive file.  */
template<typename FType1>
ReadIterator<typename std::remove_reference<FType1>::type> read_archive_begin(FType1&& f)
{
  using FType = typename std::remove_reference<FType1>::type;
  FType file(std::move(f));
  constexpr std::size_t magic_len = 8;
  constexpr char const* expected_magic = "!<arch>\n";
  std::string magic(magic_len, '\0');
  file.read(std::span<char>(magic));
  if (magic != expected_magic) {
    throw std::runtime_error("Missing archive magic");
  }
  std::size_t offset = magic_len;

  std::optional<Member const> long_names = std::nullopt;
  SymbolMap symbol_map;

  /* Read first - header need to guess the format type.  */
  if (file.eof()) {
    return read_archive_end<FType>();
  }

  std::string name(Details::MemberHeader::name_len, '\0');
  [[maybe_unused]] auto len = file.read_upto(std::span<char>(name));
  if (file.eof()) {
    return read_archive_end<FType>();
  }
  assert(len == 16);
  auto hdr = Details::MemberHeader(file, name);
  Format format = hdr.format();
  auto id = MemberID(offset);
  auto data = std::make_shared<std::vector<std::byte>>(hdr.size());
  file.read(std::span<std::byte>(*data));
  offset += hdr.header_size() + hdr.size();
  auto member = std::make_optional<Member const>(std::move(hdr), id, data, nullptr);

  auto read_next_member = [&offset, &file, &member, &symbol_map, format]() {
    if (offset & 1) {
      char c;
      offset += file.read_upto(std::span(&c, 1));
    }

    if (file.eof()) {
      member.reset();
      return;
    }

    auto id = MemberID(offset);
    std::string name(Details::MemberHeader::name_len, '\0');
    [[maybe_unused]] auto len = file.read_upto(std::span<char>(name));
    if (file.eof()) {
      member.reset();
      return;
    }
    assert(len == 16);
    auto hdr = Details::MemberHeader(file, name, format, nullptr);
    auto data = std::make_shared<std::vector<std::byte>>();
    data->resize(hdr.size());
    file.read(std::span<std::byte>(*data));
    offset += hdr.header_size() + hdr.size();
    auto it = symbol_map.find(id);
    auto symbols = it != symbol_map.end() ? it->second : nullptr;
    member.emplace(std::move(hdr), id, data, symbols);
  };

  /* Handle the symbol table(s) if present.  */
  if (member->name() == Details::symbol_table_name(format)) {
    symbol_map = Details::get_symbols(member.value());
    read_next_member();
    if (member.has_value() && member->name() == Details::symbol_table_name(format)) {
      /* If we have two headers we are Win32 format.  */
      format = Format::win32;
      symbol_map = Details::get_symbols(symbol_map, member.value());
      read_next_member();
    }
  }
  if (!member.has_value()) {
    return read_archive_end<FType>();
  }

  /* Handle string table.  */
  if (!Details::inline_long_names(format) && member->name() == Details::string_table_name(format)) {
    long_names.emplace(std::move(member.value()));
    member.reset();
  }

  return ReadIterator(std::move(file), offset, format, member, std::move(symbol_map), long_names);
}

inline WriteIterator<TxnWriteFile> archive_inserter(fs::path const& fname, Format format,
                                                    mode_t mode = 0644)
{
  TxnWriteFile wf(fname, mode);
  return WriteIterator(std::move(wf), format);
}

}  // namespace GD::Ar

template<typename FType>
void GD::Ar::Details::MemberHeader::update_name(FType& file, GD::Ar::Member const* long_names)
{
  /* Some names may not obey the rules about spaces.  Leave them as is.  */
  if (name_ == symbol_table_name(format_)) {
    return;
  }

  if (Details::inline_long_names(format_)) {
    /* Long name is represented by #1/<Len> in name field with name immediately following header.
     */
    if (name_.substr(0, 3) == "#1/") {
      auto length = to_number<std::size_t>(name_.substr(3), std::string::npos);
      if (length == std::string::npos) {
        throw std::runtime_error("Bad string length");
      }
      if (length > size_) {
        throw std::runtime_error("Long name longer than data");
      }
      name_ = read_str(file, length);
      size_ -= length;
      auto nul_pos = name_.find('\0');
      if (nul_pos != std::string::npos) {
        name_.erase(name_.find('\0'), name_.size());
      }
      return;
    }
  }
  else {
    /* Out of line long names: Represented by /<offset> in name field.  With name at <offset> in
     * long names field.  Terminated by the long_name_terminator character.  */
    if (name_[0] == '/') {
      if (name_[1] == ' ' || (name_[1] == '/' && name_[2] == ' ')) {
        name_.erase(name_.find(' '), name_.size());
        return;
      }
      auto offset = to_number<std::size_t>(name_.substr(1), std::string::npos);
      if (offset == std::string::npos) {
        throw std::runtime_error("Bad string offset into long names");
      }

      if (long_names == nullptr) {
        throw std::runtime_error("No long name archive found.");
      }
      auto data = long_names->data();
      if (data.size() < offset) {
        throw std::runtime_error("Offset out of range for long names.");
      }
      auto it = data.begin() + offset;
      auto it_end = std::find(it, data.end(), std::byte(long_name_terminator(format_)));
      if (it_end == data.end()) {
        throw std::runtime_error("Unterminated string in long names table.");
      }
      name_.resize(it_end - it);
      long_names->read_at(std::span<char>(name_), offset);
      return;
    }
  }

  /* Short names.  */
  auto it = std::find(name_.begin(), name_.end(), short_name_terminator(format_));
  if (it != name_.end()) {
    check_no_trailing_nonspace(it + 1, name_.end());
  }
  name_.erase(it, name_.end());
  return;
}

#endif  // SRC_AR_AR_HH_INCLUDED_
