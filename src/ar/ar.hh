/** \file   ar.hh
 *  \brief  Header file for ar
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef SRC_AR_AR_HH_INCLUDED_
#define SRC_AR_AR_HH_INCLUDED_

#include "gd/filesystem.hh"

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

namespace GD::Ar {

/** \brief Enumeration of supported archive formats.  */
enum class Format {
  svr4,      ///< SVR4
  bsd,       ///< BSD,
  darwin,    ///< macOS, Darwin
  gnu,       ///< GNU
  gnu_thin,  ///< GNU "thin" archive
  win32      ///< Win32.
};

class Member;

/** \brief  Internal Namespace - API not stable. */
namespace Details {
/** \brief  Header of an archive member.  */
class MemberHeader
{
public:
  /** \brief            Constructor
   *  \param is         Input stream
   *  \param format     Format of archive.
   *  \param long_names Pointer to string table of long names (if used)
   */
  MemberHeader(std::istream& is, Format format, std::shared_ptr<Member const> long_names);

  ~MemberHeader() = default;
  MemberHeader(MemberHeader const&) = delete;
  MemberHeader(MemberHeader&&) = default;
  MemberHeader& operator=(MemberHeader const&) = delete;
  MemberHeader& operator=(MemberHeader&&) = default;

  /** Get archive member name.  */
  std::string const& name() const noexcept;

  /** Get archive member modification time (secs since 1 Jan 1970).  */
  uint64_t mtime() const noexcept;

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

private:
  /** \brief      Read a string
   *  \param  is  Stream to read from.
   *  \param  len Length of string to read.
   *  \return     Read string
   */
  std::string read_str(std::istream& is, std::size_t len);

  /** \brief       Read an unsigned integer
   *  \tparam T    Type of integer to read.
   *  \param  is   Stream to read from.
   *  \param  len  Number of digits to read.
   *  \param  def  Default value to return if string is all spaces.
   *  \return      Value read.
   *
   * It excpeted that the integer will match the following regex:
   *   " *[0-9]* *"
   * That is zero or more spaces followed by zero or more digits followed by zero or more spaces.
   */
  template<typename T, unsigned Base = 10>
  T read_uint(std::istream& is, std::size_t len, T def = 0);

  /** \brief             Update the name of the member archive
   *  \param  is         Stream to read from
   *  \param  format     Archive format.
   *  \param  long_names Member with string table of long names.
   *
   * Updates the member archive name to its full name handling however the format produces long
   * names.
   */
  void update_name(std::istream& is, Format format, std::shared_ptr<Member const> long_names);

  std::string name_;         ///< Member name
  uint64_t mtime_;           ///< Member modification time (seconds since 1 Jan 1970)
  uid_t uid_;                ///< User ID
  gid_t gid_;                ///< Group ID
  mode_t mode_;              ///< Mode
  std::size_t size_;         ///< Size of member
  std::size_t header_size_;  ///< Size of header.
};

}  // namespace Details

class Member
{
public:
  using Data = std::shared_ptr<std::vector<std::byte> const>;

  Member(Details::MemberHeader&& header, std::size_t offset, Data data);
  ~Member() = default;
  Member(Member const&) = delete;
  Member(Member&&) = delete;
  Member& operator=(Member const&) = default;
  Member& operator=(Member&&) = default;

  // Accessors
  std::string const& name() const noexcept;
  uint64_t mtime() const noexcept;
  uid_t uid() const noexcept;
  gid_t gid() const noexcept;
  mode_t mode() const noexcept;
  std::size_t offset() const noexcept;
  std::size_t size() const noexcept;
  Data data() const;

private:
  Details::MemberHeader header_;
  std::size_t offset_;
  Data data_;
};

class ReadIterator
{
public:
  enum class EndTag : int {};

  using reference = std::vector<std::byte> const&;
  using pointer = std::vector<std::byte> const*;

  bool operator==(ReadIterator const& rhs) const;
  reference operator*() const;
  pointer operator->() const;
  ReadIterator& operator++();
  void operator++(int);

private:
  friend ReadIterator read_archive(fs::path const& fname);
  ReadIterator(std::istream& is, std::shared_ptr<Member const> symbol_table,
               std::shared_ptr<Member const> long_names);
  ReadIterator(EndTag);
};

ReadIterator read_archive(fs::path const& fname);
// OutputIterator write_archive(fs::path const& fname);
}  // namespace GD::Ar

#endif  // SRC_AR_AR_HH_INCLUDED_
