/** \file   ar.hh
 *  \brief  Header file for ar
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef SRC_AR_FILES_HH_INCLUDED_
#define SRC_AR_FILES_HH_INCLUDED_

#include "gd/fcntl.h"
#include "gd/filesystem.hh"
#include "gd/stdlib.h"
#include "gd/string.h"
#include "gd/sys/stat.h"
#include "gd/unistd.h"

#include <assert.h>
#include <span>
#include <stddef.h>
#include <stdexcept>
#include <string>
#include <system_error>
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
 *
 * \subsection Concepts Concepts
 *
 * \subsubsection ConceptIFType IFType Concept
 *
 * The IFType Concept provides file reading functions.  Types implementing the concept need to
 * implement the following member variables and functions:
 *
 * // Can we change the offset of the file?
 * static constexpr bool seekable;
 *
 * // Is the size always known?
 * static constexpr bool fixed_size;
 *
 * // Return the number of bytes in a file.  If the size is unknown return size_t(-1)
 * std::size_t size_bytes() const;
 *
 * // Current offset in the file.
 * std::size_t offset_bytes(std::enable_if_t<seekable> = 0) const;
 *
 * // Set the current offset in the file.  Only required if the file is seekable.
 * void offset_bytes(std::size_t offset, std::enable_if_t<seekable> = 0);
 *
 * // Have we reached end of file?
 * bool eof() const;
 *
 * // Read the next s.size_bytes() starting at offset_bytes() into the given span.  An exception is
 * // to be raised if there are not enough bytes to fill the span.  Updates offset_bytes() by
 * // s.size_bytes() bytes.
 * template<typename T, std::size_t E>
 * void read(std::span<T, E> s);
 *
 *  // Read s.size_bytes() from offset into the span s.  An exception is to be raised if there are
 *  // not enough bytes to fille the span or offset is after the end of the file.  Should not affect
 *  // the result of offset_bytes().
 * template<typename T, std::size_t E>
 * void read_at(std::span<T, E> s, std::size_t offset, std::enable_if_t<seekable> = 0);
 *
 * // Read up to s.size_bytes() from offset_bytes() into the given span.  Returns the number of
 * // bytes actually read.  Updates offset_bytes() by the number of bytes read.
 * template<typename T, std::size_t E>
 * std::size_t read_upto(std::span<T, E> s);
 */
namespace GD::Ar {

/** \brief  Input file based on a file.
 *
 * Implements the IFType concept - this object may be treated as a non-seekable input file.
 */
class InputFile
{
public:
  /** \brief The file is seekable.  */
  static constexpr bool seekable = false;

  /** \brief  The size of the file is fixed.  */
  static constexpr bool size_fixed = false;

  /** \brief      Construct the input file.
   *  \param name Name of the file.
   */
  InputFile(std::string name) : name_(name), fd_(::open(name.c_str(), O_RDONLY)), eof_(false)
  {
    if (fd_ == -1) {
      throw std::runtime_error("Unable to open file.");
    }
    if (::fstat(fd_, &stat_) == -1) {
      throw std::runtime_error("Unable to stat file.");
    }
  }

  ~InputFile()
  {
    if (fd_ != -1) {
      ::close(fd_);
    }
  }

  InputFile(InputFile const&) = delete;
  InputFile(InputFile&& rhs) noexcept
      : name_(std::move(rhs.name_)), stat_(std::move(rhs.stat_)), fd_(std::move(rhs.fd_)),
        eof_(std::move(rhs.eof_))
  {
    rhs.fd_ = -1;
  }

  InputFile& operator=(InputFile const&) = delete;
  InputFile& operator=(InputFile&& rhs)
  {
    if (&rhs != this) {
      name_ = std::move(rhs.name_);
      stat_ = std::move(rhs.stat_);
      fd_ = std::move(rhs.fd_);
      eof_ = std::move(rhs.eof_);
      rhs.fd_ = -1;
    }

    return *this;
  }

  std::size_t size_bytes() const noexcept { return stat_.st_size; }

  template<typename T, std::size_t Count>
  void read(std::span<T, Count> dest)
  {
    auto res = read_upto(dest);
    if (res != dest.size_bytes()) {
      throw std::runtime_error("Unable to read enough bytes.");
    }
  }

  template<typename T, std::size_t Count>
  std::size_t read_upto(std::span<T, Count> dest)
  {
    auto saved_errno = errno;
    ::ssize_t res;
    std::byte* data = std::as_writable_bytes(dest).data();
    std::size_t count = dest.size_bytes();
    while (count > 0) {
      errno = 0;
      res = ::read(fd_, data, count);
      if (res < 0) {
        if (errno != EINTR) {
          throw std::system_error(errno, std::generic_category(), "Whilst reading.");
        }
        continue;
      }
      if (res == 0) {
        eof_ = true;
        break;
      }
      assert(res > 0);
      count -= res;
      data += res;
    };

    errno = saved_errno;
    return dest.size_bytes() - count;
  }

  bool eof() const noexcept { return eof_; }

  std::string const& name() const { return name_; }

  time_t mtime() const { return stat_.st_mtim.tv_sec; }

  uid_t uid() const { return stat_.st_uid; }

  gid_t gid() const { return stat_.st_gid; }

  mode_t mode() const { return stat_.st_mode; }

private:
  std::string name_;  ///< File name.
  struct stat stat_;  ///< File stats.
  int fd_;            ///< File descriptor
  bool eof_;          ///< Have we reached end of file.
};

/** \brief  Input file based on a memory span.
 *
 * Implements the IFType concept - this object may be treated as a seekable input file.
 */
class MemorySpanInputFile
{
public:
  /** \brief The file is seekable.  */
  static constexpr bool seekable = true;

  /** \brief  The size of the file is fixed.  */
  static constexpr bool size_fixed = true;

  /** \brief     Construct the input file.
   *  \param mem Memory to use as the file data.
   *
   * \a mem must remain valid for the lifetime of this MemorySpanInputFile object.
   */
  template<typename T, std::size_t E>
  MemorySpanInputFile(std::span<T, E> mem) : data_(std::as_bytes(mem)), offset_(0)
  {
  }

  ~MemorySpanInputFile() = default;
  MemorySpanInputFile(MemorySpanInputFile const&) = default;
  MemorySpanInputFile(MemorySpanInputFile&&) = default;
  MemorySpanInputFile& operator=(MemorySpanInputFile const&) = default;
  MemorySpanInputFile& operator=(MemorySpanInputFile&&) = default;

  std::size_t size_bytes() const noexcept { return data_.size_bytes(); }

  std::size_t offset_bytes() const noexcept { return offset_; }

  void offset_bytes(std::size_t offset)
  {
    if (offset >= size_bytes()) {
      throw std::runtime_error("Tried to set offset outside of data.");
    }
    offset_ = offset;
  }

  template<typename T, std::size_t Count>
  void read(std::span<T, Count> dest)
  {
    read_at(dest, offset_);
    offset_ += dest.size_bytes();
  }

  template<typename T, std::size_t Count>
  void read_at(std::span<T, Count> dest, std::size_t offset) const
  {
    if (offset > size_bytes()) {
      throw std::runtime_error("Reading outside of data.");
    }
    if (size_bytes() - offset < dest.size_bytes()) {
      throw std::runtime_error("Trying to read too many bytes");
    }
    memcpy(dest.data(), data_.data() + offset, dest.size_bytes());
  }

  template<typename T, std::size_t Count>
  std::size_t read_upto(std::span<T, Count> dest)
  {
    std::size_t count = std::min(dest.size_bytes(), size_bytes() - offset_);
    memcpy(dest.data(), data_.data() + offset_, count);
    offset_ += count;
    return count;
  }

  bool eof() const noexcept { return size_bytes() == offset_; }

private:
  std::span<std::byte const> data_;  ///< Data we're using
  std::size_t offset_;               ///< Current offset into the file.
};

class MemoryFileWriter
{
public:
  explicit MemoryFileWriter() = default;
  ~MemoryFileWriter() = default;

  MemoryFileWriter(MemoryFileWriter const&) = delete;
  MemoryFileWriter(MemoryFileWriter&&) = default;
  MemoryFileWriter& operator=(MemoryFileWriter const&) = delete;
  MemoryFileWriter& operator=(MemoryFileWriter&&) = default;

  /** \brief Write
   *  \tparam T Type of data in span
   *  \tparam E Extent of span
   *  \param span Span to output.
   */
  template<typename T, std::size_t E>
  void write(std::span<T, E> span)
  {
    auto raw_data = std::as_bytes(span);
    data_.reserve(data_.size() + raw_data.size_bytes());
    std::copy(raw_data.begin(), raw_data.end(), std::back_inserter(data_));
  }

  /** \brief  Get the data that has been written to this memory file.  */
  std::span<std::byte const> data() const noexcept { return std::span<std::byte const>(data_); }

private:
  std::vector<std::byte> data_;
};

/** \brief  Implement a transactional file writer.
 *
 * By "transactional" we mean that the destination file is either completely written with the new
 * data, or the previous file state is left.
 *
 * Example usage:
 *
 * \code {.c++}
 * TxnWriteFile file("file.txt", 06440);
 * file.write("This is some text\n");
 * file.commit(); // Needed to show we've finished writing.
 * \endcode
 *
 */
class TxnWriteFile
{
public:
  /** \brief  Constructor
   *  \param dest Destination file
   *  \param mode Mode to give destination file.
   */
  TxnWriteFile(fs::path const& dest, mode_t mode)
      : fd_(-1), dest_(dest), temp_(), mode_(mode & 01777)
  {
    // Use a temporary file if the destination already exists.
    if (fs::exists(dest_)) {
      temp_ = dest_ + "XXXXXX";
      while (fd_ == -1) {
        fd_ = ::mkstemp(temp_.data());
        if (fd_ == -1 && errno != EINTR) {
          throw std::system_error(errno, std::generic_category(), "Whilst opening temp file.");
        }
      }
      return;
    }

    temp_ = dest_;
    while (fd_ == -1) {
      fd_ = ::open(dest_.c_str(), O_WRONLY | O_CREAT, S_IWUSR | S_IRUSR);
      if (fd_ == -1 && errno != EINTR) {
        throw std::system_error(errno, std::generic_category(), "Whilst opening.");
      }
    }
  }

  /** \brief Destructor
   *
   * If TxnWriteFile::commit() has *not* been called this will abort the file write.
   */
  ~TxnWriteFile() { abort(); }

  /** Cannot copy.  */
  TxnWriteFile(TxnWriteFile const&) = delete;
  TxnWriteFile& operator=(TxnWriteFile const&) = delete;

  /** \brief Move constructor.  */
  TxnWriteFile(TxnWriteFile&& rhs)
      : fd_(std::move(rhs.fd_)), dest_(std::move(rhs.dest_)), temp_(std::move(rhs.temp_)),
        mode_(std::move(rhs.mode_))
  {
    rhs.has_moved();
  }

  /** \brief Move assignment.  */
  TxnWriteFile& operator=(TxnWriteFile&& rhs)
  {
    if (this != &rhs) {
      fd_ = std::move(rhs.fd_);
      dest_ = std::move(rhs.dest_);
      temp_ = std::move(rhs.temp_);
      mode_ = std::move(rhs.mode_);
      rhs.has_moved();
    }
    return *this;
  }

  /** \brief Write
   *  \tparam T Type of data in span
   *  \tparam E Extent of span
   *  \param span Span to output.
   */
  template<typename T, std::size_t E>
  void write(std::span<T, E> span)
  {
    if (fd_ == -1 || temp_.empty()) {
      throw std::runtime_error("Writing after file has been committed.");
    }
    auto data = std::as_bytes(span).data();
    auto count = span.size_bytes();
    while (count != 0) {
      auto res =
        ::write(fd_, data, std::min((std::size_t)std::numeric_limits<::ssize_t>::max(), count));
      if (res == -1) {
        if (errno != EINTR) {
          throw std::system_error(errno, std::generic_category(), "Whilst writing.");
        }
      }
      else {
        data += res;
        count -= res;
      }
    }
  }

  /** \brief Commit transaction.  */
  void commit()
  {
    if (fd_ == -1 || temp_.empty()) {
      throw std::runtime_error("Committing empty txn.");
    }
    ::close(fd_);
    fd_ = -1;
    if (temp_ != dest_) {
      if (::rename(temp_.c_str(), dest_.c_str()) == -1) {
        throw std::system_error(errno, std::generic_category(), "Whilst committing a write.");
      }
    }
    temp_.clear();
    /* And set the mode bits appropriately.  */
    chmod(dest_.c_str(), mode_);
  }

  /** \brief  Abort transaction.  */
  void abort()
  {
    if (fd_ != -1) {
      ::close(fd_);
      fd_ = -1;
    }
    if (!temp_.empty())
      ::remove(temp_.c_str());
    temp_.clear();
  }

private:
  /** \brief Mark the object as having been moved. */
  void has_moved()
  {
    fd_ = -1;
    temp_.clear();
  }

  int fd_;            ///< File descriptor.
  std::string dest_;  ///< Final destination file.
  std::string temp_;  ///< File we actually write to.
  mode_t mode_;       ///< Final mode to give file.
};

}  // namespace GD::Ar

#endif  // SRC_AR_FILES_HH_INCLUDED_
