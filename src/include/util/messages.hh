/** \file   util/messages.hh
 *  \brief  Messages utility classes and functions
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_MESSAGES_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_MESSAGES_HH_INCLUDED

#include "gd/nl_types.h"

#include "gd/format.hh"

#include <array>
#include <string>
#include <vector>

#include <string_view>

namespace GD {

/** \brief       Messages Template class
 *  \tparam Data Data structure.  See below for required members.
 *
 * This class provides the majority of the infrastructure to manage i18n messages for a program.
 * It is configured by the \a Data structure template parameter.  Usually this data structure will
 * have been initialised by the Python gen-messages.py script having processed the appropriate
 * .messages.json file.
 *
 * Construct the class after calling <tt>::setlocale(LC_ALL, "")</tt> in the main function.
 *
 * \subsection Data structure
 *
 * The data structure consists of a set of <tt>static constexpr</tt> member variables which describe
 * the i18n configuration, and provide the base fallback messages.  It also provides some
 * \c typedefs to enumeration types that help identify the types in use.
 *
 *  - \c SetEnum: Enumeration class giving the list of sets in the catalogue.
 *  - \c MessageEnum: Enumeration class giving the list of messages in the catalogue.
 *  - \c catalogue_: Castable to <tt>const char*</tt>.  Contains the root name of the message
 *    catalogue on disk.
 *  - \c default_set_: Of type \c Data::SetEnum.  Default set to use when none is provided.
 *  - \c messages_: Can be viewed as of type <tt>const char*[][]</tt>.  That is a two dimensional
 *    array of strings.  The first index is to the set ID, the second is to the message ID.  The
 *    string is the default "C" locale message for that (set, message) tuple.  As there is no set
 *    or message 0 the indexing is offset by one - so Set 1 is at index 0, etc...
 */
template<typename Data>
class Messages : private Data
{
public:
  /** \brief  Destructor.  */
  ~Messages()
  {
    if (catd_ != static_cast<nl_catd>(-1)) {
      ::catclose(catd_);
    }
  }

  Messages(Messages const&) = delete;
  Messages& operator=(Messages const&) = delete;
  Messages(Messages&&) = delete;
  Messages& operator=(Messages&&) = delete;

  /** \brief  Get the one instance of this class.
   *  \return Reference to the Messages class.
   */
  static Messages const& get()
  {
    static Messages the_messages;
    return the_messages;
  }

  /** \brief      Get a string_view of the message associated with \a msg in the default set.
   *  \param  msg Message ID.
   *  \return     Message
   *
   * This function is not necessarily thread safe, and future calls to any \c Messages function may
   * invalidate the returned string view.
   */
  std::string_view get(typename Data::MessageEnum msg) const noexcept
  {
    return get(Data::default_set_, msg);
  }

  /** \brief      Get a string_view of the message associated with (\a set, \a msg) pair.
   *  \param  set Set ID
   *  \param  msg Message ID.
   *  \return     Message
   *
   * This function is not necessarily thread safe, and future calls to any \c Messages function may
   * invalidate the returned string view.
   */
  std::string_view get(typename Data::SetEnum set, typename Data::MessageEnum msg) const noexcept
  {
    auto val =
      Data::messages_[static_cast<std::size_t>(set) - 1][static_cast<std::size_t>(msg) - 1];
    if (catd_ == (nl_catd)-1) {
      return val;
    }

    auto p = ::catgets(catd_, static_cast<int>(set), static_cast<int>(msg), val);
    return std::string_view(p);
  }

  /** \brief      Get a std::string copy of the message associated with \a msg in the default set.
   *  \param  msg Message ID.
   *  \return     Message
   *
   * This function is not necessarily thread safe.
   */
  std::string get_copy(typename Data::MessageEnum msg) const { return std::string(get(msg)); }

  /** \brief      Get a std::string copy of the message associated with (\a set, \a msg) pair.
   *  \param  set Set ID
   *  \param  msg Message ID.
   *  \return     Message
   *
   * This function is not necessarily thread safe.
   */
  std::string get_copy(typename Data::SetEnum set, typename Data::MessageEnum msg) const
  {
    return std::string(get(set, msg));
  }

  /** \brief       Apply std::format to the message with message ID \a msg in the default set.
   *  \param  msg  Message ID.
   *  \param  args Arguments for the formatter
   *  \return      Formatted message
   *
   * This function is not necessarily thread safe.
   */
  template<typename... Ts>
  std::string format(typename Data::MessageEnum msg, Ts... args) const
  {
    return format(Data::default_set_, msg, args...);
  }

  /** \brief       Apply std::format to the message with ID pair (\a set, \a msg).
   *  \param  set  Set ID
   *  \param  msg  Message ID.
   *  \param  args Arguments for the formatter
   *  \return      Formatted message
   *
   * This function is not necessarily thread safe.
   */
  template<typename... Ts>
  std::string format(typename Data::SetEnum set, typename Data::MessageEnum msg, Ts... args) const
  {
    return fmt::format(get(set, msg), args...);
  }

private:
  /** \brief  Default constructor.  */
  Messages() : catd_(::catopen(Data::catalogue_, NL_CAT_LOCALE)) {}

  nl_catd catd_;  ///< Message catalogue handle.  */
};

}  // namespace GD

#endif  // _SRC_INCLUDE_UTIL_MESSAGES_HH_INCLUDED
