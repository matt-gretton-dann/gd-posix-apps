/** \file   include/util/utils.hh
 *  \brief  General utilities
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_UTILS_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_UTILS_HH_INCLUDED

#include <string_view>

namespace GD {
/** \brief       Set the program name.
 *  \param argv0 argv[0].
 */
void program_name(std::string_view argv0);

/** \brief  Get the program name.
 *  \return Program name.
 */
std::string_view program_name();

/** \brief      Confirm an action.
 *  \param  msg Message to display
 *  \return     True if answer is confirmatory (matches YESEXPR regular expression).
 */
bool confirm_action(std::string_view msg);

}  // namespace GD
#endif  // _SRC_INCLUDE_UTIL_UTILS_HH_INCLUDED
