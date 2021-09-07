/** \file   libgdsup/support/support.h
 *  \brief  Header for internal support function
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_SUPPORT_SUPPORT_H_INCLUDED
#define _LIBGDSUP_SUPPORT_SUPPORT_H_INCLUDED

#include "gd/bits/defines.h"

#include <stdint.h>

/** \brief      Read a little-endian 32-bit value from memory
 *  \param  buf Memory location to read from.  Must be aligned to 32-bits
 *  \return     32-bit value.
 */
// NOLINTNEXTLINE(modernize-use-trailing-return-type)
__EXTERN_C uint32_t __support_read_le_u32(char const* buf);

/** \brief      Read a little-endian 64-bit value from memory
 *  \param  buf Memory location to read from.  Must be aligned to 64-bits
 *  \return     64-bit value.
 */
// NOLINTNEXTLINE(modernize-use-trailing-return-type)
__EXTERN_C uint64_t __support_read_le_u64(char const* buf);

/** \brief        Log a debug message
 *  \param format Print format
 *  \param ...    Format args
 */
// NOLINTNEXTLINE(modernize-use-trailing-return-type)
__EXTERN_C void __support_log(char const* format, ...);

/** \brief  Is logging enabled?  Set to 0 to disable logging.  */
__EXTERN_C_BEGIN
extern int __support_logging_enabled;
__EXTERN_C_END

#endif  // _LIBGDSUP_SUPPORT_SUPPORT_H_INCLUDED
