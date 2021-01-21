/** \file   include/gd/bit.hh
 *  \brief  Handle including <bit> when it may not be implemented by a
 *          library.
 *  \author Copyright 2020, Matthew Gretton-Dann
 *
 * Include this file instead of <bit>, so that if necessary we can use
 * the experimental version.  The namespace ::bit is aliased to the appropriate
 * standard namespace.
 *
 * Note that this isn't a complete implementation - only the bits we need.
 */
#ifndef _LIBGDSUP_INCLUDE_GD_BIT_HH_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_BIT_HH_INCLUDED

#if defined(FORCE_SUPPLEMENTAL_LIBRARY)
#  define _USE_OUR_BIT_IMPLEMENTATION 1
#elif defined(__cpp_lib_endian) && __cpp_lib_endian >= 201907L && __has_include(<bit>)
#  include <bit>
namespace bit {
using endian = std::endian;
}  // namespace bit
#  define _USE_OUR_BIT_IMPLEMENTATION 0
#else
#  define _USE_OUR_BIT_IMPLEMENTATION 1
#endif

#if _USE_OUR_BIT_IMPLEMENTATION
#  include "gd/bits/defines.h"

#  if !defined(__BYTE_ORDER__) || !defined(__ORDER_LITTLE_ENDIAN__) ||                             \
    !defined(__ORDER_BIG_ENDIAN__)
#    error                                                                                         \
      "Require __BYTE_ORDER__, __ORDER_LITTLE_ENDIAN__, and __ORDER_BIG_ENDIAN__ to be defined."
#  endif

#  if (__BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__) && (__BYTE_ORDER__ != __ORDER_BIG_ENDIAN__)
#    error                                                                                         \
      "We require __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ or __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__"
#  endif

namespace bit {
/** \brief  Endianess class. */
enum class endian {
  little = __ORDER_LITTLE_ENDIAN__,
  big = __ORDER_BIG_ENDIAN__,
  native = __BYTE_ORDER__,
};

}  // namespace bit
#endif

#undef _USE_OUR_BIT_IMPLEMENTATION

#endif  // _LIBGDSUP_INCLUDE_GD_BIT_HH_INCLUDED
