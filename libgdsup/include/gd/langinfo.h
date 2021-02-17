/**
 * \file      libgdsup/include/gd/langinfo.h
 * \brief     Wrapper around
 * \author    Matthew Gretton-Dann
 * \copyright 2021, Matthew Gretton-Dann
 *            SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_LANGINFO_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_LANGINFO_H_INCLUDED

#if __has_include(<langinfo.h>)
#  include <langinfo.h>
#else

#  include "gd/nl_types.h"

#  include "gd/bits/defines.h"

__EXTERN_C_BEGIN

/** \brief Valid `nl_item` values. */
enum __nl_defines {
  CODESET,     /**< Codeset name.  */
  D_T_FMT,     /**< String for formatting date and time.  */
  D_FMT,       /**< Date format string.  */
  T_FMT,       /**< Time format string.  */
  T_FMT_AMPM,  /**< a.m. or pm. time format string.  */
  AM_STR,      /**< ante-meridian affix.  */
  PM_STR,      /**< post-meridian affix.  */
  DAY_1,       /**< Full name of first day of week (Sunday).  */
  DAY_2,       /**< Full name of second day of week.  */
  DAY_3,       /**< Full name of third day of week.  */
  DAY_4,       /**< Full name of fourth day of week.  */
  DAY_5,       /**< Full name of fifth day of week.  */
  DAY_6,       /**< Full name of sixth day of week.  */
  DAY_7,       /**< Full name of seventh day of week.  */
  ABDAY_1,     /**< Abbreviated name of first day of week (Sunday).  */
  ABDAY_2,     /**< Abbreviated name of second day of week.  */
  ABDAY_3,     /**< Abbreviated name of third day of week.  */
  ABDAY_4,     /**< Abbreviated name of fourth day of week.  */
  ABDAY_5,     /**< Abbreviated name of fifth day of week.  */
  ABDAY_6,     /**< Abbreviated name of sixth day of week.  */
  ABDAY_7,     /**< Abbreviated name of seventh day of week.  */
  MON_1,       /**< Full name of first month of the year.  */
  MON_2,       /**< Full name of second month of the year.  */
  MON_3,       /**< Full name of third month of the year.  */
  MON_4,       /**< Full name of fourth month of the year.  */
  MON_5,       /**< Full name of fifth month of the year.  */
  MON_6,       /**< Full name of sixth month of the year.  */
  MON_7,       /**< Full name of seventh month of the year.  */
  MON_8,       /**< Full name of eighth month of the year.  */
  MON_9,       /**< Full name of ninth month of the year.  */
  MON_10,      /**< Full name of tenth month of the year.  */
  MON_11,      /**< Full name of eleventh month of the year.  */
  MON_12,      /**< Full name of twelve month of the year.  */
  ABMON_1,     /**< Abbreviated name of first month of the year.  */
  ABMON_2,     /**< Abbreviated name of second month of the year.  */
  ABMON_3,     /**< Abbreviated name of third month of the year.  */
  ABMON_4,     /**< Abbreviated name of fourth month of the year.  */
  ABMON_5,     /**< Abbreviated name of fifth month of the year.  */
  ABMON_6,     /**< Abbreviated name of sixth month of the year.  */
  ABMON_7,     /**< Abbreviated name of seventh month of the year.  */
  ABMON_8,     /**< Abbreviated name of eighth month of the year.  */
  ABMON_9,     /**< Abbreviated name of ninth month of the year.  */
  ABMON_10,    /**< Abbreviated name of tenth month of the year.  */
  ABMON_11,    /**< Abbreviated name of eleventh month of the year.  */
  ABMON_12,    /**< Abbreviated name of twelve month of the year.  */
  ERA,         /**< Era description segments.  */
  ERA_D_FMT,   /**< Era date format string.  */
  ERA_D_T_FMT, /**< Era date-time format string.  */
  ERA_T_FMT,   /**< Era time format string.  */
  ALT_DIGITS,  /**< Alternative symbols for digits.  */
  RADIXCHAR,   /**< Radix character.  */
  THOUSEP,     /**< Thousands separator.  */
  YESEXPR,     /**< Affirmative response expression.  */
  NOEXPR,      /**< Negative response expression.  */
  CRNCYSTR,    /**< Currency symbol.  */
};

/**
 * \brief         Get language information.
 * \param  __item Item to query
 * \return char*  Value or \c NULL if error.
 */
char* nl_langinfo(nl_item __item);

__EXTERN_C_END
#endif

#endif /* _LIBGDSUP_INCLUDE_GD_LANGINFO_H_INCLUDED */
