/** \file libgdsup/nl_types/catclose.c
 *  \brief Implemenation of catclose()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/stdlib.h"

#include <errno.h>

#include "nl_types/nl_types.h"
#include "support/support.h"

int catclose(nl_catd catd)
{
  if (catd == CATD_ERROR || catd == CATD_NOTFOUND ||
      !__nl_types_check_cat_header((char const*)catd)) {              // NOLINT
    __support_log("catclose: Bad Catalogue ID [%p]\n", (void*)catd);  // NOLINT
    errno = EBADF;
    return -1;
  }

  free((void*)catd);  // NOLINT
  return 0;
}
