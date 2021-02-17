/** \file   libgdsup/stdlib/setenv-win32.c
 *  \brief  Win32 API implementation of setenv()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/bits/defines.h"
#include "gd/stdlib.h"

#include <errno.h>
#include <stddef.h>
#include <string.h>

#include "support/support.h"

int setenv(char const* envname, char const* envval, int overwrite)
{
  /* Check for valid environment name.  */
  if (envname == NULL || *envname == '\0' || strchr(envname, '=') != NULL) {
    __support_log("setenv: Invalid environment name: %s\n", envname == NULL ? "(null)" : envname);
    errno = EINVAL;
    return -1;
  }

  /* If we're only adding check for existance of variable already.  */
  if (overwrite == 0) {
    size_t var_size = 0;
    getenv_s(&var_size, NULL, 0, envname);
    if (var_size != 0) {
      return 0;
    }
  }

  /* And update the environment.  */
  errno_t e = _putenv_s(envname, envval);
  if (e == 0) {
    return 0;
  }

  errno = e;
  return -1;
}
