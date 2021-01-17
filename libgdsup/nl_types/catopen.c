/** \file libgdsup/nl_types/catopen.c
 *  \brief Implemenation of catopen()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "fcntl/fcntl.h"
#include "gd/fcntl.h"
#include "gd/nl_types.h"
#include "gd/stdlib.h"
#include "gd/unistd.h"
#include "nl_types/nl_types.h"
#include "support/support.h"
#include "unistd/unistd.h"

#include <assert.h>
#include <errno.h>
#include <locale.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/** \brief       Attempt to open a catalogue file
 *  \param  path Path of file to attempt to open.
 *  \return      Catalogue value, NULL on file is not open, CATD_ERROR on other error.
 *
 * This is not quite the error codes expected for the return from \c catopen().  NULL is returned
 * when the file doesn't exist - this is so that the path searcher can move to the next file to
 * attempt.  Use \c generate_result() to convert the result of this function to an appropriate one
 * for \c catopen().
 */
static nl_catd do_catopen(char const* path)
{
#define error_return(e, fd)                                                                        \
  do {                                                                                             \
    errno = e;                                                                                     \
    close(fd);                                                                                     \
    return CATD_ERROR;                                                                             \
  } while (0)

  int fd = __fcntl_open(path, O_RDONLY | O_CLOEXEC);
  if (fd == -1) {
    if (errno == ENOENT) {
      return CATD_NOTFOUND;
    }
    return CATD_ERROR;
  }

  char buffer[CAT_HDR_SIZE];
  ssize_t read = __unistd_read(fd, buffer, CAT_HDR_SIZE);

  if (read != CAT_HDR_SIZE || !__nl_types_check_cat_header(buffer)) {
    __support_log("Catalogue header is invalid: %s\n", path);
    error_return(EINVAL, fd);
  }

  uint64_t size = __support_read_le_u64(buffer + CAT_HDR_FILE_SIZE_OFFSET);
  char* full_buffer = malloc((size_t)size);
  if (full_buffer == NULL) {
    error_return(ENOMEM, fd);
  }

  memcpy(full_buffer, buffer, CAT_HDR_SIZE);
  read = __unistd_read(fd, full_buffer + CAT_HDR_SIZE, size - CAT_HDR_SIZE);
  if (read == -1 || (size_t)read != size - CAT_HDR_SIZE) {
    __support_log("Catalogue size on disk does not match recorded size: %s", path);
    error_return(EINVAL, fd);
  }

  close(fd);
  return (nl_catd)full_buffer;

#undef error_return
}

/** \brief           Copy text from \a src to \a result.
 *  \param  src      Source buffer to read from.
 *  \param  src_len  Length of source buffer.
 *  \param  result   Buffer to insert into
 *  \param  off      Pointer to offset to insert into in \a result.  Updated with offset after copy.
 *  \param  capacity Pointer to capacity of \a result.  Updated on exit with new capacity.
 *  \return          Pointer to result buffer, or NULL if out of memory.
 *
 * Will resize the result buffer if necessary.  If out of memory will tidy up after itself.
 */
static char* copy_to_result(char const* src, size_t src_len, char* result, size_t* off,
                            size_t* capacity)
{
  if (*off + src_len > *capacity) {
    while (*off + src_len > *capacity) {
      *capacity += *capacity >> 1;
    }

    char* t = realloc(result, *capacity);
    if (t == NULL) {
      free(result);
      return NULL;
    }

    result = t;
  }

  memcpy(result + *off, src, src_len);
  *off += src_len;
  return result;
}

/** \brief         Expand an NLSPATH entry
 *  \param  begin  Pointer to start of entry
 *  \param  end    Pointer to one past the end of entry
 *  \param  name   Name of file to use in %N expansions.
 *  \param  locale Locale to use for %L, %l, %t, and %c expansions.
 *  \return        Expanded NLSPATH entry.  Result needs to be \c free'd().
 */
static char* expand_nlspath_entry(char const* begin, char const* end, char const* name,
                                  char const* locale)
{
  /* Guess an initial capacity for the string.  Make it length of the NLSPATH entry we're looking
     at, plus the name length + length of locale * 5.
   */
  size_t capacity = end - begin + strlen(end) + strlen(locale) * 5 + 2;

  char* result = malloc(capacity);
  if (result == NULL) {
    return NULL;
  }

  char const* lang = locale;
  char const* terr = locale;
  while (*terr != '\0' && *terr != '_' && *terr != '.') {
    ++terr;
  }
  size_t lang_len = terr - lang;
  char const* codeset = terr;
  size_t terr_len = 0;
  if (*terr == '_') {
    ++terr;
    codeset = terr;
    while (*codeset != '\0' && *codeset != '.') {
      ++codeset;
    }
    terr_len = codeset - terr;
  }
  size_t codeset_len = 0;
  if (*codeset == '.') {
    codeset_len = strlen(codeset + 1);
  }

  size_t p = 0;
  bool expanding_percent = false;
  for (char const* it = begin; it < end; ++it) {
    if (!expanding_percent) {
      if (*it == '%') {
        expanding_percent = true;
      }
      else {
        result = copy_to_result(it, 1, result, &p, &capacity);
      }
    }
    else {
      expanding_percent = false;
      switch (*it) {
      case '%':
        result = copy_to_result(it, 1, result, &p, &capacity);
        break;
      case 'N':
        result = copy_to_result(name, strlen(name), result, &p, &capacity);
        break;
      case 'L':
        result = copy_to_result(locale, strlen(locale), result, &p, &capacity);
        break;
      case 'l':
        if (lang_len != 0) {
          result = copy_to_result(lang, lang_len, result, &p, &capacity);
        }
        break;
      case 't':
        if (terr_len != 0) {
          result = copy_to_result(terr, terr_len, result, &p, &capacity);
        }
        break;
      case 'c':
        if (codeset_len != 0) {
          result = copy_to_result(codeset, codeset_len, result, &p, &capacity);
        }
        break;
      default:
        assert(false && "Unexpected character following %");
        errno = ENOENT;
        free(result);
        result = NULL;
      }

      if (result == NULL) {
        return NULL;
      }
    }
  }

  return result;
}

/** \brief          Iterate through all NLSPATH entries attempting to open the catalogue file.
 *  \param  nlspath NLSPATH to expand
 *  \param  name    Name to use in %N expansion.
 *  \param  locale  Locale to use in %L, %l, %t, and %c.
 *  \return         Catalogue ID, NULL if no catalogue found, -1 on other error.
 *
 * This is not quite the error codes expected for the return from \c catopen().  NULL is returned
 * when the file doesn't exist - this is so that the path searcher can move to the next file to
 * attempt.  Use \c generate_result() to convert the result of this function to an appropriate one
 * for \c catopen().
 */
static nl_catd do_catopen_path(char const* nlspath, char const* name, char const* locale)
{
  int saved_errno = errno;

  char const* p = nlspath;
  while (p != NULL) {
    char const* end = strchr(p, ':');
    if (end == NULL) {
      p = NULL;
      continue;
    }

    if (p == end) {
      nl_catd result = do_catopen(name);
      if (result != CATD_NOTFOUND) {
        return result;
      }
    }
    else {
      char* expanded_name = expand_nlspath_entry(p, end, name, locale);
      if (expanded_name == NULL) {
        return CATD_ERROR;
      }
      nl_catd result = do_catopen(name);
      free(expanded_name);
      if (result != CATD_NOTFOUND) {
        return result;
      }
    }

    errno = saved_errno;
    p = end + 1;
  }

  return CATD_NOTFOUND;
}

/** \brief         Generate a \c catopen() result
 *  \param  result Prototype result.
 *  \return        Expected result from \c catopen().
 *
 * Basically returns \a result unless it is \c NULL in which case it sets \c errno to \c ENOENT.
 */
static nl_catd generate_result(nl_catd result)
{
  if (result == CATD_NOTFOUND) {
    errno = ENOENT;
    return CATD_ERROR;
  }
  return result;
}

nl_catd catopen(char const* name, int oflag)
{
  if (oflag != 0 && oflag != NL_CAT_LOCALE) {
    __support_log("catopen: Illegal oflag: %d\n", oflag);
    errno = EINVAL;
    return CATD_ERROR;
  }

  if (name == NULL || name[0] == '\0') {
    errno = ENOENT;
    return CATD_ERROR;
  }

  /* If name has a / we don't use a NLSPATH lookup.  */
  if (strchr(name, '/') != NULL) {
    return generate_result(do_catopen(name));
  }

  /* Determine the locale identifier to use:
   * Windows doesn't support LC_MESSAGES - use LC_ALL instead as a safe alternative.
   * #160 tracks this.
   */
  int lc_id =
#ifdef _WIN32
    LC_ALL
#else
    LC_MESSAGES
#endif
    ;
  char const* locale = oflag == 0 ? getenv("LANG") : setlocale(lc_id, NULL);
  if (locale == NULL || locale[0] == '\0') {
    locale = "C";
  }

  /* Find the NLSPATH.  */
  char const* nlspath = getenv("NLSPATH");

  /* Do lookup on the NLSPATH.  */
  if (nlspath != NULL && nlspath[0] != '\0') {
    assert(strchr(nlspath, '/') == NULL);
    int saved_errno = errno;
    nl_catd result = do_catopen_path(nlspath, name, locale);
    if (result != CATD_NOTFOUND) {
      return result;
    }
    errno = saved_errno;
  }

  /* NLSPATH lookup failed - try the default NLSPATH instead.  */
  return generate_result(do_catopen_path(DEFAULT_NLSPATH, name, locale));
}
