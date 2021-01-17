/** \file libgdsup/nl_types/catgets.c
 *  \brief Implemenation of catgets()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"
#include "gd/stdlib.h"
#include "nl_types/nl_types.h"
#include "support/support.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>

/* See /docs/file-formats/message-catalogues.md for a description of the file format we are reading.
 */
char* catgets(nl_catd catd, int set_id, int msg_id, char const* s)
{
#define error_return(e, fmt, ...)                                                                  \
  do {                                                                                             \
    __support_log(fmt, __VA_ARGS__);                                                               \
    errno = e;                                                                                     \
    return (char*)s;                                                                               \
  } while (0)

  /* Parameter checks.  */
  if (catd == CATD_ERROR || catd == CATD_NOTFOUND ||
      !__nl_types_check_cat_header((char const*)catd)) {
    error_return(EBADF, "catgets: Bad Catalogue ID [%p, %d, %d]\n", catd, set_id, msg_id);
  }
  if (set_id < 1 || msg_id < 1) {
    error_return(ENOMSG, "catgets: Set ID and Message ID too low [%p, %d, %d]\n", catd, set_id,
                 msg_id);
  }

  char* buf = (char*)catd;
  uint32_t num_sets = __support_read_le_u32(buf + CAT_HDR_NUM_SET_OFFSET);

  /* Find the Set ID.  The header check above will ensure that we're not reading invalid memory
   * here.  */
  char const* off = buf + CAT_HDR_SIZE;
  unsigned i = 0;
  for (; i < num_sets; ++i) {
    if (__support_read_le_u32(off + CAT_ARRAY_ID_OFFSET) == (uint32_t)set_id) {
      break;
    }
    off += CAT_ARRAY_ENTRY_SIZE;
  }

  if (i == num_sets) {
    error_return(ENOMSG, "catgets: Set ID does not exist in catalogue [%p, %d, %d]\n", catd, set_id,
                 msg_id);
  }

  uint64_t cat_size = __support_read_le_u64(buf + CAT_HDR_FILE_SIZE_OFFSET);
  uint32_t num_msgs = __support_read_le_u32(off + CAT_ARRAY_COUNT_OFFSET);
  uint64_t msg_offset = __support_read_le_u64(off + CAT_ARRAY_PTR_OFFSET);
  if (msg_offset > UINT64_MAX - ((uint64_t)num_msgs) * CAT_ARRAY_ENTRY_SIZE) {
    error_return(EINVAL,
                 "catgets: Message array overflows uint64_t capacity [%p, %i, %i]: [% " PRIx64
                 ", %" PRIu32 "]\n",
                 catd, set_id, msg_id, msg_offset, num_msgs);
  }
  if (cat_size < msg_offset + ((uint64_t)num_msgs) * CAT_ARRAY_ENTRY_SIZE) {
    error_return(EINVAL,
                 "catgets: Message array overflows catalogue [%p, %i, %i]: [% " PRIx64 ", %" PRIu32
                 ", %" PRIx64 "]\n",
                 catd, set_id, msg_id, msg_offset, num_msgs, cat_size);
  }

  /* Search for the message buffer.  */
  off = buf + msg_offset;
  unsigned j = 0;
  for (; j < num_msgs; ++j) {
    if (__support_read_le_u32(off + CAT_ARRAY_ID_OFFSET) == (uint32_t)msg_id) {
      break;
    }
  }
  if (j == num_msgs) {
    error_return(ENOMSG, "catgets: Message ID does not exist in catalogue [%p, %d, %d]\n", catd,
                 set_id, msg_id);
  }

  uint32_t msg_len = __support_read_le_u32(off + CAT_ARRAY_COUNT_OFFSET);
  msg_offset = __support_read_le_u64(off + CAT_ARRAY_PTR_OFFSET);
  if (msg_offset > UINT64_MAX - msg_len) {
    error_return(EINVAL,
                 "catgets: Message string overflows uint64_t capacity [%p, %i, %i]: [% " PRIx64
                 ", %" PRIu32 "]\n",
                 catd, set_id, msg_id, msg_offset, msg_len);
  }
  if (cat_size < msg_offset + num_msgs) {
    error_return(EINVAL,
                 "catgets: Message string overflows catalogue [%p, %i, %i]: [% " PRIx64 ", %" PRIu32
                 ", %" PRIx64 "]\n",
                 catd, set_id, msg_id, msg_offset, msg_len, cat_size);
  }

  char* result = buf + msg_offset;

  if (result[msg_len - 1] != '\0') {
    error_return(EBADMSG, "catgets: Message does not end in a NUL character [%p, %i, %i]", catd,
                 set_id, msg_id);
  }

  return result;
#undef error_return
}
