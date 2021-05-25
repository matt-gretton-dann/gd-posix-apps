/** \file   libgdsup/include/gd/signal.h
 *  \brief  Expose signal.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_SIGNAL_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_SIGNAL_H_INCLUDED

#include "gd/bits/defines.h"
#include "gd/bits/types/pid_t.h"
#include "gd/bits/types/uid_t.h"

#include <signal.h>

#if _WIN32

#  define SIGKILL 9  /**< Kill. */
#  define SIGSTOP 19 /**< Stop executing. */

/** Number of elements in the ss_flags array. */
#  define __GD_SS_FLAGS_SIZE 1

/** How many bits in an individual flags entry. */
#  define __GD_SS_FLAGS_BITS 32

/** \brief Signal set.
 *
 * Do not manipulate directly.  Instead use sigemptyset(), sigfillset() to get hold of initial
 * values, and the sigaddset(), sigdelset() to modify.
 */
typedef struct
{
  unsigned int __gd_ss_flags[__GD_SS_FLAGS_SIZE];
} sigset_t;

static_assert(sizeof(unsigned int) == 4, "Unsigned integers are expected to be 4 bytes.");

/** \brief Union of signal values passed in siginfo_t.  */
union sigval
{
  int sival_int;
  void* sival_ptr;
};

/** Generic siginfo codes. */
#  define SI_USER 1
#  define SI_QUEUE 2
#  define SI_TIMER 3
#  define SI_ASYNCIO 4
#  define SI_MESGQ 5

/** \brief  Signal info.
 */
typedef struct
{
  int si_signo;          /**< Signal number.  */
  int si_code;           /**< Signal sub-code. (Unimplemented currently on Windows). */
  int si_errno;          /**< Signal errno. (Unimplemented currently on Windows). */
  pid_t si_pid;          /**< Process ID that raised the signal.  */
  uid_t si_uid;          /**< User ID that raised the signal. */
  void* si_addr;         /**< Address of fault.  */
  int si_status;         /**< Exit value or signal  */
  long si_poll;          /**< Band event for SIGPOLL. (Unimplemented currently on Windows).  */
  union sigval si_value; /**< Signal value. */
} siginfo_t;

#  define SA_NOCLDSTOP 0x01 /**< Do not generate SIGCHLD when children stop.  Not implemented. */
#  define SA_ONSTACK 0x02 /**< Use an alternative stack.  Always active.  */
#  define SA_RESETHAND 0x04 /**< Reset handler to default on entry. */
#  define SA_RESTART 0x08 /**< Enable kernel restartable functions. Not implemented. */
#  define SA_SIGINFO 0x10 /**< Use sa_siginfo instead of sa_handler. */
#  define SA_NOCLDWAIT 0x20 /**< Do not wait for children */
#  define SA_NODEFER 0x40 /**< Do not block this signal on entry to the handler. */
#  define SS_ONSTACK 0x02 /**< Alternate stack enable. */
#  define SS_DISABLE 0x80 /**< Alternate stack disable. */

/** \brief Signal action. */
struct sigaction
{
  sigset_t sa_mask; /**< Set of signals to be blocked during handling of signal. */
  int sa_flags;     /**< Flags (See SA_* defines. */
  union
  {
    void (*sa_handler)(int); /**< Old style handler - used if sa_flags & SA_SIGINFO is 0. */
    void (*sa_sigaction)(int, siginfo_t*,
                         void*); /**< New style handler - used if sa_flags & SA_SIGINFO is not 0. */
  };
};

/** \brief        Examine and change a signal action.
 *  \param  _sig  Signal to manipulate
 *  \param  _act  Action to set for signal
 *  \param  _oact Old action
 *  \return       0 on success, -1 on failure, errno set.
 *
 * This is a very limited implementation currently.
 */
__EXTERN_C int sigaction(int _sig, struct sigaction const* __RESTRICT _act,
                         struct sigaction* __RESTRICT _oact);

/** \brief       Add signal to a set.
 *  \param  _set Signal set to update
 *  \param  _sig Signal to add to set.
 *  \return      0 on success, -1 on failure - errno set.
 */
__EXTERN_C int sigaddset(sigset_t* _set, int _sig);

/** \brief       Delete a signal from a set.
 *  \param  _set Signal set to update
 *  \param  _sig Signal to remove from set.
 *  \return      0 on success, -1 on failure - errno set.
 */
__EXTERN_C int sigdelset(sigset_t* _set, int _sig);

/** \brief       Set a signal set to empty
 *  \param  _set Signal set to empty
 *  \return      0 on success, -1 on failure - errno set.
 */
__EXTERN_C int sigemptyset(sigset_t* _set);

/** \brief       Set a signal set to full
 *  \param  _set Signal set to empty
 *  \return      0 on success, -1 on failure - errno set.
 */
__EXTERN_C int sigfillset(sigset_t* _set);

#endif  // _WIN32

#endif  // _LIBGDSUP_INCLUDE_GD_SIGNAL_H_INCLUDED
