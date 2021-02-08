/** \file   libgdsup/unistd/getopt.c
 *  \brief  Implement getopt() and friends
 *  \author Copyright 2020-2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include "gd/stdlib.h"
#include "gd/string.h"
#include "gd/unistd.h"

#include <stdio.h>

/* In this implementation optarg is also used to indicate the next option we want to look at.
 * If optarg is not NULL and points to a character in argv[optind] when getopt() is called then
 * that is the next flag to process.
 */
char* optarg = NULL;
int opterr = 1;
int optind = 1;
int optopt = 0;

static void update_for_next_call(char* opt) __NOEXCEPT
{
  optarg = opt + 1;
  if (*optarg == '\0') {
    optind += 1;
    optarg = NULL;
  }
}

int getopt(int argc, char* const argv[], const char* optstring) __NOEXCEPT
{
  /* Don't go off the end of the array.  */
  if (optind >= argc) {
    return -1;
  }

  /* Check for operands.  */
  if (argv[optind] == NULL || argv[optind][0] != '-' || argv[optind][1] == '\0') {
    return -1;
  }

  /* Check for the option/operand separator '--'.  We already know we have at least one -.  */
  if (argv[optind][1] == '-' && argv[optind][2] == '\0') {
    ++optind;
    return -1;
  }

  /* Find the argument to process.  */
  char* opt = argv[optind] + 1;
  if (optarg != NULL) {
    for (char* p = opt; *p != '\0'; ++p) {
      if (p == optarg) {
        opt = optarg;
        break;
      }
    }
  }

  char* ps = strchr(optstring, *opt);

  if (ps == NULL) {
    /* *opt doesn't point to a valid option.  */
    optopt = *opt;
    update_for_next_call(opt);
    if (opterr != 0 && optstring[0] != ':') {
      char* argv0 = strdup(argv[0]);
      char const* program_name = argv0 != NULL ? basename(argv0) : argv[0];
      int r = fprintf(stderr, "%s: Invalid option '-%c'\n", program_name, (char)optopt);
      free(argv0);
      if (r == -1) {
        return -1;
      }
    }
    return '?';
  }

  if (ps[1] == ':') {
    /* We want an argument with this option.  */
    if (opt[1] == '\0') {
      /* It is in the next argv entry.  */
      optarg = argv[optind + 1];
      optind += 2;
      if (optind > argc) {
        /* We have run out of argv entries.  So no argument.  */
        optopt = *opt;
        update_for_next_call(opt);
        if (opterr != 0 && optstring[0] != ':') {
          char* argv0 = strdup(argv[0]);
          char const* program_name = argv0 != NULL ? basename(argv0) : argv[0];
          int r =
            fprintf(stderr, "%s: No argument provided for '-%c'\n", program_name, (char)optopt);
          free(argv0);
          if (r == -1) {
            return -1;
          }
        }
        return optstring[0] == ':' ? ':' : '?';
      }
    }
    else {
      /* Argument tacked onto option.  */
      optarg = opt + 1;
      optind += 1;
    }
  }
  else {
    /* *opt is just a flag.  Update optind and optarg to point to next result.  */
    update_for_next_call(opt);
  }

  return *opt;
}
