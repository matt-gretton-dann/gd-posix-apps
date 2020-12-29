/** \file true-false.c
 *  \brief  Implement `true` and `false`
 *  \author Copyright 2020, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 *
 * Requires TRUE_FALSE_EXIT_CODE to be defined on the command line.  Will be the exit code used by 
 * the program.
 */

#ifndef TRUE_FALSE_EXIT_CODE
#define TRUE_FALSE_EXIT_CODE 1
#error "Must define TRUE_FALSE_EXIT_CODE"
#endif

int main(void) {
    /* Whilst not specified by POSIX most true/false implementations ignore command line arguments.
       We do the same.  */
    return TRUE_FALSE_EXIT_CODE;
}