# \file   CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

add_utility(NAME bc NO_INT_TEST CAT_ID bc
            SOURCES bc.cc lexer.cc reader.cc token.cc)
