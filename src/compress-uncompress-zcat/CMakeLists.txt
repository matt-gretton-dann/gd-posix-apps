# \file   src/cksum/CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

# We generate the checksum table automatically...
add_utility(NAME compress CAT_ID compress SOURCES compress.cc)
