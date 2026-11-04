// SPDX-License-Identifier: Apache-2.0
#ifndef GD_BC_MATH_LIBRARY_IMAGE_HH_INCLUDED
#define GD_BC_MATH_LIBRARY_IMAGE_HH_INCLUDED

#include <cstddef>

namespace GD::Bc::Library {
// The native generator and loader are built together. This is an internal
// encoding, with 16-bit little-endian lengths and biased signed offsets.
enum class ImageOperand {
  string,
  stream,
  offset,
  location,
  mask,
  unsigned_,
  letter,
  variable,
  array
};
struct ImageItem
{
  std::size_t begin;
  std::size_t size;
  unsigned function;  // Letter index, or Letter::count_ for an eager item.
};
inline constexpr unsigned image_word_max = 65535;
inline constexpr int image_offset_bias = 32768;
}  // namespace GD::Bc::Library

#endif
