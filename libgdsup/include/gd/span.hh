/** \file  libgdsup/include/gd/span.hh
 *  \brief Expose Spanlibrary
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef GD_SPAN_HH
#define GD_SPAN_HH

#if !defined(FORCE_SUPPLEMENTAL_LIBRARY) && __has_include(<span>)
#  include <span>
namespace GD::Std {
using dynamic_extent = std::dynamic_extent;
template<typename T, std::size_t Extent = std::dynamic_extent>
using span = std::span<T, Extent>;
using as_bytes = std::as_bytes;
using as_writable_bytes = std::as_writable_bytes;
}  // namespace GD::Std
#else
#  include <array>
#  include <cstddef>
#  include <iterator>
#  include <limits>

#  include <type_traits>

namespace GD::Std {

using size_t = ::std::size_t;
inline constexpr size_t dynamic_extent = ::std::numeric_limits<size_t>::max();

namespace Details {
template<typename T, size_t Extent>
struct SpanBase
{
public:
  constexpr SpanBase() noexcept = delete;
  constexpr SpanBase(SpanBase const&) noexcept = default;
  constexpr SpanBase(SpanBase&&) noexcept = delete;
  constexpr SpanBase& operator=(SpanBase const&) noexcept = default;
  constexpr SpanBase& operator=(SpanBase&&) noexcept = delete;
  constexpr ~SpanBase() noexcept = default;

  constexpr SpanBase(T* begin, [[maybe_unused]] size_t extent) noexcept : begin_(begin) {}
  constexpr T* begin() const noexcept { return begin_; }
  constexpr size_t size() const noexcept { return Extent; }

private:
  T* begin_;
};

template<typename T>
struct SpanBase<T, 0>
{
public:
  constexpr SpanBase() noexcept = default;
  constexpr SpanBase(SpanBase const&) noexcept = default;
  constexpr SpanBase(SpanBase&&) noexcept = delete;
  constexpr SpanBase& operator=(SpanBase const&) noexcept = default;
  constexpr SpanBase& operator=(SpanBase&&) noexcept = delete;
  constexpr ~SpanBase() noexcept = default;

  constexpr SpanBase(T* begin, [[maybe_unused]] size_t extent) noexcept : begin_(begin) {}
  constexpr T* begin() const noexcept { return begin_; }
  constexpr size_t size() const noexcept { return 0; }

private:
  T* begin_ = nullptr;
};

template<typename T>
struct SpanBase<T, GD::Std::dynamic_extent>
{
public:
  constexpr SpanBase() noexcept = default;
  constexpr SpanBase(SpanBase const&) noexcept = default;
  constexpr SpanBase(SpanBase&&) noexcept = delete;
  constexpr SpanBase& operator=(SpanBase const&) noexcept = default;
  constexpr SpanBase& operator=(SpanBase&&) noexcept = delete;
  constexpr ~SpanBase() noexcept = default;

  constexpr SpanBase(T* begin, size_t extent) noexcept : begin_(begin), extent_(extent) {}
  constexpr T* begin() const noexcept { return begin_; }
  constexpr size_t size() const noexcept { return extent_; }

private:
  T* begin_ = nullptr;
  size_t extent_ = 0;
};

}  // namespace Details

template<typename T, size_t Extent = dynamic_extent>
class span : public Details::SpanBase<T, Extent>
{
public:
  using element_type = T;
  using value_type = ::std::remove_cv_t<T>;
  using size_type = size_t;
  using difference_type = ::std::ptrdiff_t;
  using pointer = T*;
  using const_pointer = T const*;
  using reference = T&;
  using const_reference = T const&;
  using iterator = T*;
  using reverse_iterator = ::std::reverse_iterator<iterator>;

  static constexpr size_t extent = Extent;

  constexpr span() noexcept = default;

  template<typename It>
  explicit(extent != dynamic_extent) constexpr span(It begin, size_type extent)
      : Details::SpanBase<T, Extent>(begin, extent)
  {
  }

  template<typename It>
  explicit(extent != dynamic_extent) constexpr span(It begin, It end)
      : Details::SpanBase<T, Extent>(begin, end - begin)
  {
  }

  template<std::size_t N>
  constexpr span(element_type (&arr)[N]) noexcept : Details::SpanBase<T, Extent>(arr, N)
  {
  }

  template<typename U, std::size_t N>
  constexpr span(::std::array<U, N>& arr) noexcept : Details::SpanBase<T, Extent>(arr.data(), N)
  {
  }

  template<typename U, std::size_t N>
  constexpr span(::std::array<U, N> const& arr) noexcept
      : Details::SpanBase<T, Extent>(arr.data(), N)
  {
  }

  template<typename U, std::size_t N>
  explicit(extent != dynamic_extent &&
           N == dynamic_extent) constexpr span(span<U, N> const& source) noexcept
      : Details::SpanBase<T, Extent>(source.data(), source.size())
  {
  }

  constexpr span(span const&) noexcept = default;
  constexpr span(span&&) noexcept = delete;

  constexpr span& operator=(span const&) noexcept = default;
  constexpr span& operator=(span&&) noexcept = delete;

  constexpr ~span() noexcept = default;

  constexpr iterator begin() const noexcept { return Details::SpanBase<T, Extent>::begin(); }
  constexpr iterator end() const noexcept { return begin() + size(); }
  constexpr iterator rbegin() const noexcept { return begin() + size(); }
  constexpr iterator rend() const noexcept { return begin(); }

  constexpr reference front() const { return *begin(); }
  constexpr reference back() const { return *(begin() + size() - 1); }
  constexpr reference operator[](size_type idx) const { return begin()[idx]; }
  constexpr pointer data() const noexcept { return begin(); }

  constexpr size_type size() const noexcept { return Details::SpanBase<T, Extent>::size(); }

  constexpr size_type size_bytes() const noexcept { return size() * sizeof(T); }
  [[nodiscard]] constexpr bool empty() const noexcept { return size() == 0; }

  template<std::size_t Count>
  constexpr span<element_type, Count> first() const
  {
    return span<element_type, Count>(begin(), Count);
  }

  constexpr span<element_type, dynamic_extent> first(size_type count) const
  {
    return span<element_type, dynamic_extent>(begin(), count);
  }

  template<std::size_t Count>
  constexpr span<element_type, Count> last() const
  {
    return span<element_type, Count>(begin() + size() - Count, Count);
  }

  constexpr span<element_type, dynamic_extent> last(size_type count) const
  {
    return span<element_type, dynamic_extent>(begin() + size() - count, count);
  }

  template<size_t Offset, size_t Count = dynamic_extent>
  constexpr span<element_type, (Count == dynamic_extent
                                  ? (Extent == dynamic_extent ? dynamic_extent : Extent - Offset)
                                  : Count)>
  subspan() const
  {
    if constexpr (Count == dynamic_extent) {
      if constexpr (Extent == dynamic_extent) {
        return span<element_type, dynamic_extent>(begin() + Offset, size() - Offset);
      }
      else if constexpr (Extent != dynamic_extent) {
        return span<element_type, Extent - Offset>(begin() + Offset, size() - Offset);
      }
    }
    else if constexpr (Count != dynamic_extent) {
      return span<element_type, Count>(begin() + Offset, Count);
    }
  }

  constexpr span<element_type, dynamic_extent> subspan(size_type Offset,
                                                       size_type Count = dynamic_extent) const
  {
    return span<element_type, dynamic_extent>(begin() + Offset,
                                              Count == dynamic_extent ? size() - Offset : Count);
  }
};

template<typename It, typename EndOrSize>
span(It, EndOrSize) -> span<::std::remove_reference_t<std::iter_reference_t<It>>>;
template<typename T, size_t N>
span(T (&)[N]) -> span<T, N>;
template<typename T, size_t N>
span(::std::array<T, N>&) -> span<T, N>;
template<typename T, size_t N>
span(const ::std::array<T, N>&) -> span<const T, N>;

template<typename T, size_t N>
span<std::byte const, (N == dynamic_extent ? N : N * sizeof(T))> as_bytes(span<T, N> s) noexcept
{
  if constexpr (N == dynamic_extent) {
    return span<std::byte const, dynamic_extent>(reinterpret_cast<std::byte const*>(s.data()),
                                                 s.size() * sizeof(T));
  }
  else if constexpr (N != dynamic_extent) {
    return span<std::byte const, N * sizeof(T)>(reinterpret_cast<std::byte const*>(s.data()),
                                                s.size() * sizeof(T));
  }
}

template<typename T, size_t N>
span<std::byte, (N == dynamic_extent ? N : N * sizeof(T))> as_writable_bytes(span<T, N> s) noexcept
{
  if constexpr (N == dynamic_extent) {
    return span<std::byte, dynamic_extent>(reinterpret_cast<std::byte*>(s.data()),
                                           s.size() * sizeof(T));
  }
  else if constexpr (N != dynamic_extent) {
    return span<std::byte, N * sizeof(T)>(reinterpret_cast<std::byte*>(s.data()),
                                          s.size() * sizeof(T));
  }
}

}  // namespace GD::Std
#endif  // Pick a spanheader.

#endif  // GD_SPAN_HH
