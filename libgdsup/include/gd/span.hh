/** \file  libgdsup/include/gd/span.hh
 *  \brief Expose Spanlibrary
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef GD_SPAN_HH
#define GD_SPAN_HH

#if !defined(FORCE_SUPPLEMENTAL_LIBRARY) && __has_include(<span>)
#  include <span>
namespace GD::Span {
using namespace std;  // NOLINT(google-build-using-namespace)
}  // namespace GD::Span
#else
#  include <array>
#  include <cstddef>
#  include <iterator>
#  include <limits>
#  include <type_traits>
#  if defined(__cpp_impl_three_way_comparison) && __cpp_impl_three_way_comparison >= 201907L
#    include <compare>
#  endif

namespace GD::Span {

using size_t = ::std::size_t;
inline constexpr size_t dynamic_extent = ::std::numeric_limits<size_t>::max();

namespace Details {
template<typename T, size_t Extent>
struct SpanBase  // NOLINT
{
public:
  constexpr SpanBase() noexcept = delete;
  constexpr SpanBase(SpanBase const&) noexcept = default;
  constexpr auto operator=(SpanBase const&) noexcept -> SpanBase& = default;
  ~SpanBase() noexcept = default;

  constexpr SpanBase(T* begin, [[maybe_unused]] size_t extent) noexcept : begin_(begin) {}
  [[nodiscard]] constexpr auto begin() const noexcept -> T* { return begin_; }
  [[nodiscard]] constexpr auto size() const noexcept -> size_t { return Extent; }

private:
  T* begin_;
};

template<typename T>
struct SpanBase<T, 0>  // NOLINT
{
public:
  constexpr SpanBase() noexcept = default;
  constexpr SpanBase(SpanBase const&) noexcept = default;
  constexpr auto operator=(SpanBase const&) noexcept -> SpanBase& = default;
  ~SpanBase() noexcept = default;

  constexpr SpanBase(T* begin, [[maybe_unused]] size_t extent) noexcept : begin_(begin) {}
  [[nodiscard]] constexpr auto begin() const noexcept -> T* { return begin_; }
  [[nodiscard]] constexpr auto size() const noexcept -> size_t { return 0; }

private:
  T* begin_ = nullptr;
};

template<typename T>
struct SpanBase<T, GD::Span::dynamic_extent>  // NOLINT
{
public:
  constexpr SpanBase() noexcept = default;
  constexpr SpanBase(SpanBase const&) noexcept = default;
  constexpr auto operator=(SpanBase const&) noexcept -> SpanBase& = default;
  ~SpanBase() noexcept = default;

  constexpr SpanBase(T* begin, size_t extent) noexcept : begin_(begin), extent_(extent) {}
  [[nodiscard]] constexpr auto begin() const noexcept -> T* { return begin_; }
  [[nodiscard]] constexpr auto size() const noexcept -> size_t { return extent_; }

private:
  T* begin_ = nullptr;
  size_t extent_ = 0;
};

template<typename T>
class SpanIterator
{
public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = ::std::remove_cv_t<T>;
  using difference_type = std::ptrdiff_t;
  using pointer = T*;
  using reference = T&;

  constexpr explicit SpanIterator(T* it) noexcept : p_(it) {}
  constexpr SpanIterator() noexcept = delete;
  constexpr SpanIterator(SpanIterator const&) noexcept = default;
  constexpr SpanIterator(SpanIterator&&) noexcept = default;
  constexpr auto operator=(SpanIterator const&) noexcept -> SpanIterator& = default;
  constexpr auto operator=(SpanIterator&&) noexcept -> SpanIterator& = default;
  ~SpanIterator() noexcept = default;

  [[nodiscard]] constexpr auto operator*() const noexcept -> reference { return *p_; }
  [[nodiscard]] constexpr auto operator->() const noexcept -> pointer { return p_; }

  constexpr auto operator++() noexcept -> SpanIterator&
  {
    ++p_;  // NOLINT
    return *this;
  }

  constexpr auto operator++(int) noexcept -> SpanIterator
  {
    auto temp = *this;
    ++p_;  // NOLINT
    return temp;
  }

  constexpr auto operator--() noexcept -> SpanIterator&
  {
    --p_;
    return *this;
  }

  constexpr auto operator--(int) noexcept -> SpanIterator
  {
    auto temp = *this;
    --p_;
    return temp;
  }

  constexpr auto operator+=(difference_type const offset) noexcept -> SpanIterator&
  {
    p_ += offset;  // NOLINT
    return *this;
  }

  constexpr auto operator-=(difference_type const offset) noexcept -> SpanIterator&
  {
    p_ -= offset;
    return *this;
  }

  [[nodiscard]] constexpr auto operator-(SpanIterator rhs) const noexcept -> difference_type
  {
    return this->p_ - rhs.p_;
  }

  [[nodiscard]] constexpr auto operator[](difference_type const offset) const noexcept -> reference
  {
    return p_[offset];  // NOLINT
  }

  [[nodiscard]] constexpr auto operator==(SpanIterator rhs) const noexcept -> bool
  {
    return rhs.p_ == this->p_;
  }

#  if defined(__cpp_impl_three_way_comparison) && __cpp_impl_three_way_comparison >= 201907L
  [[nodiscard]] constexpr auto operator<=>(SpanIterator rhs) const noexcept -> std::strong_ordering
  {
    return this->p_ <=> rhs.p_;
  }
#  else
  [[nodiscard]] constexpr auto operator<(SpanIterator rhs) const noexcept -> bool
  {
    return this->p_ < rhs.p_;
  }
#  endif

private:
  T* p_;
};

template<typename T>
[[nodiscard]] constexpr auto
operator+(SpanIterator<T> it, typename SpanIterator<T>::difference_type const offset) noexcept
  -> SpanIterator<T>
{
  it += offset;
  return it;
}

template<typename T>
[[nodiscard]] constexpr auto operator+(typename SpanIterator<T>::difference_type const offset,
                                       SpanIterator<T> it) noexcept -> SpanIterator<T>
{
  it += offset;
  return it;
}

template<typename T>
[[nodiscard]] constexpr auto operator-(SpanIterator<T> it,
                                       typename SpanIterator<T>::difference_type offset) noexcept
  -> SpanIterator<T>
{
  it -= offset;
  return it;
}

#  if !defined(__cpp_impl_three_way_comparison) || __cpp_impl_three_way_comparison < 201907L
template<typename T>
[[nodiscard]] constexpr auto operator!=(SpanIterator<T> lhs, SpanIterator<T> rhs) noexcept -> bool
{
  return !(lhs == rhs);
}

template<typename T>
[[nodiscard]] constexpr auto operator<=(SpanIterator<T> lhs, SpanIterator<T> rhs) noexcept -> bool
{
  return (lhs == rhs) || (lhs < rhs);
}

template<typename T>
[[nodiscard]] constexpr auto operator>(SpanIterator<T> lhs, SpanIterator<T> rhs) noexcept -> bool
{
  return !(lhs == rhs) && !(lhs < rhs);
}

template<typename T>
[[nodiscard]] constexpr auto operator>=(SpanIterator<T> lhs, SpanIterator<T> rhs) noexcept -> bool
{
  return !(lhs < rhs);
}
#  endif  // Implement comparisons in absence of <=>

}  // namespace Details

template<typename T, size_t Extent = dynamic_extent>
class span : public Details::SpanBase<T, Extent>  // NOLINT
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
  using iterator = Details::SpanIterator<T>;
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
  constexpr span(element_type (&arr)[N]) noexcept  // NOLINT
      : Details::SpanBase<T, Extent>(arr, N)
  {
  }

  template<typename U, std::size_t N>
  constexpr span(::std::array<U, N>& arr) noexcept  // NOLINT
      : Details::SpanBase<T, Extent>(arr.data(), N)
  {
  }

  template<typename U, std::size_t N>
  constexpr span(::std::array<U, N> const& arr) noexcept  // NOLINT
      : Details::SpanBase<T, Extent>(arr.data(), N)
  {
  }

  template<typename U, std::size_t N>
  explicit(extent != dynamic_extent &&                                             // NOLINT
           N == dynamic_extent) constexpr span(span<U, N> const& source) noexcept  // NOLINT
      : Details::SpanBase<T, Extent>(source.data(), source.size())
  {
  }

  constexpr span(span const&) noexcept = default;

  constexpr auto operator=(span const&) noexcept -> span& = default;

  ~span() noexcept = default;

  [[nodiscard]] constexpr auto begin() const noexcept -> iterator
  {
    return Details::SpanIterator<T>(Details::SpanBase<T, Extent>::begin());
  }
  [[nodiscard]] constexpr auto end() const noexcept -> iterator { return begin() + size(); }
  [[nodiscard]] constexpr auto rbegin() const noexcept -> iterator { return begin() + size(); }
  [[nodiscard]] constexpr auto rend() const noexcept -> iterator { return begin(); }

  [[nodiscard]] constexpr auto front() const -> reference { return *begin(); }
  [[nodiscard]] constexpr auto back() const -> reference { return *(begin() + size() - 1); }
  [[nodiscard]] constexpr auto operator[](size_type idx) const -> reference { return begin()[idx]; }
  [[nodiscard]] constexpr auto data() const noexcept -> pointer
  {
    return Details::SpanBase<T, Extent>::begin();
  }

  [[nodiscard]] constexpr auto size() const noexcept -> size_type
  {
    return Details::SpanBase<T, Extent>::size();
  }

  [[nodiscard]] constexpr auto size_bytes() const noexcept -> size_type
  {
    return size() * sizeof(T);
  }
  [[nodiscard]] constexpr auto empty() const noexcept -> bool { return size() == 0; }

  template<std::size_t Count>
  constexpr auto first() const -> span<element_type, Count>
  {
    return span<element_type, Count>(begin(), Count);
  }

  [[nodiscard]] constexpr auto first(size_type count) const -> span<element_type, dynamic_extent>
  {
    return span<element_type, dynamic_extent>(begin(), count);
  }

  template<std::size_t Count>
  constexpr auto last() const -> span<element_type, Count>
  {
    return span<element_type, Count>(begin() + size() - Count, Count);
  }

  [[nodiscard]] constexpr auto last(size_type count) const -> span<element_type, dynamic_extent>
  {
    return span<element_type, dynamic_extent>(begin() + size() - count, count);
  }

  template<size_t Offset, size_t Count = dynamic_extent>
  constexpr auto subspan() const
    -> span<element_type,
            (Count == dynamic_extent ? (Extent == dynamic_extent ? dynamic_extent : Extent - Offset)
                                     : Count)>
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

  [[nodiscard]] constexpr auto subspan(size_type Offset, size_type Count = dynamic_extent) const
    -> span<element_type, dynamic_extent>
  {
    return span<element_type, dynamic_extent>(data() + Offset,
                                              Count == dynamic_extent ? size() - Offset : Count);
  }
};

template<typename T, size_t N>
span(T (&)[N]) -> span<T, N>;  // NOLINT
template<typename T, size_t N>
span(::std::array<T, N>&) -> span<T, N>;
template<typename T, size_t N>
span(const ::std::array<T, N>&) -> span<const T, N>;

template<typename T, size_t N>
auto as_bytes(span<T, N> s) noexcept
  -> span<std::byte const, (N == dynamic_extent ? N : N * sizeof(T))>
{
  if constexpr (N == dynamic_extent) {
    // NOLINTNEXTLINE
    return span<std::byte const, dynamic_extent>(reinterpret_cast<std::byte const*>(s.data()),
                                                 s.size() * sizeof(T));
  }
  else if constexpr (N != dynamic_extent) {
    // NOLINTNEXTLINE
    return span<std::byte const, N * sizeof(T)>(reinterpret_cast<std::byte const*>(s.data()),
                                                s.size() * sizeof(T));
  }
}

template<typename T, size_t N>
auto as_writable_bytes(span<T, N> s) noexcept
  -> span<std::byte, (N == dynamic_extent ? N : N * sizeof(T))>
{
  if constexpr (N == dynamic_extent) {
    // NOLINTNEXTLINE
    return span<std::byte, dynamic_extent>(reinterpret_cast<std::byte*>(s.data()),
                                           s.size() * sizeof(T));
  }
  else if constexpr (N != dynamic_extent) {
    // NOLINTNEXTLINE
    return span<std::byte, N * sizeof(T)>(reinterpret_cast<std::byte*>(s.data()),
                                          s.size() * sizeof(T));
  }
}

}  // namespace GD::Span
#endif    // Pick a spanheader.

#endif  // GD_SPAN_HH
