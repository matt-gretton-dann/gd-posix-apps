/** \file   libcpp/include/tokenizer.hh
 *  \brief  Base class for Tokenizer
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBCPP_INCLUDE_TOKENIZER_HH_
#define LIBCPP_INCLUDE_TOKENIZER_HH_

#include <optional>

#include "error.hh"
#include "file-store.hh"
#include "token.hh"

namespace GD::CPP {

/** \brief         Helper class to provide a tokenizer.  Derive from this class using CRTP.
 *  \tparam Impl   Implementation class that implements peeking
 *  \tparam Parent Parent class that we chain from.
 *
 * \subsection TokenizerUsage Usage
 *
 * Derived classes need to implement the following interfaces:
 *
 * \code
 * template<typename P>
 * class Impl : public Tokenizer<Impl<P>, P>
 * {
 *    auto do_peek(Parent& parent) -> std::optional<Token>;
 * };
 * \endcode
 *
 * \c do_peek() should implement peeking the next token.  If the token is unchanged from the token
 * the parent would return then it can return \c std::nullopt.  If this takes ownership or replaces
 * the parent's token then \c parent.chew() must be called for any used tokens.
 *
 * The class \c Parent must implement the same interface as \c Tokenizer or \c FileStore.
 */
template<typename Impl, typename Parent>
class Tokenizer
{
public:
  Tokenizer() = delete;
  Tokenizer(Tokenizer const&) = delete;
  Tokenizer(Tokenizer&&) noexcept = default;
  auto operator=(Tokenizer const&) -> Tokenizer& = delete;
  auto operator=(Tokenizer&&) noexcept -> Tokenizer& = delete;
  ~Tokenizer() = default;

  /** \brief  Peek the next token from the token stream.
   *  \return Next token in the stream.
   */
  [[nodiscard]] auto peek() -> Token const&
  {
    if (!token_) {
      token_ = impl()->do_peek(parent_);
    }
    return token_ ? *token_ : parent_.peek();
  }

  /** \brief  Chew the current token, accepting any.
   */
  void chew()
  {
    (void)peek();
    if (token_) {
      token_.reset();
    }
    else {
      parent_.chew();
    }
  }

  /** \brief       Chew the current token, it must have the given token type.
   *  \param  type Expected token type.
   */
  void chew(TokenType type)
  {
    (void)peek();
    if (token_) {
      assert_ice(type == token_->type(), "Token must be one of specified types.");
      token_.reset();
    }
    else {
      parent_.chew(type);
    }
  }

  /** \brief             Chew the current token, it must have one of the given token types.
   *  \param type_begin_ Begin iterator for types we accept
   *  \param type_end_   End iterator for types we accept.
   */
  template<typename It>
  void chew(It type_begin, It type_end)
  {
    (void)peek();
    if (token_) {
      assert_ice(std::any_of(type_begin, type_end, token_->type()),
                 "Token must be one of specified types.");
      token_.reset();
    }
    else {
      parent_.chew(type_begin, type_end);
    }
  }

  /** \brief       Push a stream.
   *  \param  name Name to give the stream.
   *  \param  is   Input stream.
   *
   * This is mostly used by testing - but may have a use if you want to parse a generated text
   * string of data.
   */
  void push_stream(std::string const& name, std::istream& is) { parent_.push_stream(name, is); }

  /** \brief        Push a file into the tokenizer stack
   *  \param  fname File name.
   */
  void push_file(std::string const& fname) { parent_.push_file(fname); }

  /** \brief  Push the standard input onto the tokenizer stack.
   */
  void push_standard_input() { parent_.push_standard_input(); }

  /** \brief        Get an iterator to the first character in a range.
   *  \param  range Range to query
   *  \return       Iterator to beginning of range.
   */
  [[nodiscard]] auto range_begin(Range range) const -> std::string::const_iterator
  {
    return parent_.range_begin(range);
  }

private:
  friend Impl;
  explicit Tokenizer(Parent& parent) : parent_(parent), token_(std::nullopt) {}

  [[nodiscard]] auto impl() noexcept -> Impl* { return static_cast<Impl*>(this); }
  [[nodiscard]] auto impl() const noexcept -> Impl const* { return static_cast<Impl const*>(this); }

  Parent& parent_;
  std::optional<Token> token_;
};

}  // namespace GD::CPP
#endif  //  LIBCPP_INCLUDE_TOKENIZER_HH_
