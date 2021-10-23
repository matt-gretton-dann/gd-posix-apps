#include <optional>

#include "error.hh"
#include "file-store.hh"
#include "token.hh"

namespace GD::CPP {

namespace Details {
/** \brief    Get the trigraph character to replace the given token by.
 *  \param  t Token to replace.  Can be any token.
 *  \return   Replacement character, or U'\0' if \a t does not have a valid replacement.
 */
auto trigraph_replacement(Token const& t) noexcept -> char32_t;
}  // namespace Details

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

/** \brief  Trigraph parser
 *  \tparam Parent Parent class type to call to get tokens.
 *
 * This parser handles the trigraph processing part of translation phase 1 (C2018-5.1.1.2-1-1).
 *
 * We warn on trigraph replacement as they may be deprecated on a surprise.
 */
template<typename Parent>
class TrigraphParser : public Tokenizer<TrigraphParser<Parent>, Parent>
{
public:
  TrigraphParser(Parent& parent, ErrorManager& em)
      : Tokenizer<TrigraphParser<Parent>, Parent>(parent), error_manager_(em)
  {
  }

private:
  friend class Tokenizer<TrigraphParser<Parent>, Parent>;

  auto do_peek(Parent& parent) -> std::optional<Token>
  {
    if (!pending_query_ && parent.peek() != U'?') {
      /* No query pending a return, and the character in parent is not a ? so just return.  */
      return std::nullopt;
    }

    if (pending_query_ && parent.peek() != U'?') {
      /* We have the first '?' of a trigraph but the next character isn't a '?', so clear the
       * pending_query_ flag and return the initial '?'.
       */
      pending_query_ = false;
      return Token(TokenType::character, query_range_, U'?');
    }

    /* Now the next token is a ?.  */
    assert_ice(parent.peek() == U'?', "Made wrong assumption about ?");

    if (!pending_query_) {
      /* This is the first '?' see what the next character is.  */
      auto t{parent.peek()};
      parent.chew(TokenType::character);
      if (parent.peek() != U'?') {
        /* Not a '?' return the original token.  */
        return t;
      }

      /* Is a '?' so we have a pending_query_.  */
      pending_query_ = true;
      query_range_ = t.range();
    }

    assert_ice(parent.peek() == U'?', "Made wrong assumption about ??.");
    assert_ice(pending_query_, "Should be pending a query by now.");

    /* We now have seen one '?' and parent points at the second,  chew parent and see what the next
     * token is, and do the trigraph replacement if needed.  */
    auto t{parent.peek()};
    parent.chew(TokenType::character);
    char32_t replacement = Details::trigraph_replacement(parent.peek());
    if (replacement == '\0') {
      /* This isn't a trigraph replacement, return the first '?' leaving the second '?' in the
       * pending_query for returning next time we're called.
       */
      auto rt = Token{TokenType::character, query_range_, U'?'};
      query_range_ = t.range();
      return rt;
    }

    /* We have a trigraph, chew the parent, and return the new token.  */
    auto r = parent.peek().range();
    char32_t c = parent.peek().character();
    r = Range{query_range_.begin(), r.end()};
    parent.chew(TokenType::character);
    pending_query_ = false;
    /* We're safe doing these static casts as the input and output chars of trigraph replacements
     * are all single byte UTF-8 chars.
     */
    error_manager_.error(ErrorCode::trigraph_replacement, r, static_cast<char>(c),
                         static_cast<char>(replacement));
    return Token{TokenType::character, r, replacement};
  }

  ErrorManager& error_manager_;
  bool pending_query_{false};
  Range query_range_{Location{0}, 0};
};

/** \brief         New Line Chewer and splice handler.
 *  \tparam Parent Parent Tokenizer
 *
 * This tokenizer implements Translation phase 2 (see C2018-5.1.1.2):
 *  * We splice lines (remove \ tokens followed by newlines).
 *  * Error on missing new-lines at end of files
 *  * Error on splices at the end of files.
 *  * Tokenize newlines to end_of_line tokens.
 *
 * If we encounter an error we insert a new line and treat it as non-fatal.
 */
template<typename Parent>
class NewLineChewer : public Tokenizer<NewLineChewer<Parent>, Parent>
{
public:
  NewLineChewer(Parent& parent, ErrorManager& em)
      : Tokenizer<NewLineChewer<Parent>, Parent>(parent), error_manager_(em)
  {
  }

private:
  friend class Tokenizer<NewLineChewer<Parent>, Parent>;

  auto do_peek(Parent& parent) -> std::optional<Token>
  {
    while (true) {
      Token const& t = parent.peek();
      Range r = t.range();

      if (t == TokenType::end_of_include) {
        /* As long as everything is OK we just chew the end of an include, as this is the only
         * place it makes a difference.  */
        parent.chew();

        switch (newline_state_) {
        case NewLineState::after_newline:
          /* No change of newline state.  */
          continue;
        case NewLineState::after_splice:
          error_manager_.error(ErrorCode::splice_followed_by_end_of_file, r);
          newline_state_ = NewLineState::after_newline;
          return Token{TokenType::character, r, U'\n'};
        case NewLineState::normal:
          error_manager_.error(ErrorCode::missing_newline_at_end_of_file, r);
          newline_state_ = NewLineState::after_newline;
          return Token{TokenType::character, r, U'\n'};
        default:
          assert_ice(false, "Unexpected value in switch.");
        }
      }

      if (t == TokenType::character) {
        if (t == U'\n') {
          newline_state_ = NewLineState::after_newline;
          return std::nullopt;
        }

        if (t == U'\\') {
          Token pending{t};
          /* See if we have a splice. */
          parent.chew();
          Token const& t2 = parent.peek();
          if (t2 == U'\n') {
            parent.chew();
            newline_state_ = NewLineState::after_splice;
            continue;
          }
          /* Wasn't a splice. */
          newline_state_ = NewLineState::normal;
          return pending;
        }
      }

      newline_state_ = NewLineState::normal;
      return std::nullopt;
    }
  }

  /** \brief Description of state in processing newlines. */
  enum class NewLineState {
    normal,         ///< We have seen a 'normal' non-newline character.
    after_newline,  ///< We are at the character after a newline (also true for initial
                    ///< state)
    after_splice    ///< We have just seen a splice
  };
  ErrorManager& error_manager_;
  NewLineState newline_state_ = NewLineState::after_newline;
};

}  // namespace GD::CPP
