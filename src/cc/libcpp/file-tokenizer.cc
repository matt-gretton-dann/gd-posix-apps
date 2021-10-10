
#include <cassert>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "file-store.hh"

GD::CPP::FileTokenizer::FileTokenizer(FileStore& fs)
    : fs_(fs), location_(), line_(), token_(std::nullopt)
{
}

auto GD::CPP::FileTokenizer::peek() -> Token const&
{
  if (!token_) {
    do_peek();
  }
  assert(token_.has_value());  // NOLINT
  return *token_;
}

auto GD::CPP::FileTokenizer::chew() { token_.reset(); }

auto GD::CPP::FileTokenizer::chew(TokenType type_)
{
  assert(token_.has_value());       // NOLINT
  assert(token_->type() == type_);  // NOLINT
  token_.reset();
}

template<typename It>
auto GD::CPP::FileTokenizer::chew(It type_begin_, It type_end_)
{
  assert(token_.has_value());                                              // NOLINT
  assert(std::find(type_begin_, type_end_, token_->type()) != type_end_);  // NOLINT
  token_.reset();
}

void GD::CPP::FileTokenizer::peek_special_conditions()
{
  auto error = fs_.error();
  if (error) {
    location_ = error->first;
    line_ = nullptr;
    token_.emplace(TokenType::error, Range{location_}, error->second);
    return;
  }

  auto eof = fs_.eof();
  if (eof) {
    location_ = *eof;
    line_ = nullptr;
    token_.emplace(TokenType::eof, Range{location_});
  }
}

void GD::CPP::FileTokenizer::do_peek()
{
  if (line_ != nullptr) {
    peek_character();
  }

  if (!token_) {
    peek_special_conditions();
  }

  if (!token_) {
    auto [location, line] = fs_.next_line();
    location_ = location;
    line_ = line;
    assert(line_ != nullptr || fs_.error().has_value() || fs_.eof().has_value());  // NOLINT
    do_peek();
  }
}

void GD::CPP::FileTokenizer::peek_character()
{
  assert(line_ != nullptr);  // NOLINT
  auto const* end = line_;
  constexpr char utf8_continuation = 0x80;
  while (*end != '\0' && (*end & utf8_continuation) != 0) {
    ++end;  // NOLINT
  }
  if (end == line_) {  // NOLINT
    ++end;             // NOLINT
  }

  Range range(location_, end - line_);
  token_.emplace(TokenType::character, range);

  if (*end == '\0' || *line_ == '\n') {
    line_ = nullptr;
  }
  else {
    location_ = location_ + static_cast<Column>(end - line_);
    line_ = end;
  }
}
