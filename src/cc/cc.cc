/** \file   cc/cc.cc
 *  \brief  Compiler driver.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/format.hh"
#include "gd/span.hh"
#include "gd/stdlib.h"

#include <iostream>

#include "error.hh"
#include "file-store.hh"
#include "token.hh"
#include "tokenizers.hh"

namespace {
void dump_tokens(GD::CPP::FileStore& file_store, GD::CPP::ErrorManager& em)
{
  auto tokenizer = GD::CPP::NewLineChewer(file_store, em);

  while (tokenizer.peek() != GD::CPP::TokenType::end_of_source) {
    auto const& token = tokenizer.peek();
    switch (token.type()) {
    case GD::CPP::TokenType::character:
      std::copy(file_store.range_begin(token.range()), file_store.range_end(token.range()),
                std::ostream_iterator<char>(std::cout));
      break;
    case GD::CPP::TokenType::end_of_include:
    case GD::CPP::TokenType::end_of_source:
      break;
    default:
      std::cout << fmt::format("{0}", token);
    }
    tokenizer.chew(token.type());
  }

  tokenizer.chew(GD::CPP::TokenType::end_of_source);
}
}  // namespace

auto main(int argc, char** argv) -> int
try {
  auto args = GD::Span::span<char*>(argv + 1, argc - 1);  // NOLINT

  GD::CPP::ErrorManager error_manager(std::cerr);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);

  if (args.empty()) {
    file_store.push_standard_input();
    dump_tokens(file_store, error_manager);
  }
  else {
    for (auto const* file : args) {
      file_store.push_file(file);
      dump_tokens(file_store, error_manager);
    }
  }

  return 0;
}
catch (std::exception const& e) {
  GD::CPP::ErrorManager::ice("Uncaught standard exception: {0}\n", e.what());
}
catch (...) {
  GD::CPP::ErrorManager::ice("Uncaught exception\n");
}
