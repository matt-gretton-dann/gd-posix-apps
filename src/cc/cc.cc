/** \file   cc/cc.cc
 *  \brief  Compiler driver.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/format.hh"
#include "gd/span.hh"
#include "gd/stdlib.h"

#include <iostream>

#include "error.hh"
#include "file-store.hh"
#include "preprocessor-tokenizer.hh"
#include "simple-tokenizers.hh"
#include "token.hh"

namespace {
void dump_tokens(GD::CPP::FileStore& file_store, GD::CPP::ErrorManager& em)
{
  auto trigraph_tokenizer = GD::CPP::TrigraphParser(file_store, em);
  auto slice_tokenizer = GD::CPP::NewLineChewer(trigraph_tokenizer, em);
  auto tokenizer = GD::CPP::PreprocessorTokenizer(slice_tokenizer, em);

  while (tokenizer.peek() != GD::CPP::TokenType::end_of_source) {
    auto const& token = tokenizer.peek();
    switch (token.type()) {
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
