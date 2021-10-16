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

namespace {
void report_error(GD::CPP::FileStore const& fs, GD::CPP::Location loc, GD::CPP::Error const& error)
{
  std::cerr << fmt::format("{0}:{1}:{2}:{3}:{4}:{5}\n", fs.logical_filename(loc),
                           fs.logical_line(loc), fs.logical_column(loc), error.id(),
                           error.severity(), error.message());
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
}

void dump_tokens(GD::CPP::FileStore& file_store)
{
  while (file_store.peek() != GD::CPP::TokenType::end_of_source) {
    auto const& token = file_store.peek();
    switch (token.type()) {
    case GD::CPP::TokenType::error:
      report_error(file_store, token.range().begin(), token.get<GD::CPP::Error>());
      break;
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
    file_store.chew(token.type());
  }

  file_store.chew(GD::CPP::TokenType::end_of_source);
}
}  // namespace

auto main(int argc, char** argv) -> int
try {
  auto args = GD::Span::span<char*>(argv + 1, argc - 1);  // NOLINT

  GD::CPP::ErrorManager error_manager;
  GD::CPP::FileStore file_store(error_manager);

  if (args.empty()) {
    file_store.push_standard_input();
    dump_tokens(file_store);
  }
  else {
    for (auto const* file : args) {
      file_store.push_file(file);
      dump_tokens(file_store);
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
