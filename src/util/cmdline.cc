/**
 * \file      src/util/cmdline.cc
 * \brief     Implement command line parsing utilities.
 * \author    Matthew Gretton-Dann
 * \copyright 2021, Matthew Gretton-Dann
 *            SPDX-License-Identifier: Apache-2.0
 */

#include "util/cmdline.hh"

#include "gd/unistd.h"

#include "util/utils.hh"

#include "util-messages.hh"

#include <assert.h>

#include "util-internals.hh"

namespace {
/** \brief  Helper for std::visit */
template<class... Ts>
struct overloaded : Ts...
{
  using Ts::operator()...;
};
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

}  // namespace

GD::CmdlineParser::CmdlineParser() : args_(":")
{
  /* We cheat and handle the 'error' characters ':' and '?' as normal options, except they don't
   * appear in the \c args_ list.
   */
  opts_.insert({':', [&](char) { return missing_argument(static_cast<char>(optopt)); }});
  opts_.insert({'?', [&](char) { return invalid_option(static_cast<char>(optopt)); }});
}

void GD::CmdlineParser::add_option(char c, OptFn fn)
{
  auto [it, success] = opts_.insert({c, fn});
  assert(success);
  args_.push_back(c);
}

void GD::CmdlineParser::add_option(char c, ArgFn fn)
{
  auto [it, success] = opts_.insert({c, fn});
  assert(success);
  args_.push_back(c);
  args_.push_back(':');
}

void GD::CmdlineParser::add_option(char c, bool& flag)
{
  auto fn = [&flag](char) {
    flag = true;
    return true;
  };
  add_option(c, fn);
}

int GD::CmdlineParser::parse_args(int argc, char** argv) const
{
  int c;
  bool success = true;
  while ((c = ::getopt(argc, argv, args_.data())) != -1) {
    auto it = opts_.find(c);
    assert(it != opts_.end());
    success &= std::visit(overloaded{[c](OptFn fn) { return fn(static_cast<char>(c)); },
                                     [c](ArgFn fn) { return fn(static_cast<char>(c), optarg); }},
                          it->second);
  }

  return success ? optind : argc + 1;
}

bool GD::CmdlineParser::missing_argument(char c) const
{
  Util::message(Util::Msg::missing_argument, c);
  return false;
}

bool GD::CmdlineParser::invalid_option(char c) const
{
  Util::message(Util::Msg::invalid_option, c);
  return false;
}
