#include "util/utils.hh"

#include "util-messages.hh"

#include <iostream>

namespace GD::Util {

template<typename... Ts>
void message(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Util::Messages::get().format(GD::Util::Set::util, msg, args...) << '\n';
}

}  // namespace GD::Util
