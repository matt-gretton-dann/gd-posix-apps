#include "util/utils.hh"

#include <iostream>
#include <langinfo.h>
#include <regex>
#include <string>
#include <string_view>

bool GD::confirm_action(std::string_view msg)
{
  char const* yesexpr = nl_langinfo(YESEXPR);
  char const* noexpr = nl_langinfo(NOEXPR);
  if (yesexpr == nullptr) {
    yesexpr = "^[yY]";
  }
  if (noexpr == nullptr) {
    noexpr = "^[nN]";
  }

  std::regex yesre(yesexpr, std::regex_constants::basic | std::regex_constants::nosubs);
  std::regex nore(noexpr, std::regex_constants::basic | std::regex_constants::nosubs);
  while (true) {
    std::cout << msg << " ";
    std::string response;
    std::cin >> response;
    if (std::regex_search(response, yesre)) {
      return true;
    }
    else if (std::regex_search(response, nore)) {
      return false;
    }
  }
}
