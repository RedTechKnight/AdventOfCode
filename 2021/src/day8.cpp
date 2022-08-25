#include "main.hpp"

bool same_elements(std::string a, std::string b) {
  if (a.size() < b.size()) {
    return std::all_of(a.begin(), a.end(), [&b](char c) -> bool {
      for (auto chr : b) {
        if (chr == c) {
          return true;
        }
      }
      return false;
    });
  } else {
    return std::all_of(b.begin(), b.end(), [&a](char c) -> bool {
      for (auto chr : a) {
        if (chr == c) {
          return true;
        }
      }
      return false;
    });
  }
}

std::string difference(std::string a, std::string b) {
  std::string result = "";
  for (auto c : a) {
    if (std::count(b.begin(), b.end(), c) < 1) {
      result.push_back(c);
    }
  }
  return result;
}

std::string distinct(std::string a, std::string b) {
  std::string distinct = "";
  if (a.size() < b.size()) {
    for (auto c : a) {
      if (std::count(b.begin(), b.end(), c) < 1) {
        distinct.push_back(c);
      }
    }
  } else {
    for (auto c : b) {
      if (std::count(a.begin(), a.end(), c) < 1) {
        distinct.push_back(c);
      }
    }
  }
  return distinct;
}

void day8() {
  std::ifstream file("inputs/day8");
  std::vector<std::pair<std::string, std::string>> inputs;
  int count = 0;
  while (file.good()) {
    std::string signals;
    std::string outputs;
    std::getline(file, signals, '|');
    std::getline(file, outputs, '\n');
    if (signals.size() > 0)
      inputs.push_back(std::pair(signals, outputs));
  }
  auto println = [](std::string s) -> void { std::cout << s << std::endl; };
  std::uint64_t value = 0;
  int part1 = 0;
  for (auto input : inputs) {
    std::stringstream sig_stream(input.first);
    std::array<std::string, 10> as;
    as.fill("");
    std::vector<std::string> signals;
    while (sig_stream.good()) {
      std::string next;
      sig_stream >> next;
      signals.push_back(next);
    }

    while (std::count(as.begin(), as.end(), "") > 0) {
      for (auto next : signals) {
        if (next.size() == 2) {
          as[1] = next;
        } else if (next.size() == 3) {
          as[7] = next;
        } else if (next.size() == 4) {
          as[4] = next;
        } else if (next.size() == 7) {
          as[8] = next;
        } else if (next.size() == 6 && (same_elements(next, as[4]))) {
          as[9] = next;
        } else if (next.size() == 6 &&
                   (same_elements(next, as[1]) || same_elements(next, as[7]))) {

          as[0] = next;
        } else if (next.size() == 6) {
          as[6] = next;
        } else if (next.size() == 5 && same_elements(next, as[1])) {
          as[3] = next;
        } else if (next.size() == 5 && difference(next, as[9]).size() == 0) {

          as[5] = next;
        } else if (next.size() == 5 && difference(next, as[9]).size() == 1) {
          as[2] = next;
        }
      }
    }

    std::stringstream outputs(input.second);
    std::string val;
    while (outputs.good()) {

      std::string output;

      outputs >> output;
      if (output.size() == 2 || output.size() == 3 || output.size() == 4 ||
          output.size() == 7) {
        part1++;
      }
      for (int i = 0; i < as.size(); i++) {
        if (output.size() == as[i].size() && same_elements(output, as[i]))
          val.append(std::to_string(i));
      }
    }

    value += std::stoi(val);
  }
  std::cout << "Day 8 => Part 1: " << part1 << " - Part 2: " << value
            << std::endl;
}
