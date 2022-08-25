#include "main.hpp"

std::map<char, std::uint64_t>
produces(std::map<std::pair<char, char>, char> &rules,
         std::pair<char, char> seq, int iterations,
         std::map<std::pair<std::pair<char, char>, int>,
                  std::map<char, std::uint64_t>> &memo) {
  std::map<char, std::uint64_t> result;
  if (iterations == 1) {
    std::map<char, std::uint64_t> prod;
    prod[rules[seq]]++;
    memo[std::pair(seq, 1)] = prod;
    return prod;
  }
  auto iter = memo.find(std::pair(seq, iterations));
  if (iter == memo.end()) {
    auto res = rules[seq];
    result[res]++;
    for (auto product :
         produces(rules, std::pair(seq.first, res), iterations - 1, memo)) {
      result[product.first] += product.second;
    }
    for (auto product :
         produces(rules, std::pair(res, seq.second), iterations - 1, memo)) {
      result[product.first] += product.second;
    }
    memo[std::pair(seq, iterations)] = result;
  } else {
    result = iter->second;
  }
  return result;
}

void day14() {
  std::ifstream file("inputs/day14");
  std::string polymer;
  file >> polymer;
  std::map<std::pair<char, char>, char> rules;

  while (file.good()) {
    std::string line;
    std::getline(file, line);
    if (!line.empty()) {
      std::replace(line.begin(), line.end(), '-', ' ');
      std::replace(line.begin(), line.end(), '>', ' ');

      std::stringstream stream(line);
      std::string lhs, rhs;
      stream >> lhs;
      stream >> rhs;
      rules[std::pair(lhs[0], lhs[1])] = rhs[0];
    }
  }

  std::map<char, std::uint64_t> frequencies;
  std::map<std::pair<std::pair<char, char>, int>, std::map<char, std::uint64_t>>
      memo;

  for (int i = 0; i < polymer.size() - 1; i++) {
    auto prod =
        produces(rules, std::pair(polymer[i], polymer[i + 1]), 10, memo);

    for (auto product : prod) {
      frequencies[product.first] += product.second;
    }
    frequencies[polymer[i]]++;
    frequencies[polymer[i + 1]]++;
  }
  auto [min, max] =
      std::minmax_element(frequencies.begin(), frequencies.end(),
                          [](auto a, auto b) { return a.second < b.second; });
  std::uint64_t min_p1 = min->second;
  std::uint64_t max_p1 = max->second;
  frequencies.clear();
  for (int i = 0; i < polymer.size() - 1; i++) {
    auto prod =
        produces(rules, std::pair(polymer[i], polymer[i + 1]), 40, memo);

    for (auto product : prod) {
      frequencies[product.first] += product.second;
    }
    frequencies[polymer[i]]++;
    frequencies[polymer[i + 1]]++;
  }

  auto [min_p2, max_p2] =
      std::minmax_element(frequencies.begin(), frequencies.end(),
                          [](auto a, auto b) { return a.second < b.second; });
  std::cout << "Day 14 => Part 1: " << max_p1 - min_p1
            << " - Part 2: " << max_p2->second - min_p2->second << std::endl;
}
