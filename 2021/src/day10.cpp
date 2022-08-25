#include "main.hpp"

std::uint64_t solve(std::string &stack, std::string &input) {
  std::map<char, char> matches;
  std::map<char, std::uint64_t> scores;
  scores[')'] = 3;
  scores['}'] = 1197;
  scores[']'] = 57;
  scores['>'] = 25137;
  matches['('] = ')';
  matches['['] = ']';
  matches['{'] = '}';
  matches['<'] = '>';
  std::uint64_t score = 0;
  while (input.size() > 0) {
    if (input[0] == '(' || input[0] == '[' || input[0] == '{' ||
        input[0] == '<') {
      stack.push_back(matches[input[0]]);
      input.erase(input.begin());
    } else if (stack.back() == input[0]) {

      stack.pop_back();
      input.erase(input.begin());
    } else {

      stack = "";
      return scores[input[0]];
    }
  }

  return 0;
}

void day10() {
  std::ifstream file("inputs/day10");
  std::map<char, std::uint64_t> scores;
  scores[')'] = 1;
  scores['}'] = 3;
  scores[']'] = 2;
  scores['>'] = 4;
  std::uint64_t score = 0;
  std::vector<std::uint64_t> autocomplete_scores;
  while (file.good()) {
    std::string line;
    file >> line;

    std::string stack = "";
    std::string reversed = "";

    score += solve(stack, line);
    std::reverse(stack.begin(), stack.end());
    std::uint64_t autocomp_score = 0;
    for (auto c : stack) {
      autocomp_score = autocomp_score * 5;
      autocomp_score += scores[c];
    }

    if (autocomp_score > 0) {
      autocomplete_scores.push_back(autocomp_score);
    }
  }
  std::sort(autocomplete_scores.begin(), autocomplete_scores.end());
  std::cout << "Day 10 => Part 1: " << score << " - Part 2: "
            << autocomplete_scores[autocomplete_scores.size() / 2] << std::endl;
}
