#include "main.hpp"

void day7() {
  std::ifstream file("inputs/day7");
  std::string input;
  file >> input;
  std::replace(input.begin(), input.end(), ',', ' ');
  std::vector<int> positions;

  std::stringstream str(input);
  while (str.good()) {
    int pos = 0;
    str >> pos;
    positions.push_back(pos);
  }

  int min_cost = 1000000;
  long long min_cost_p2 = 1000009090909000;
  int min_pos = 0;
  int min_pos_p2 = 0;

  for (int target_pos = *std::min_element(positions.begin(), positions.end());
       target_pos <= *std::max_element(positions.begin(), positions.end());
       target_pos++) {
    long long fuel = 0;
    long long fuel_p2 = 0;
    for (auto pos : positions) {
      int dist = std::abs(target_pos - pos);
      fuel += std::abs(target_pos - pos);
      fuel_p2 += (dist * (dist + 1)) / 2;
    }
    if (min_cost > fuel) {
      min_cost = fuel;
      min_pos = target_pos;
    }
    if (min_cost_p2 > fuel_p2) {
      min_cost_p2 = fuel_p2;
      min_pos_p2 = target_pos;
    }
  }
  std::cout << "Day 7 => Part 1: " << min_cost << " Part 2: " << min_cost_p2
            << std::endl;
}
