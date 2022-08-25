#include "main.hpp"

void day1() {
  std::ifstream file("inputs/day1");
  std::vector<int> readings;
  while (file.good()) {
    int next = 0;
    file >> next;
    readings.push_back(next);
  }

  int increases_p1 = 0;
  int increases_p2 = 0;
  for (int i = 0; i < readings.size(); i++) {
    if (i + 1 < readings.size() && readings[i] < readings[i + 1]) {
      increases_p1++;
    }
    if (i + 3 < readings.size() &&
        (readings[i] + readings[i + 1] + readings[i + 2]) <
            (readings[i + 1] + readings[i + 2] + readings[i + 3])) {
      increases_p2++;
    }
  }
  std::cout << "Day 1 => Part 1: " << increases_p1
            << " - Part 2:  " << increases_p2 << std::endl;
}
