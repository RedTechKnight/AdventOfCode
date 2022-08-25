#include "main.hpp"

void day2() {
  std::ifstream file("inputs/day2");
  int x_pos_p1 = 0;
  int x_pos_p2 = 0;
  int depth_p1 = 0;
  int depth_p2 = 0;
  int aim = 0;

  while (file.good()) {
    std::string dir = "";
    int mag = 0;
    file >> dir;
    file >> mag;

    if (dir == "forward") {
      x_pos_p1 += mag;
      x_pos_p2 += mag;
      depth_p2 += aim * mag;
    } else if (dir == "down") {
      depth_p1 += mag;
      aim += mag;
    } else if (dir == "up") {
      depth_p1 -= mag;
      aim -= mag;
    }
  }

  std::cout << "Day 2 => Part 1: " << x_pos_p1 * depth_p1
            << " - Part 2: " << x_pos_p2 * depth_p2 << std::endl;
}
