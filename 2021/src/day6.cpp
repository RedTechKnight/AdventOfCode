#include "main.hpp"
struct FishGroup {
  long long spawn_counter;
  long long number;
};
void simulate(std::vector<FishGroup> &fish) {
  long long to_make = 0;
  for (auto f : fish) {
    if (f.spawn_counter == 0) {
      to_make += f.number;
    }
  }
  bool absent = true;
  for (auto &f : fish) {
    if (f.spawn_counter == 0) {
      f.spawn_counter = 6;
    } else {
      f.spawn_counter--;
    }
  }
  FishGroup f = FishGroup{};
  f.spawn_counter = 8;
  f.number = to_make;
  fish.push_back(f);
}

long long fish_size(std::vector<FishGroup> &fish) {
  long long count = 0;
  for (auto f : fish) {
    count += f.number;
  }
  return count;
}

void day6() {
  std::ifstream file("inputs/day6");
  std::string spawns;
  file >> spawns;
  std::replace(spawns.begin(), spawns.end(), ',', ' ');
  std::stringstream nums(spawns);
  std::vector<FishGroup> fish;
  while (nums.good()) {
    long long next = 0;
    nums >> next;
    bool present = false;
    for (auto &g : fish) {
      if (next == g.spawn_counter) {
        g.number++;
        present = true;
      }
    }
    if (!present) {
      FishGroup f = FishGroup{};
      f.spawn_counter = next;
      f.number = 1;
      fish.push_back(f);
    }
  }

  std::uint64_t part1 = 0;
  std::uint64_t part2 = 0;
  for (auto i = 0; i < 256; i++) {

    simulate(fish);
    if (i == 79) {
      part1 = fish_size(fish);
    }
  }
  part2 = fish_size(fish);
  std::cout << "Day 6 => Part 1: " << part1 << " - Part 2: " << part2
            << std::endl;
}
