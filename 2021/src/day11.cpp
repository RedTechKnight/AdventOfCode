#include "main.hpp"
struct Octopus {
  int x, y;
  int energy;
  bool flashed;
};

std::vector<Octopus> adjacent(int width, int height, int x, int y,
                              std::vector<Octopus> &octopii) {
  std::vector<Octopus> adjacent_octo;
  for (int i = -1; i < 2; i++) {
    for (int j = -1; j < 2; j++) {
      if (i == 0 && j == 0) {
        continue;
      }
      if ((y + j) < height && (y + j) >= 0 && (x + i) < width && (x + i) >= 0) {
        adjacent_octo.push_back(octopii[width * (y + j) + (x + i)]);
      }
    }
  }
  return adjacent_octo;
}

int step(int width, int height, std::vector<Octopus> &octopii) {
  for (auto &octo : octopii) {
    octo.energy += 1;
  }
  int flashes = 0;
  while (std::count_if(octopii.begin(), octopii.end(),
                       [](Octopus octo) -> bool { return octo.energy > 9; }) >
         0) {
    for (auto &octo : octopii) {
      if (octo.energy > 9) {
        octo.energy = 0;
        octo.flashed = true;
        flashes++;
        auto adj = adjacent(width, height, octo.x, octo.y, octopii);
        for (auto adjacent_octo : adj) {
          if (!adjacent_octo.flashed) {
            octopii[width * adjacent_octo.y + adjacent_octo.x].energy += 1;
          }
        }
      }
    }
  }
  for (auto &octo : octopii) {
    octo.flashed = false;
  }
  return flashes;
}

void day11() {
  std::ifstream file("inputs/day11");
  std::vector<Octopus> octopii;
  int width = 0, height = 0;
  while (file.good()) {
    std::string row;
    file >> row;
    if (row.size() > 0) {
      width = row.size();
      height++;
      for (int i = 0; i < row.size(); i++) {
        Octopus octo;
        octo.x = i;
        octo.y = height - 1;
        octo.energy = std::stoi(row.substr(i, 1));

        octo.flashed = false;
        octopii.push_back(octo);
      }
    }
  }

  int flashes = 0;
  int steps = 0;
  int sync = 0;
  while (true) {
    int flash = step(width, height, octopii);
    if (flash == 100) {
      sync = steps;
      break;
    }
    if (steps < 100) {
      flashes += flash;
    }

    steps++;
  }
  std::cout << "Day 11 => Part 1: " << flashes << " - Part 2: " << steps + 1
            << std::endl;
}
