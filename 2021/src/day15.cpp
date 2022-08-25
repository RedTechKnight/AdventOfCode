#include "main.hpp"

struct Cell {
  std::pair<int, int> pos;
  int risk;
  int distance;
  std::vector<std::pair<int, int>> adjacent;
};

class CellCompare {
public:
  constexpr bool operator()(const Cell &lhs, const Cell &rhs) {
    return lhs.distance > rhs.distance;
  }
};

void day15() {
  std::ifstream file("inputs/day15");
  std::map<std::pair<int, int>, Cell> grid;

  int grid_size = 0;

  auto start = std::make_pair(0, 0);

  int height = 0;
  while (file.good()) {
    std::string line;
    file >> line;
    if (line.empty()) {
      continue;
    }

    grid_size = line.size();
    for (int c = 0; c < line.size(); c++) {

      for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
          auto pos = std::make_pair(c + grid_size * i, height + grid_size * j);
          int risk = std::stoi(line.substr(c, 1));

          risk = risk + i + j;
          if (risk > 9) {
            risk -= 9;
          }

          grid[pos] = Cell{pos, risk, 9999999};
        }
      }
    }
    height++;
  }
  grid[start].risk = 0;
  grid[start].distance = 0;
  grid_size = grid_size * 5;
  auto dest_p1 = std::make_pair(grid_size / 5 - 1, grid_size / 5 - 1);
  auto dest_p2 = std::make_pair(grid_size - 1, grid_size - 1);

  for (auto &cell : grid) {

    if (cell.first.first + 1 < grid_size) {
      cell.second.adjacent.push_back(
          std::pair(cell.first.first + 1, cell.first.second));
    }
    if (cell.first.first - 1 >= 0) {
      cell.second.adjacent.push_back(
          std::pair(cell.first.first - 1, cell.first.second));
    }
    if (cell.first.second + 1 < grid_size) {
      cell.second.adjacent.push_back(
          std::pair(cell.first.first, cell.first.second + 1));
    }

    if (cell.first.second - 1 >= 0) {
      cell.second.adjacent.push_back(
          std::pair(cell.first.first, cell.first.second - 1));
    }
  }

  std::priority_queue<Cell, std::vector<Cell>, CellCompare> to_visit;
  to_visit.push(grid[start]);
  while (!to_visit.empty()) {

    auto current = to_visit.top().pos;
    to_visit.pop();

    for (auto next : grid[current].adjacent) {

      int dist = grid[current].distance + grid[next].risk;
      if (dist < grid[next].distance) {
        grid[next].distance = dist;
        to_visit.push(grid[next]);
      }
    }
  }

  int lowest_p1 = grid[dest_p1].distance;
  int lowest_p2 = grid[dest_p2].distance;

  std::cout << "Day 15 => Part 1: " << lowest_p1 << " - Part 2: " << lowest_p2
            << std::endl;
}
