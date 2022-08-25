#include "main.hpp"

struct point {
  int x, y = 0;
};

struct line {
  point a, b;
};

void day5() {
  std::ifstream file("inputs/day5");
  std::vector<line> lines;
  point max;
  while (file.good()) {
    std::string next = "";
    std::getline(file, next);

    if (next.empty()) {
      continue;
    }
    std::replace(next.begin(), next.end(), ',', ' ');
    std::replace(next.begin(), next.end(), '-', ' ');
    std::replace(next.begin(), next.end(), '>', ' ');

    std::stringstream stream(next);
    line l;

    stream >> l.a.x;
    stream >> l.a.y;
    stream >> l.b.x;
    stream >> l.b.y;

    max.x = std::max(max.x, std::max(l.a.x, l.b.x));
    max.y = std::max(max.y, std::max(l.a.y, l.b.y));
    lines.push_back(l);
  }

  std::map<int, int> squares;
  std::map<int, int> squares_p2;
  auto move_closer = [](int a, int b) -> int {
    if (a < b) {
      return a + 1;
    } else if (a > b) {
      return a - 1;
    }
    return a;
  };
  
  for (const auto &line : lines) {
    if (line.a.x == line.b.x) {
      for (int i = std::min(line.a.y, line.b.y);
           i <= std::max(line.a.y, line.b.y); i++) {

        squares.insert({line.a.x + (max.x + 1) * i, 0});
        squares[line.a.x + (max.x + 1) * i]++;

        squares_p2.insert({line.a.x + (max.x + 1) * i, 0});
        squares_p2[line.a.x + (max.x + 1) * i]++;
      }
    } else if (line.a.y == line.b.y) {
      for (int i = std::min(line.a.x, line.b.x);
           i <= std::max(line.a.x, line.b.x); i++) {

        squares.insert({(max.x + 1) * line.a.y + i, 0});
        squares[(max.x + 1) * line.a.y + i]++;

        squares_p2.insert({(max.x + 1) * line.a.y + i, 0});
        squares_p2[(max.x + 1) * line.a.y + i]++;
      }
    } else if (std::abs(line.a.x - line.b.x) == std::abs(line.a.y - line.b.y)) {
      point start = line.a;
      for (int i = 0; i <= std::abs(line.a.x - line.b.x); i++) {
        squares_p2.insert({(max.x + 1) * start.y + start.x, 0});
        squares_p2[(max.x + 1) * start.y + start.x]++;
        start.x = move_closer(start.x, line.b.x);
        start.y = move_closer(start.y, line.b.y);
      }
    }
  }
  int part1 = std::count_if(squares.begin(), squares.end(),
                            [](auto kv) { return kv.second > 1; });
  int part2 = std::count_if(squares_p2.begin(), squares_p2.end(),
                            [](auto kv) { return kv.second > 1; });
  std::cout << "Day 5 => Part 1: " << part1 << " - Part 2: " << part2
            << std::endl;
}
