#include "main.hpp"

struct square {
  int num;
  bool marked;
};

struct board {
  std::array<square, 25> cells;
  bool won = false;
};

bool win(const board &b) {
  auto row = [](const board &b, int index) -> bool {
    std::array<bool, 5> res{
        b.cells[index * 5].marked, b.cells[index * 5 + 1].marked,
        b.cells[index * 5 + 2].marked, b.cells[index * 5 + 3].marked,
        b.cells[index * 5 + 4].marked};
    return std::all_of(res.begin(), res.end(),
                       [](bool b) { return b == true; });
  };
  auto col = [](const board &b, int index) -> bool {
    std::array<bool, 5> res{b.cells[index].marked, b.cells[index + 5].marked,
                            b.cells[index + 10].marked,
                            b.cells[index + 15].marked,
                            b.cells[index + 20].marked};
    return std::all_of(res.begin(), res.end(),
                       [](bool b) { return b == true; });
  };
  std::vector<bool> wins;
  for (int i = 0; i < 5; i++) {
    wins.push_back(row(b, i));
    wins.push_back(col(b, i));
  }
  return std::any_of(wins.begin(), wins.end(),
                     [](bool b) { return b == true; });
}

void mark(board &b, int num) {
  for (auto &c : b.cells) {
    if (c.num == num) {
      c.marked = true;
    }
  }
}

int rem(const board &b) {
  int res = 0;
  for (auto c : b.cells) {
    if (!c.marked) {
      res += c.num;
    }
  }
  return res;
}

int play(std::vector<board> &boards, std::vector<int> &nums) {
  for (auto num : nums) {
    for (auto &board : boards) {
      mark(board, num);
      if (win(board)) {

        return rem(board) * num;
      }
    }
  }
  return 0;
}

int play_p2(std::vector<board> &boards, std::vector<int> &nums) {
  int last_winners = 0;
  for (auto num : nums) {
    for (auto &board : boards) {
      mark(board, num);
      if (win(board)) {
        if (last_winners == boards.size() - 1 && !board.won) {

          return rem(board) * num;
        }
        board.won = true;
      }
    }
    last_winners = std::count_if(boards.begin(), boards.end(),
                                 [](const board &b) { return win(b); });
  }
  return 0;
}

void day4() {
  std::ifstream file("inputs/day4");
  std::string line;

  file >> line;
  std::transform(line.begin(), line.end(), line.begin(),
                 [](char c) { return c == ',' ? ' ' : c; });
  std::stringstream draw(line);

  std::vector<int> nums;

  while (draw.good()) {
    int next = 0;

    draw >> next;
    nums.push_back(next);
  }

  std::vector<board> boards;
  while (file.good()) {
    int next = 0;
    board b = {};
    for (int i = 0; i < 25; i++) {
      file >> next;
      b.cells[i] = square{next, false};
    }
    boards.push_back(b);
  }
  std::cout << "Day 4 => Part 1: " << play(boards, nums)
            << " - Part 2: " << play_p2(boards, nums) << std::endl;
}
