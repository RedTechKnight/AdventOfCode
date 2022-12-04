#include "main.hpp"
#include <fstream>
#include <locale>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
struct SnailNum {
  std::variant<int,
               std::pair<std::unique_ptr<SnailNum>, std::unique_ptr<SnailNum>>>
      num;
};

int magnitude(std::unique_ptr<SnailNum> &snail_num) {
  if(snail_num->num.index() == 0) {
    return std::get<0>(snail_num->num);
  } else {
    return 3 * magnitude(std::get<1>(snail_num->num).first) + 2 * magnitude(std::get<1>(snail_num->num).second);
  }
}

std::unique_ptr<SnailNum> add(std::unique_ptr<SnailNum> left,
                              std::unique_ptr<SnailNum> right) {
  SnailNum s;
  s.num = std::make_pair(std::move(left), std::move(right));
  std::unique_ptr<SnailNum> result = std::make_unique<SnailNum>(std::move(s));
  return std::move(result);
}

bool split(std::unique_ptr<SnailNum> &snail_num) {
  if (snail_num->num.index() == 0 && std::get<0>(snail_num->num) > 9) {
    float val = (float)std::get<0>(snail_num->num) / 2.0f;
    SnailNum left;
    left.num = (int)std::floor(val);
    SnailNum right;
    right.num = (int)std::ceil(val);
    snail_num->num =
        std::make_pair(std::make_unique<SnailNum>(std::move(left)),
                       std::make_unique<SnailNum>(std::move(right)));
    return true;
  } else if (snail_num->num.index() == 1) {
    return split(std::get<1>(snail_num->num).first) ||
           split(std::get<1>(snail_num->num).second);
  }
  return false;
}

void show_snail_num(std::unique_ptr<SnailNum> &snail_num) {
  if (snail_num->num.index() == 0) {
    std::cout << std::get<0>(snail_num->num);
  } else {
    std::cout << "[";
    show_snail_num(std::get<1>(snail_num->num).first);
    std::cout << ",";
    show_snail_num(std::get<1>(snail_num->num).second);
    std::cout << "]";
  }
}

void add_to_left(std::unique_ptr<SnailNum> &snail_num, std::string path,
                 int to_add) {
  if (path.empty()) {
    if (snail_num->num.index() == 1) {
      add_to_left(std::get<1>(snail_num->num).second, "", to_add);
    } else {
      std::get<0>(snail_num->num) += to_add;
    }
  } else {
    if (path[0] == 'l') {
      add_to_left(std::get<1>(snail_num->num).first, path.substr(1), to_add);
    } else if (path[0] == 'r') {
      add_to_left(std::get<1>(snail_num->num).second, path.substr(1), to_add);
    }
  }
}

void add_to_right(std::unique_ptr<SnailNum> &snail_num, std::string path,
                  int to_add) {
  if (path.empty()) {
    if (snail_num->num.index() == 1) {
      add_to_right(std::get<1>(snail_num->num).first, "", to_add);
    } else {
      std::get<0>(snail_num->num) += to_add;
    }
  } else {
    if (path[0] == 'l') {
      add_to_right(std::get<1>(snail_num->num).first, path.substr(1), to_add);
    } else if (path[0] == 'r') {
      add_to_right(std::get<1>(snail_num->num).second, path.substr(1), to_add);
    }
  }
}

std::string to_explode(std::unique_ptr<SnailNum> &snail_num, std::string path) {
  if (path.length() > 3) {
    return path;
  }
  std::string right_path = path;
  path.push_back('l');
  right_path.push_back('r');
  if (snail_num->num.index() == 1 &&
      std::get<1>(snail_num->num).first->num.index() == 1) {
    path = to_explode(std::get<1>(snail_num->num).first, path);
  }
  if (snail_num->num.index() == 1 &&
      std::get<1>(snail_num->num).second->num.index() == 1) {
    right_path = to_explode(std::get<1>(snail_num->num).second, right_path);
  }
  if (snail_num->num.index() == 0) {
    return "";
  }
  if (snail_num->num.index() == 1 &&
      std::get<1>(snail_num->num).first->num.index() == 0) {
    path = "";
  }
  if (snail_num->num.index() == 1 &&
      std::get<1>(snail_num->num).second->num.index() == 0) {
    right_path = "";
  }
  if (path.empty()) {
    return right_path;
  }
  return path;
}

std::string on_left(std::string path) {
  for (int i = path.size() - 1; i >= 0; i--) {
    if (path[i] == 'r') {
      path[i] = 'l';
      return path.substr(0, i + 1);
    }
  }
  return "";
}

std::string on_right(std::string path) {
  for (int i = path.size() - 1; i >= 0; i--) {
    if (path[i] == 'l') {
      path[i] = 'r';
      return path.substr(0, i + 1);
    }
  }
  return "";
}

std::pair<int, int> replace(std::unique_ptr<SnailNum> &snail_num,
                            std::string path) {
  if (path.empty()) {
    std::pair<int, int> result;
    result.first = std::get<0>(std::get<1>(snail_num->num).first->num);
    result.second = std::get<0>(std::get<1>(snail_num->num).second->num);
    snail_num->num = 0;
    return result;
  } else {
    if (path[0] == 'l') {
      return replace(std::get<1>(snail_num->num).first, path.substr(1));
    } else {
      return replace(std::get<1>(snail_num->num).second, path.substr(1));
    }
  }
  return std::make_pair(-1, -1);
}

bool explode(std::unique_ptr<SnailNum> &snail_num) {
  auto pending = to_explode(snail_num, "");
  if (pending == "") {
    return false;
  }
  auto values = replace(snail_num, pending);
  if (values.first >= 0) {

    auto left = on_left(pending);
    if (!left.empty()) {
      add_to_left(snail_num, left, values.first);
    }
    auto right = on_right(pending);
    if (!right.empty()) {
      add_to_right(snail_num, right, values.second);
    }
  }
  return true;
}

void reduce(std::unique_ptr<SnailNum> &snail_num) {
  auto exploded = explode(snail_num);
  if (!exploded) {
    auto has_split = split(snail_num);
    if (!has_split) {
      return;
    }
  }
  reduce(snail_num);
}

std::unique_ptr<SnailNum> parse_snail_num(const std::string &input,
                                          int &index) {
  SnailNum s;
  s.num = 0;
  std::unique_ptr<SnailNum> snail_num =
      std::make_unique<SnailNum>(std::move(s));
  if (input.empty()) {
    return nullptr;
  } else if (input[index] == '[') {
    index++;
    auto left = parse_snail_num(input, index);
    if (input[index] != ',') {
      return nullptr;
    }
    index++;
    auto right = parse_snail_num(input, index);
    snail_num->num = std::pair(std::move(left), std::move(right));
  } else if (std::isdigit(input[index])) {
    std::string num;
    num.push_back(input[index]);
    std::stringstream stream(num);
    int val = 0;
    stream >> val;
    snail_num->num = val;
    index++;
    return std::move(snail_num);
  } else {
    return nullptr;
  }

  if (input[index] == ']') {
    index++;
    return std::move(snail_num);
  } else {
    return nullptr;
  }
};
void day18() {
  std::ifstream input("samples/day18");
  std::string line = "";
  input >> line;

  int index = 0;
  auto sum = parse_snail_num(line, index);
  while(input.good()) {
    input >> line;
    index = 0;
    auto next = parse_snail_num(line, index);
    sum = add(std::move(sum),std::move(next));
    reduce(sum);
  }
  reduce(sum);
  show_snail_num(sum);
  std::cout << std::endl;
  std::cout << magnitude(sum) << std::endl;
  return;
}