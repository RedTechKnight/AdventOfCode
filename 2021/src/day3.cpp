#include "main.hpp"
long long bit_string_to_decimal(std::vector<char> &chars);
long long bit_string_to_decimal(std::string &chars);
void day3() {
  std::ifstream file("inputs/day3", std::ios::in);
  std::vector<std::string> lines;
  std::vector<std::map<char, int>> bit_frequencies;
  std::vector<char> most_common;
  std::vector<char> least_common;
  while (file.good()) {
    std::string next_line;
    file >> next_line;
    lines.push_back(next_line);
    for (int i = 0; i < next_line.size(); i++) {
      if (bit_frequencies.size() <= i) {
        std::map<char, int> map;
        map['0'] = 0;
        map['1'] = 0;

        bit_frequencies.push_back(map);
      }
      bit_frequencies[i][next_line[i]]++;
      if (most_common.size() <= i) {
        most_common.push_back('1');
        least_common.push_back('0');
      }
      if (bit_frequencies[i]['0'] > bit_frequencies[i]['1']) {
        most_common[i] = '0';
        least_common[i] = '1';
      } else {
        most_common[i] = '1';
        least_common[i] = '0';
      }
    }
  }

  std::cout << "Day 3 => Part 1: "
            << bit_string_to_decimal(most_common) *
                   bit_string_to_decimal(least_common);
  std::vector<std::string> lines_b;
  for (auto line : lines) {
    lines_b.push_back(line);
  }
  lines.erase(std::remove_if(lines.begin(), lines.end(),
                             [most_common](std::string str) {
                               return str[0] != most_common[0];
                             }),
              lines.end());
  lines_b.erase(std::remove_if(lines_b.begin(), lines_b.end(),
                               [least_common](std::string str) {
                                 return str[0] != least_common[0];
                               }),
                lines_b.end());
  bit_frequencies.clear();
  for (int i = 0; i < most_common.size(); i++) {

    std::map<char, int> freqs;
    freqs['0'] = 0;
    freqs['1'] = 0;
    bit_frequencies.push_back(freqs);
  }
  for (int i = 1; i < most_common.size(); i++) {

    for (int j = 0; j < lines.size(); j++) {
      if (lines[j][i] == '0') {
        bit_frequencies[i]['0']++;
      } else {
        bit_frequencies[i]['1']++;
      }
      if (bit_frequencies[i]['0'] > bit_frequencies[i]['1']) {
        most_common[i] = '0';
        least_common[i] = '1';
      } else {
        most_common[i] = '1';
        least_common[i] = '0';
      }
    }
    lines.erase(std::remove_if(lines.begin(), lines.end(),
                               [i, most_common](std::string str) {
                                 return str[i] != most_common[i];
                               }),
                lines.end());
  }

  bit_frequencies.clear();
  for (int i = 0; i < most_common.size(); i++) {

    std::map<char, int> freqs;
    freqs['0'] = 0;
    freqs['1'] = 0;
    bit_frequencies.push_back(freqs);
  }
  for (int i = 1; i < least_common.size(); i++) {

    for (int j = 0; j < lines_b.size(); j++) {
      if (lines_b[j][i] == '0') {
        bit_frequencies[i]['0']++;
      } else {
        bit_frequencies[i]['1']++;
      }

      if (bit_frequencies[i]['0'] > bit_frequencies[i]['1']) {
        most_common[i] = '0';
      } else {
        most_common[i] = '1';
      }
    }
    lines_b.erase(std::remove_if(lines_b.begin(), lines_b.end(),
                                 [i, most_common](std::string str) {
                                   return str[i] == most_common[i];
                                 }),
                  lines_b.end());
    if (lines_b.size() == 1) {
      break;
    }
  }
  std::cout << "- Part 2: "
            << bit_string_to_decimal(lines[0]) *
                   bit_string_to_decimal(lines_b[0])
            << std::endl;
}

long long bit_string_to_decimal(std::vector<char> &chars) {
  long long result = 0;
  for (int i = 0; i < chars.size(); i++) {
    if (chars[i] == '1') {
      result += (long long)std::pow(2, chars.size() - i - 1);
    }
  }
  return result;
}

long long bit_string_to_decimal(std::string &chars) {
  long long result = 0;
  for (int i = 0; i < chars.size(); i++) {
    if (chars[i] == '1') {
      result += (long long)std::pow(2, chars.size() - i - 1);
    }
  }
  return result;
}
