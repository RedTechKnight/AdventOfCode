#include "main.hpp"
struct RouteState {
  std::string label;
  std::map<std::string, int> visited;
};

int path_count_part1(std::map<std::string, std::vector<std::string>> &edges) {
  int paths = 0;
  std::vector<RouteState> to_visit;
  to_visit.push_back(RouteState{"start", std::map<std::string, int>()});
  while (!to_visit.empty()) {

    auto current = to_visit.back();
    current.visited.emplace(current.label, 0);
    to_visit.pop_back();

    if (current.label == "end") {
      paths += 1;
      continue;
    }

    bool small_cave = current.label != "start" && current.label != "end" &&
                      std::all_of(current.label.begin(), current.label.end(),
                                  [](char c) { return std::islower(c); });

    if (small_cave && current.visited[current.label] > 0) {
      continue;
    }

    current.visited[current.label]++;

    for (auto next : edges[current.label]) {
      if (next != "start")
        to_visit.push_back(RouteState{next, current.visited});
    }
  }
  return paths;
}

int path_count_part2(std::map<std::string, std::vector<std::string>> edges) {
  int paths = 0;
  std::vector<RouteState> nodes;
  nodes.push_back(RouteState{"start", std::map<std::string, int>()});
  while (!nodes.empty()) {

    auto next = nodes.back();
    next.visited.emplace(next.label, 0);
    nodes.pop_back();

    if (next.label == "end") {
      paths += 1;
      continue;
    }

    bool small_cave = next.label != "start" && next.label != "end" &&
                      std::all_of(next.label.begin(), next.label.end(),
                                  [](char c) { return std::islower(c); });
    auto label = next.label;
    bool others_visited =
        next.label != "start" && next.label != "end" &&
        std::any_of(next.visited.begin(), next.visited.end(),
                    [label](std::pair<std::string, int> visited_node) -> bool {
                      if (visited_node.first != "start" &&
                          visited_node.first != "end" &&
                          visited_node.first != label &&
                          std::all_of(visited_node.first.begin(),
                                      visited_node.first.end(),
                                      [](char c) { return std::islower(c); })) {
                        return visited_node.second > 1;
                      }
                      return false;
                    });
    if (small_cave && next.visited[next.label] > 0) {
      if (others_visited)
        continue;
      if (next.visited[next.label] > 1)
        continue;
    }

    next.visited[next.label]++;

    for (auto dest : edges[next.label]) {
      if (dest != "start")
        nodes.push_back(RouteState{dest, next.visited});
    }
  }
  return paths;
}

void day12() {
  std::ifstream file("inputs/day12");
  std::map<std::string, std::vector<std::string>> edges;
  while (file.good()) {
    std::string edge;
    file >> edge;
    std::replace(edge.begin(), edge.end(), '-', ' ');
    std::stringstream stream(edge);
    std::string a, b;
    stream >> a;
    stream >> b;

    if (!a.empty()) {
      edges[a].push_back(b);
      edges[b].push_back(a);
    }
  }
  std::map<std::string, int> visited;

  std::cout << "Day 12 => Part 1: " << path_count_part1(edges)
            << " - Part 2: " << path_count_part2(edges) << std::endl;
}
