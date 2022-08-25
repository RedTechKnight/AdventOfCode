#include "main.hpp"

struct Tile {
  int x, y = 0;
  int height = -1;
  bool marked;
};
int basin_size(std::vector<Tile> &heightmap, int width, int height, int x,
               int y) {
  if(heightmap[width * y + x].marked) {
    return 0;
  }
  heightmap[width * y + x].marked = true;
  auto adjacent = [&heightmap, width, height](int x,
                                              int y) -> std::vector<Tile> {
    
    std::vector<Tile> adjacent_tiles;
    if (x < 0 || x >= width || y < 0 || y >= height) {
      return adjacent_tiles;
    }
    if (x + 1 < width && !heightmap[width * y + x + 1].marked) {
      adjacent_tiles.push_back(heightmap[width * y + x + 1]);
    }
    if (x - 1 >= 0 && !heightmap[width * y + x - 1].marked) {
      adjacent_tiles.push_back(heightmap[width * y + x - 1]);
    }
    if (y + 1 < height && !heightmap[width * (y + 1) + x].marked) {
      adjacent_tiles.push_back(heightmap[width * (y + 1) + x]);
    }
    if (y - 1 >= 0 && !heightmap[width * (y - 1) + x].marked) {
      adjacent_tiles.push_back(heightmap[width * (y - 1) + x]);
    }
    return adjacent_tiles;
  };
  
  int res = 1;
  auto adj = adjacent(x,y);
  for(auto tile : adj) {
    
    res += basin_size(heightmap,width,height,tile.x,tile.y);
  }
  return res;
}

void day9() {
  std::ifstream file("inputs/day9");
  std::vector<int> heights;
  std::vector<Tile> heightmap;
  int width = 0;
  int height = 0;
  while (file.good()) {
    std::string row;
    file >> row;
    if (row.size() > 0) {
      width = row.size();
      height++;
      for (int i = 0; i < row.size(); i++) {
        heights.push_back(std::stoi(row.substr(i, 1)));
        Tile f{};
        f.height = std::stoi(row.substr(i, 1));
        f.marked = false;
	f.x = i;
	f.y = height-1;
        if (f.height > 8)
          f.marked = true;
        heightmap.push_back(f);
      }
    }
  }
  auto adjacent_points = [&heights, width, height](int x,
                                                   int y) -> std::vector<int> {
    std::vector<int> points;
    if (x >= width || y >= height || x < 0 || y < 0) {
      return points;
    }
    if (x + 1 < width) {
      points.push_back(heights[y * width + x + 1]);
    }
    if (x - 1 >= 0) {
      points.push_back(heights[y * width + x - 1]);
    }
    if (y + 1 < height) {
      points.push_back(heights[(y + 1) * width + x]);
    }
    if (y - 1 >= 0) {
      points.push_back(heights[(y - 1) * width + x]);
    }
    return points;
  };
  auto low_point = [&heights, width, height, adjacent_points](int x,
                                                              int y) -> int {
    if (x >= width || y >= height) {
      return -1;
    }
    for (auto adj : adjacent_points(x, y)) {
      if (adj <= heights[y * width + x]) {
        return -1;
      }
    }
    return heights[y * width + x];
  };
  int risk = 0;

  std::vector<Tile> low_points;
  for (int i = 0; i < width; i++) {
    for (int j = 0; j < height; j++) {
      auto lp = low_point(i, j);
      if (lp < 0) {
      } else {
        risk += lp + 1;
        low_points.push_back(Tile{i, j, });
      }
    }
  }

  

  std::vector<int> sizes;
  for (auto lowest : low_points) {
    int size = basin_size(heightmap,width,height,lowest.x,lowest.y);
    sizes.push_back(size);
    
  }
  std::sort(sizes.begin(),sizes.end());
  int max_a = sizes[sizes.size()-1];
  int max_b = sizes[sizes.size()-2];
  int max_c = sizes[sizes.size()-3];
  std::cout << "Day 9 => Part 1: " << risk << " - Part 2: " <<  max_a * max_b * max_c << std::endl;
}
