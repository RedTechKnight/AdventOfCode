#include "main.hpp"

std::pair<int,int> folded(std::pair<int,int> dot,char axis,int line) {
  if(axis == 'x') {
    if(dot.first > line) {
      return std::make_pair(line - (dot.first - line),dot.second);
    }
  } else if(axis == 'y') {
    if(dot.second > line) {
      return std::make_pair(dot.first,line - (dot.second - line));
    }
  }
  return dot;
}

void fold(std::map<std::pair<int,int>,bool> &dots,char axis,int line) {
  for(auto dot : dots) {
    dots[dot.first] = false;
    dots[folded(dot.first,axis,line)] = true;
  }
  for(auto it = dots.begin();it != dots.end();) {
    if(it->second == false) {
      it = dots.erase(it);
    } else {
      it++;
    }
  }
}

void day13() {
  std::ifstream file("inputs/day13");
  std::vector<std::pair<char,int>> folds;
  std::map<std::pair<int,int>,bool> dots;
  while(file.good()) {
    std::string next;
    file >> next;
    if(next == "fold") {
      file >> next;
      file >> next;
      std::replace(next.begin(),next.end(),'=',' ');
      std::stringstream stream(next);
      std::string axis,line;
      stream >> axis;
      stream >> line;
      
      folds.push_back(std::make_pair(axis[0],std::stoi(line)));
    } else if (!next.empty()){
      std::replace(next.begin(),next.end(),',',' ');
      std::stringstream stream(next);
      std::string x,y;
      stream >> x;
      stream >> y;
      
      dots[std::make_pair(std::stoi(x),std::stoi(y))] = true;
      
    }
  }
  
  fold(dots,folds[0].first,folds[0].second);
  int part1 = dots.size();
  for(int i = 1;i < folds.size();i++) {
    fold(dots,folds[i].first,folds[i].second);
  }
  
  int maxX = 0;
  int maxY = 0;
  
  for(auto dot : dots) {
    if(maxX < dot.first.first)
      maxX = dot.first.first;
    if(maxY < dot.first.second)
      maxY = dot.first.second;
  }
  std::string code = "";
  for(int i = 0;i < maxY+1;i++) {
    for(int j = 0;j < maxX+1;j++) {
      code.push_back(dots[std::make_pair(j,i)] ? '#' : '.');
    }
    code.push_back('\n');
  }
  std::cout << "Day 13 => Part 1: " << part1 << " - Part 2: \n" << code << std::endl;
}
