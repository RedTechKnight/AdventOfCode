cmake_minimum_required(VERSION 3.10)

project(AOC2021)
			  
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(AOC2021 src/main.cpp src/day1.cpp src/day2.cpp src/day3.cpp src/day4.cpp src/day5.cpp src/day6.cpp src/day7.cpp src/day8.cpp src/day9.cpp src/day10.cpp src/day11.cpp src/day12.cpp src/day13.cpp src/day14.cpp src/day15.cpp src/day16.cpp)

target_include_directories(AOC2021 PUBLIC include)
