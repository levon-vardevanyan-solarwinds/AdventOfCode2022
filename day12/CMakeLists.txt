cmake_minimum_required (VERSION 3.8)

project(BFS)

add_executable (BFS "BFS.cpp")

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET BFS PROPERTY CXX_STANDARD 20)
endif()
