cmake_minimum_required(VERSION 3.5)

# Don't build in-tree
if (CMAKE_BINARY_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    message(FATAL_ERROR "Building in-source is not supported! Create a build dir and remove ${CMAKE_SOURCE_DIR}/CMakeCache.txt")
endif()

project(Advent VERSION 0.1.0 LANGUAGES CXX)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Default to Release build type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Type of build" FORCE)
endif()
message(STATUS "Build-type: ${CMAKE_BUILD_TYPE}")

file(GLOB_RECURSE all_days RELATIVE ${CMAKE_CURRENT_LIST_DIR} day*/*.cpp)
foreach(day ${all_days})
  get_filename_component(day_name ${day} NAME_WE)
  get_filename_component(day_dir ${day} DIRECTORY)
  add_executable(${day_name} ${day})
  target_compile_options(${day_name} PUBLIC -fPIC -std=c++17)
  include_directories(${day_dir})
  set_property(TARGET ${day_name} PROPERTY RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
endforeach()
