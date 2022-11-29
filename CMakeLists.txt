cmake_minimum_required(VERSION 3.18)
project(AdventOfCode VERSION 0.2022.0 LANGUAGES CXX)
include(CTest)

option(AOC_BUILD_TESTS "Enable building tests" ON)

# 
# C++ standard
# 
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

include_directories(include)

#
# External dependencies
#
include(FetchContent)
FetchContent_Declare(
  doctest
  GIT_REPOSITORY https://github.com/doctest/doctest.git
  GIT_TAG        b7c21ec5ceeadb4951b00396fc1e4642dd347e5f
)
FetchContent_MakeAvailable(doctest)

FetchContent_Declare(
  cpr
  GIT_REPOSITORY https://github.com/libcpr/cpr.git
  GIT_TAG        a2d35a1cb9f3f7e2f1469d6a189751331dc99f96
)
FetchContent_MakeAvailable(cpr)

FetchContent_Declare(
  lexy
  GIT_REPOSITORY https://github.com/foonathan/lexy.git
  GIT_TAG        5b7095dab03041b4daf7c6c521118645491b7062
)
FetchContent_MakeAvailable(lexy)

#
# Main interface
#
add_subdirectory(src)

#
# Unit tests
#
if(AOC_BUILD_TESTS)
  message(STATUS "Building tests")
  add_subdirectory(tests)
endif()
