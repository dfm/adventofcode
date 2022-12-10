cmake_minimum_required(VERSION 3.18)
project(AdventOfCode VERSION 0.2022.0 LANGUAGES CXX)
include(CTest)

option(AOC_BUILD_TESTS "Enable building tests" ON)
option(AOC_ANIMATE "Enable animations" OFF)

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
  lexy
  GIT_REPOSITORY https://github.com/foonathan/lexy.git
  GIT_TAG        5b7095dab03041b4daf7c6c521118645491b7062
)
FetchContent_MakeAvailable(lexy)

FetchContent_Declare(
  cpr
  GIT_REPOSITORY https://github.com/libcpr/cpr.git
  GIT_TAG        a2d35a1cb9f3f7e2f1469d6a189751331dc99f96
)
FetchContent_MakeAvailable(cpr)

FetchContent_Declare(
  cli11
  GIT_REPOSITORY https://github.com/CLIUtils/CLI11.git
  GIT_TAG        c2ea58c7f9bb2a1da2d3d7f5b462121ac6a07f16
)
FetchContent_MakeAvailable(cli11)

FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG        a33701196adfad74917046096bf5a2aa0ab0bb50
)
FetchContent_MakeAvailable(fmt)

FetchContent_Declare(
  inja
  GIT_REPOSITORY https://github.com/pantor/inja.git
  GIT_TAG        2d515078c647457436556763aca8d4bf7d11d5e8
)
FetchContent_MakeAvailable(inja)

#
# Library
#
include(CheckCXXCompilerFlag)
check_cxx_compiler_flag(-fconcepts HAS_CONCEPTS)

set(
  AOC_SOURCE
  src/aoc/registry.cpp
  src/aoc/remote.cpp
  src/aoc/runner.cpp)

add_library(aoclib STATIC ${AOC_SOURCE})
target_link_libraries(aoclib PUBLIC cpr::cpr foonathan::lexy::core foonathan::lexy::file)
target_include_directories(aoclib PRIVATE ${cpr_SOURCE_DIR}/include)
target_compile_options(aoclib PUBLIC -O3 -Wall -pedantic -Wextra -Werror)
if(HAS_CONCEPTS)
  target_compile_options(aoclib PUBLIC -fconcepts)
endif()

#
# CLI
#
file(
  GLOB DAYS_SOURCE
  CONFIGURE_DEPENDS
  "src/*/day*.cpp")
message(STATUS "Found days: ${DAYS_SOURCE}")

add_executable(aoc src/aoc/cli.cpp ${DAYS_SOURCE})
target_link_libraries(aoc PRIVATE aoclib CLI11::CLI11 fmt::fmt)
target_compile_definitions(aoc PRIVATE DOCTEST_CONFIG_DISABLE)
target_include_directories(aoc PRIVATE 
  ${doctest_SOURCE_DIR}/doctest
  ${inja_SOURCE_DIR}/single_include
  ${inja_SOURCE_DIR}/third_party/include
  ${cli11_SOURCE_DIR}/include)

if(AOC_ANIMATE)
  target_compile_definitions(aoc PRIVATE AOC_ANIMATE)
endif()

if(AOC_BUILD_TESTS)
  message(STATUS "Building tests")

  file(
    GLOB TESTS_SOURCE
    CONFIGURE_DEPENDS
    "tests/*.cpp")
  message(STATUS "Found tests: ${TESTS_SOURCE}")

  # Tests for each day
  add_executable(aoctest src/tests.cpp ${TESTS_SOURCE} ${DAYS_SOURCE})
  target_link_libraries(aoctest PRIVATE aoclib doctest::doctest)
  target_include_directories(aoctest PRIVATE ${doctest_SOURCE_DIR}/doctest)
  add_test(aoctest aoctest)
endif()
