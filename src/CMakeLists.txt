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

file(
  GLOB DAYS_SOURCE
  CONFIGURE_DEPENDS
  "*/day*.cpp")
message(STATUS "Found days: ${days}")

set(
  AOC_SOURCE
  aoc/registry.cpp
  aoc/remote.cpp
  aoc/runner.cpp)

add_library(aoclib STATIC ${AOC_SOURCE})
target_link_libraries(aoclib PUBLIC cpr::cpr foonathan::lexy::core)
target_include_directories(aoclib PRIVATE ${cpr_SOURCE_DIR}/include)
target_compile_options(aoclib PRIVATE -O3 -Wall -pedantic -Wextra -Werror)

add_executable(aoc aoc/cli.cpp ${DAYS_SOURCE})
target_link_libraries(aoc PRIVATE aoclib CLI11::CLI11 fmt::fmt)
target_compile_definitions(aoc PRIVATE DOCTEST_CONFIG_DISABLE)
target_include_directories(aoc PRIVATE ${doctest_SOURCE_DIR}/doctest)
target_include_directories(aoc PRIVATE ${inja_SOURCE_DIR}/single_include)
target_include_directories(aoc PRIVATE ${inja_SOURCE_DIR}/third_party/include)
target_include_directories(aoc PRIVATE ${cli11_SOURCE_DIR}/include)
target_compile_options(aoc PRIVATE -O3 -Wall -pedantic -Wextra -Werror)

if(AOC_BUILD_TESTS)
  add_executable(tests tests.cpp ${DAYS_SOURCE})
  target_link_libraries(tests PRIVATE aoclib doctest::doctest)
  target_include_directories(tests PRIVATE ${doctest_SOURCE_DIR}/doctest)
  target_compile_options(tests PRIVATE -O3 -Wall -pedantic -Wextra -Werror)
  add_test(tests tests)
endif()
