FetchContent_Declare(
  cli11
  GIT_REPOSITORY https://github.com/CLIUtils/CLI11.git
  GIT_TAG        c2ea58c7f9bb2a1da2d3d7f5b462121ac6a07f16
)
FetchContent_MakeAvailable(cli11)

add_executable(aoc aoc.cpp)
target_link_libraries(aoc PRIVATE cpr::cpr CLI11::CLI11)
target_include_directories(aoc PRIVATE ${cpr_SOURCE_DIR}/include)
target_include_directories(aoc PRIVATE ${cli11_SOURCE_DIR}/include)
target_compile_options(aoc PRIVATE -O3 -Wall -pedantic -Wextra -Werror)
