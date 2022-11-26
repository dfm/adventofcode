set(AOC_TESTS
  test_io
  test_runner
)

foreach(name ${AOC_TESTS})
  add_executable(${name} ${name}.cpp)
  target_link_libraries(${name} PRIVATE doctest::doctest)
  target_include_directories(${name} PRIVATE ${doctest_SOURCE_DIR}/doctest)
  target_compile_definitions(${name} PRIVATE AOC_OFFLINE_MODE)
  add_test(${name} ${name})
endforeach()
