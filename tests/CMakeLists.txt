set(AOC_TESTS
  test_io)

foreach(name ${AOC_TESTS})
  add_executable(${name} ${name}.cpp)
  target_link_libraries(${name} PRIVATE aoclib doctest::doctest foonathan::lexy::core)
  target_include_directories(${name} PRIVATE ${doctest_SOURCE_DIR}/doctest)
  add_test(${name} ${name})
endforeach()
