cmake_minimum_required(VERSION 3.16)
project(task_dispatch LANGUAGES Fortran)
enable_testing()

add_subdirectory(deps/yaftree)

file(GLOB_RECURSE SRC_SOURCES CONFIGURE_DEPENDS
    src/*.f90
    src/*.F90
    src/*.c
)

add_library(task_dispatch_static STATIC ${SRC_SOURCES})
target_link_libraries(task_dispatch_static PUBLIC yaftree_static)

set_target_properties(task_dispatch_static PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod
)

target_include_directories(task_dispatch_static PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}/mod
)

add_executable(example_console app/console.f90)
target_link_libraries(example_console PRIVATE task_dispatch_static)

file(GLOB TEST_SOURCES CONFIGURE_DEPENDS test/*.f90 test/*.F90)

foreach(test_src ${TEST_SOURCES})
    get_filename_component(test_name ${test_src} NAME_WE)
    add_executable(${test_name} ${test_src} test/utils/assert.f90)
    target_link_libraries(${test_name} PRIVATE task_dispatch_static)
    add_test(NAME ${test_name} COMMAND ${test_name})
endforeach()
