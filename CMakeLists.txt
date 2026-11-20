cmake_minimum_required(VERSION 3.25)

project(
    fparse
    VERSION 0.1.0
    DESCRIPTION "Fortran library for parsing simple block-based input files"
    LANGUAGES Fortran
)

option(FPARSE_BUILD_EXAMPLES "Build the fparse example program" ON)

add_library(fparse STATIC)
add_library(fparse::fparse ALIAS fparse)

set_target_properties(
    fparse
    PROPERTIES
        Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod"
)

include(FetchContent)

FetchContent_Declare(
    futils
    GIT_REPOSITORY git@github.com:itp-curtin/futils.git
    GIT_TAG main
)
FetchContent_MakeAvailable(futils)

target_link_libraries(fparse PUBLIC futils::futils)

target_include_directories(
    fparse
    PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>"
)

target_sources(
    fparse
    PRIVATE
        src/fparse_util.f90
        src/fparse_types.f90
        src/fparse_io.f90
        src/fparse_schema.f90
        src/fparse_getters.f90
        src/fparse_parser.f90
        src/fparse.f90
)

if(FPARSE_BUILD_EXAMPLES)
    add_executable(fparse_example examples/example_main.f90)
    target_link_libraries(fparse_example PRIVATE fparse::fparse)
endif()
