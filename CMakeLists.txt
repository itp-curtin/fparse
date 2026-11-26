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

if(NOT TARGET futils::futils)
    include(FetchContent)
    set(FUTILS_BUILD_EXAMPLES OFF CACHE BOOL "Build futils examples")
    FetchContent_Declare(
        futils
        GIT_REPOSITORY https://github.com/itp-curtin/futils.git
        GIT_TAG main
    )
    FetchContent_MakeAvailable(futils)
endif()

target_link_libraries(fparse PUBLIC futils::futils)

target_include_directories(
    fparse
    PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>"
)

target_sources(
    fparse
    PRIVATE
        src/fparse_tools.f90
        src/file_reader.f90
        src/key_def.f90
        src/block_def.f90
        src/schema.f90
        src/block_data.f90
        src/input.f90
        src/fparse.f90
)

if(FPARSE_BUILD_EXAMPLES)
    add_executable(fparse_example examples/example_main.f90)
    target_link_libraries(fparse_example PRIVATE fparse::fparse)
endif()
