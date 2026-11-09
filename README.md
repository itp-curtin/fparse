# fparse

`fparse` is a Fortran library for parsing a simple block-based input file
format. I created it for my own projects and decided to share it in case others
find it useful.

## Input file schema
It is inspired by well-known formats/schemas like toml and yaml. However,
ultimately I designed the format in a way I liked and thought would be easy to
use.

Below is an example of the format:

```
# comment lines start with #
BLOCK_NAME
[key] value
[another_key] value1 value2 \
              value3 value4
END

@include other_input.in
```

- Blocks are started by a line containing the block name (case-insensitive).
- Blocks end with a line containing `end` (case-insensitive).
- Keys are written as `[key] value` and are case-insensitive.
- `#` starts a comment anywhere on a line.
- A trailing `\` joins the next line to the current value.
- `@include filename` inserts another file at that location.
- Order of blocks or includes does not matter.
- Keys belonging to a block must be between the block name and the `end`
  statement.

## Integration as a git submodule
It is recommended to include this repo as a git submodule in your project. This
way you can easily pull updates to fparse while keeping it organised within
your project structure.

Some recommended layout in your target project:

```
my_project/
    src/
        fparse/          # this repo as a submodule
```

add the submodule:

```
git submodule add <fparse_repo_url> src/fparse
```

or it can be placed in an `external` directory:

```
my_project/
    external/
        fparse/          # this repo as a submodule
```

where the submodule is added with:

```
git submodule add <fparse_repo_url> external/fparse
```
You will need to adjust your build system to ensure `fparse` is compiled and
linked with your project, and that the module files are in the search path.

## CMake integration

`fparse` now provides a native CMake target, so projects can consume it either
as a checked-out submodule or via `FetchContent`.

### Using a local checkout / submodule

If your project contains `fparse` at `external/fparse`, a typical integration is:

```cmake
add_subdirectory(external/fparse)
target_link_libraries(my_app PRIVATE fparse::fparse)
```

This lets CMake handle the build order and the Fortran module include path.

### Using FetchContent

If you want to fetch `fparse` automatically when it is missing:

```cmake
include(FetchContent)

FetchContent_Declare(
    fparse
    GIT_REPOSITORY https://github.com/nantonio1/fparse.git
    GIT_TAG <fixed-commit>
)

FetchContent_MakeAvailable(fparse)
target_link_libraries(my_app PRIVATE fparse::fparse)
```

Using some fixed commit is recommended for reproducible builds.

### Building the example

By default the example program is built. To disable it:

```bash
cmake -S . -B build -DFPARSE_BUILD_EXAMPLES=OFF
```

## How to use in your fortran project
Below is a high-level overview of how to use `fparse` in your Fortran project:

1. Define a schema in your code using `init_schema`, `add_block`, and `add_key`.
2. Parse an input file with `parse_input`.
3. Read values using typed getters like `get_integer`, `get_real`, or lists.

There is an example program in `examples/example_main.f90` that demonstrates
these steps. However, below is a minimal example showing how to define a schema,
parse an input file, and read an integer value:

```fortran
program main
    use fparse
    implicit none
    type(schema_type) :: schema
    type(input_tree_type) :: tree
    integer :: nelecs

    call init_schema(schema)
    call add_block(schema, 'TARGET', .true.)
    call add_key(schema, 'TARGET', 'nelecs', .true., .false., '')

    call parse_input(schema, 'data.in', tree, 6)
    call get_integer(schema, tree, 'TARGET', 'nelecs', nelecs, 6)

end program main
```

For example, the input file `data.in` would look like for this schema:

```
# some comment
TARGET
[nelecs] 10
END
```

## Include files

Use `@include other.in` to include another input file (must still follow the
same format). The included path is relative to the main input file (data.in in
the example).

## Required vs optional keys and defaults
You are able to specify whether a key is required or optional, and if optional,
whether it has a default value or not. This is done when defining the schema
with `add_key` subroutine.

Each key derived type in the schema has three properties that control how
missing input is handled:

- `required`: if true, missing input stops the program.
- `has_default`: if true and the key is missing, the default is used.
- `default_value`: a string used when `has_default` is true.

Example:

```fortran
call add_key(schema, 'OPTIONS', 'mode', .false., .true., 'standard')
call add_key(schema, 'OPTIONS', 'flags', .false., .false., '')
```

In this example, `mode` is optional and defaults to `standard`, while `flags`
is optional and has no default. if an optional key is missing and has no
default, the getter can return a `found = .false.` flag.

## Overview of main subroutines

- schema construction: `init_schema`, `add_block`, `add_key`
- parsing: `parse_input`
- getters: `get_string`, `get_integer`, `get_real`, `get_logical`
- list getters: `get_string_list`, `get_integer_list`, `get_real_list`
- presence checks: `has_block`, `has_key`
