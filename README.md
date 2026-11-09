# fparse

`fparse` is a Fortran library for parsing a simple block-based input file format.

## Input file schema
It is inspired by well-known formats/schemas like toml and yaml. However,
ultimately I designed a format that I thought would be easy to use and look
good in a plain text file.

Below is an example of the format:

```
# comment lines start with #
BLOCK_NAME
[key] value
[another_key] value1 value2 \
              value3 value4
END

BLOCK_NAME2
[key2] value2
END

@include other_input.in
```

- Blocks are started by a line containing the block name (case-insensitive).
- Blocks end with a line containing `end` (case-insensitive).
- Keys are written as `[key] value` and are case-insensitive.
- `#` starts a comment anywhere on a line.
- A trailing `\` joins the next line to the current value.
- `@include filename` inserts another file following the input schema at that location.
- Order of blocks or includes does not matter (order can matter depending on
  the application, but fparse itself does not enforce any order).
- Keys belonging to a block must be between the block name and the `end`
  statement.

## Integration as a git submodule
`fparse` can be used by a project/application as a submodule.

Below are two examples of recommended places to place the submodule in your
project:

```
my_project/src/fparse  # this repo as a submodule
```

then add the submodule:

```
git submodule add <fparse_repo_url> src/fparse
```

or it can be placed in an `external` directory:

```
my_project/external/fparse  # this repo as a submodule
```

where the submodule is then added with:

```
git submodule add <fparse_repo_url> external/fparse
```

You will need to adjust your build system to ensure `fparse` is compiled and
linked with your project, and that the module files are in the search path. To
see how to compile and link `fparse` with your project which uses Make, see the
`examples/Makefile` in this repo. For CMake projects, see the next section.

## CMake integration

`fparse` provides a CMake target, so CMake projects can consume it either
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
    GIT_TAG <fixed-commit-or-tag-name>
)

FetchContent_MakeAvailable(fparse)
target_link_libraries(my_app PRIVATE fparse::fparse)
```

Using some fixed commit or tag is recommended for reproducible builds.

### Disabling the example program build when using CMake

By default the example program is built. To disable it:

```bash
cmake -S . -B build -DFPARSE_BUILD_EXAMPLES=OFF
```

## How to use in your fortran project
Below is a high-level overview of how to use `fparse` in your Fortran project:

1. Define a schema in your code using `init_schema`, `add_block`, and `add_key`
   subroutines.
2. Parse an input file with the `parse_input` subroutine, which will return an
   input tree custom derived type.
3. Read values using typed getters like `get_integer`, `get_real`, or lists.

There is an example program in `examples` that demonstrates
these steps. To compile and run this example, run the following commands from
within the `examples` directory:

```bash
make
./build/example.exe
```

## Include files

Use `@include other.in` to include another input file (must still follow the
same format). When using this directive, you can specify either an absolute path
to the additional *include file* or a relative path which is relative to the
input file that contains the `@include` directive.

## Required vs optional keys and defaults
You are able to specify whether a key is required or optional, and if optional,
whether it has a default value or not. This is done when defining the schema
with the `add_key` subroutine.

Each key derived type in the schema has three properties that control how
missing input is handled:

- `required`: if true, missing input stops the program.
- `has_default`: if true and the key is missing, the default is used.
- `default_value`: a string used when `has_default` is true.

Example:

```Fortran
                   ! block name, key name, required, has_default, default_value
call add_key(schema, 'OPTIONS', 'mode',    .false.,  .true.,      'standard')
call add_key(schema, 'OPTIONS', 'flags',   .false.,  .false.,     '')
```

In this example, `mode` is optional and defaults to `standard`, while `flags`
is optional and has no default.
