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
- A trailing `\` joins the next line to the current value/input.
- `@include filename` inserts another file following the input schema at that location.
- Order of blocks or includes does not matter (order can matter depending on
  the consumer application, but fparse itself does not enforce any order).
- Keys belonging to a block must be between the block name and the `end`
  statement.

## Integration as a git submodule
`fparse` can be used by a project/application as a git submodule.

Below are two examples of recommended places to place the submodule in your
project:

```
my_project/src/fparse  # this repo as a submodule
```

then you can add the submodule using the following command:

```
git submodule add <fparse_repo_url> src/fparse
```

or it can be placed in an `external` directory like:

```
my_project/external/fparse  # this repo as a submodule
```

where the submodule is then added with:

```
git submodule add <fparse_repo_url> external/fparse
```

You will need to adjust your build system to ensure `fparse` is compiled and
linked with the project. To see how to compile and link `fparse` with your 
project which uses Make, see the `examples/Makefile` in this repo. For CMake 
projects, see the next section.

## CMake integration

`fparse` provides a CMake target, so CMake projects can consume it either
as a checked out submodule or via the `FetchContent` module.

### Using a local checkout / submodule

If a project contains `fparse` at `external/fparse` for example, a typical 
CMakeLists.txt file would contain the following lines:

```cmake
add_subdirectory(external/fparse)
target_link_libraries(my_app PRIVATE fparse::fparse)
```

This lets CMake handle the build order and the Fortran module include path.

### Using FetchContent

If you want to fetch `fparse` automatically when it is missing at configure 
time, you can use the `FetchContent` module. In this case, a typical 
CMakeLists.txt file would contain the following lines:

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

Using some fixed commit or tag is recommended for reproducibility.

By default the example program is built but can be easily disabled with the 
`FPARSE_BUILD_EXAMPLES` option:

```bash
cmake -S . -B build -DFPARSE_BUILD_EXAMPLES=OFF
```

## Overview on how to use 
Below is a high-level overview of how to use `fparse` in a Fortran project:

1. Define a schema in your code using `init_schema`, `add_block`, and `add_key`
   subroutines.
2. Parse an input file with the `parse_input` subroutine, which will return an
   input tree custom derived type.
3. Read values using typed getters like `get_integer`, `get_real`, or lists.

There is an example program in `examples` that demonstrates these steps. To 
compile and run this example, run the following commands from within the 
`examples` directory:

```bash
make
./build/example.exe
```

Ultimately something similar in structure to this example program will be 
written as a wrapper module in the consumer project.

## Include files

You can use a `@include` directive to include a seperate input file (it must 
still follow the same format). When using this directive, you can specify 
either an absolute path to the additional *include file* or a relative path 
which is relative to the input file that contains the `@include` directive. An 
example of this is shown below:

```
# example main input file
BLOCK_NAME
[key] value
END

@include debug_options.in
@include grids.in
```

## Required vs optional keys and defaults
You are able to specify whether a key is required or optional, and if optional,
whether it has a default value or not. This is done when defining the schema
with the `add_key` subroutine.

When adding a key to the schema, you can specify whether it is a required key or 
an optional key that isn't always needed to be specified in the input file. If a 
key is considered optional you can specify whether it has a default value of 
not. This is controlled by three boolean arguments that are passed to the 
`add_key` subroutine: 

- `required`: if true, missing this key will stop the program.
- `has_default`: if true and the key is missing, the default is used.
- `default_value`: a string used when `has_default` is true containing the 
  default value for the key.

Example of calling the add_key subroutine to add two keys to the schema:

```Fortran
                   ! block name, key name, required, has_default, default_value
call add_key(schema, 'OPTIONS', 'mode',    .false.,  .true.,      'standard')
call add_key(schema, 'OPTIONS', 'flags',   .false.,  .false.,     '')
```

In this example, `mode` is optional and defaults to `standard`, while `flags`
is optional and has no default.
