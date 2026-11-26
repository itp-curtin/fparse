# Changelog

All notable changes to `fparse` will be documented in this file. 

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) 
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

- Massive overhaul to move towards using type-bound procedures and an object 
  oriented approach to the project.

- Instead of needing to declare an instance of a schema and a "tree" object, 
  the user only needs to declare an instance of an `input_t` object, which 
  contains the schema and the parsed data.

- We now use `futils` for case conversion and error reporting.

- Updates have been made to the example code and README to reflect this major 
  reorganisation of `fparse` and how users interact with it.

- Example code no longer has a makefile, cmake is the only way now to build the 
  project and its example moving forward.

## [0.1.0]

Initial release of `fparse`.
