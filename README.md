# Simple Pascal LLVM Compiler Frontend

## What does it do ?
This project takes source code in Pascal like language and compiles it to LLVM IR.
The outputs are: LLVM IR of the given source code (.ir) and executable for the given source code (.out)

Some examples of what this Pascal like language looks like are in the samples folder.

# Building/Makefile

- Just run `make` and the project will build.
- Run `make test` to test every source code from the samples folder.
- Run `make clean` to get the project to its original state.

# Running
- `./spc <source file> [-o output.out]` will compile the code from the given source file
- `./runtests` or `make test` will compile every file in the samples folder

# What can it compile ?
- basic program structure: main function, input/output of integers, global constants and variables, expressions, assignments
- hexadecimal and octal values in constants and expressions
- if, while and break statement
- for (to and downto) and break statement
- inner blocks
- static arrays (even multidimensional)
- procedures and functions with local constants and variables, exit statement
- parameters of procedures and functions
- recursion
- indirect recursion
- string expressions (for writing)
- see the samples folder for proof

# Project structure

- `CMakeLists.txt` - CMake source file
- `main.cpp` - main function definition
- `Lexer.h`, `Lexer.cpp` - Lexer related sources
- `Parser.h`, `Parser.cpp` - Parser related sources
- `AST.h`, `AST.cpp` - llvm ir generation related sources
- `fce.c`  - grue for `write`, `writeln`, `read`, `writeS`, `writelnS` functions, it is compiled together with the program
- `samples` - directory with samples describing syntax
- `mila` - wrapper script for the compiler
- `runtests` - test script that compiles all samples

## Dependencies

LLVM including headers. Based on your OS distribution, it would be usually packages like:
`llvm`, `llvm-dev`.

### LLVM version

Recommended version is version 11. Older version may require changes.

