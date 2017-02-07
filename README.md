# vcs-clojure
An experiment in diffing clojure code

## Building
This project uses stack, follow the usual steps to build and run the program.

## Running
The program exepects an input file as argument, if an ouput file is specified the parse result will be pretty printed to it, if none is passed it will be printed to stdout.
After the parser runs it tries to reparse the result of pretty printing the AST and reports if this successful.
