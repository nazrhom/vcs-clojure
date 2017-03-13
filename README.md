# vcs-clojure
An experiment in diffing clojure code

## Building
This project uses stack, follow the usual steps to build and run the program.

## Running
The program exepects an input file as argument, if an ouput file is specified the parse result will be pretty printed to it, if none is passed it will be printed to stdout.
After the parser runs it tries to reparse the result of pretty printing the AST and reports if this successful.


# Report
## Intro

## Repository Mining

## Generic Programming

## Type-directed diff

The approach in [1] takes advantage of the generic programming methods outlined above to design a type-directed diff algorithm between structured data.
The inspiration comes from the diff utility present in Unix which is at the heart of the current methodologies employed by VCS to attempt to compute a patch between two different versions of the same file.
The limitations of the diff algorithm, as it currently stands, is that it does not employ any structural information between the data it is trying to merge. Files are parsed on a line by line basis and, as shown in [1], this is somewhat resilient to vertical changes in the source code, but completely breaks down when dealing with horizontal changes, which constitute a heavy chunk of the changes that are usually made to code.

The underlying idea to the approach presented in [1] is to employ the generic SoP view presented in the preceding section to define a generic way to view datatypes. Once that is settled, we obtain a view of any well defined program as a structured tree of data, where each object we inspect can be represented as a choice of a certain constructor, among the available ones, and certain arguments to that constructor. This process is equivalent to parsing the source language into an AST, an interesting consideration to make is that the choice of AST for diffing purposes might be very different from the representation that would be chosen for a compiler of the language.
While these two structures should be "somewhat isomorphic", the amount of domain specific knowledge that should be represented in the AST is certainly not necessarily the same, one of the goals of this work is to explore this boundary and the choice of what kind of information is beneficial for calculating a patch between these two structures.

{Example diffing strings
Align strings = Spine
Move/insert = Align}
Computing a patch between two trees can be reduced to the following steps:
The first thing one can do is to compute a spine between the two trees. Calculating a spine for two trees corresponds to calculating the longest common prefix between two strings.
Recall that the two trees *x* and *y* are viewed as SoP, in this sense calculating the spine between *x* and *y* corresponds to capturing the common coproduct structure between them.
In general, we have three cases to consider.
- x = y
- x and y have the same constructor but not all the subtrees are equal
- x and y have different constructors
This gives rise to three different constructors for the Spine datatype, each corresponding to one of the cases described above.

Remember the spine tracks only equality (or partial equality) between the types, information about what changes must be carried along too. In the case that the constructor has remained the same, we can group up the pairs of arguments and proceed from there, however in the case the external constructor has changed there is no obvious way of pairing up the arguments (they might be completely in different numbers and types)

## Biblio

[1] Type-directed diff
