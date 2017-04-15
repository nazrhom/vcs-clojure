# vcs-clojure
An experiment in diffing clojure code

## Building
This project uses stack, follow the usual steps to build and run the program.

## Running
The program expects an input file as argument, if an ouput file is specified the parse result will be pretty printed to it, if none is passed it will be printed to stdout.
After the parser runs it tries to reparse the result of pretty printing the AST and reports if this successful.


# Report
## Intro

## Repository Mining

## Dependent types in Haskell

With time, Haskell's type system has kept evolving from its humble Hindley-Miller, through the use of different language extensions, it is now possible to express much more, in particular support for dependently typed programming has been explored in the latest years.

One major stepping stone in this direction is the DataKinds extension which duplicates an ordinary data type, such as
```
data Nat = Z | S Nat
```
at the kind level, this means that from this declaration we automatically get two new types, namely `Z` of kind `Nat` and `S` of kind `Nat -> Nat`.

We can now use the `Nat` kind to index generalized algebraic data types (GADTs) in a way that allows us to create an analogue of a dependent type.
In the case of `Nat`, we can use it to define a GADT for vectors of a given length.
```
data Vec :: * -> Nat -> * where
  Vn ::                 Vec x Z
  Vc :: x -> Vec x n -> Vec x (S n)
```
Such a vector is either the empty vector, which is indexed by `Z`, or a vector which is built by concatenating an element of type `x` to a vector of `xs` of length `n`, yielding a vector of `xs` of length `S n`.

This allows us to define principled analogues of some functions which operate on lists. The infamous `head` function on lists, will return an error when passed an empty list; in our case, we can rule this out by construction.
This is how we can define a `head` function on vectors.
```
head :: Vec x (S n) -> x
head (Vc h t) = h
```

Informally, we are saying that the `head` function takes as argument a vector with length strictly greater than 0. In this way if we try to pass an empty vector to `head` we will get a compile time error instead of the usual runtime one.

Another extension which plays a crucial role in dependent types is `TypeFamilies`: informally it allows us to write functions on the type level, enabling us to write some fundamental functions for our `Vec` data type.

The following type family can be seen as a function that takes two types of kind `Nat` and returns another type of kind `Nat` representing the result of adding those two types.

```
type family (m :: Nat) :+ (n :: Nat) :: Nat where
  Z     :+ n = n
  (S m) :+ n = S (m :+ n)
```

Equipped with this type family we can now define concatenation between vectors.

```
vappend :: Vec x n -> Vec x m -> Vec x (n :+ m)
vappend Vn          ys = ys
vappend (x `Vc` xs) ys = x `Vc` (vappend xs ys)
```

One interesting thing to note is that up to this point, we never use the `Nat` part of a vector at runtime. That information is only used at compile time to check that everything "lines up" the way it should be, but could actually be erased at runtime.

However, if we want to achieve the full power of dependent types we want to be able to make some runtime decision based on the `Nat` that we learn is indexing a vector.

Suppose we want to write a `split` function, this function takes an `n` of kind `Nat`, a vector of length `n :+ m` and splits it into a pair of vectors, with respectively `n` and `m` elements.

The first problem we incur in, is that we can not pass something of kind `Nat` to our `split` function; furthermore, we want to express that this `n` of kind `Nat` that we pass as a first argument, is the same `n` as in the vector length.
The standard solution for this problem is to define what is called a singleton GADT for `Nat`.

```
data SNat :: Nat -> * where
  SZ ::           SNat Z
  SN :: SNat n -> SNat (S n)
```
The name singleton comes from the fact that each type level value of kind `Nat` (namely `Z` or `S` applied to another type of kind `Nat`) has a single representative in the `SNat` type.

The following figure gives a good representation of this process: the DataKinds extensions promotes things of type `Nat` to things of kind `Nat`. The singleton allows us to take one step back in this ladder, and associates to every thing of kind `Nat` a term from the singleton type `SNat`.
{insert Fig 1 from \cite{singletons1}}

This type solves the two problems outlined above: it has kind `*` and it contains a `Nat` that we can later refer to in our function definition.
We can now define `split` as follows:
```
split :: SNat n -> Vec x (n :+ m) -> (Vec x n, Vec x m)
split SZ     xs          = (Vn, xs)
split (Sn n) (x `Vc` xs) = (x `Vc` ys, zs)
  where
    (ys, zs) = split n xs
```

With these three tricks up our sleeve: data kind promotion, type level functions and singletons we can emulate some of the features that are present in dependently typed languages such as Agda.
This is not the full picture yet, as pointed out in [\quote{hasochism}]
there is still the distinction between explicit and implicit dependent types, currently present in Agda, that can be interpreted in Haskell.
{singletons are not completely satisfactory (maybe put proxy example) sometime we have to give them and build them explicitly, even if they are not actually needed }

## Type-directed diff

The approach in [1] takes advantage of the generic programming methods outlined above to design a type-directed diff algorithm between structured data.
The inspiration comes from the diff utility present in Unix which is at the heart of the current methodologies employed by VCS to attempt to compute a patch between two different versions of the same file.
The limitations of the diff algorithm, as it currently stands, is that it does not employ any structural information between the data it is trying to merge. Files are parsed on a line by line basis and, as shown in [1], this is somewhat resilient to vertical changes in the source code, but completely breaks down when dealing with horizontal changes, which constitute a heavy chunk of the changes that are usually made to code.

The underlying idea to the approach presented in [1] is to employ the generic SoP view presented in the preceding section to define a generic way to view datatypes. Once that is settled, we obtain a view of any well defined program as a structured tree of data, where each object we inspect can be represented as a choice of a certain constructor, among the available ones, and certain arguments to that constructor. This process is equivalent to parsing the source language into an AST, an interesting consideration to make is that the choice of AST for diffing purposes might be very different from the representation that would be chosen for a compiler of the language.
While these two structures should be "somewhat isomorphic", the amount of domain specific knowledge that should be represented in the AST is certainly not necessarily the same, one of the goals of this work is to explore this boundary and the choice of what kind of information is beneficial for calculating a patch between these two structures.

### Spine
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

The spine tracks only relationships between the sum types, information about changes on the product level must be carried along too. In the case that the constructor has remained the same, we can group up the pairs of arguments and proceed from there, however in the case the external constructor has changed there is no obvious way of pairing up the arguments (they might be completely in different numbers and types)

### Alignment

As mentioned in the previous paragraph, the spine takes care of matching the constructors of two trees, beyond that we still need to define a way to continue diffing the products of data stored in the constructors.
Recall that this alignment has to work between two heterogeneous lists corresponding to the fields associated with two distinct constructors.
The approach presented below is inspired by the existing algorithms based on the edit distance between two strings.
The problem of finding an alignment of two lists of constructor fields can be viewed as the problem of finding an edit script between these two. An edit script is simply a sequence of operations which describe how to change the source list into the destination.
In computing an edit script we simply traverse the lists, from left to right considering one element from each list. At each step we are presented with three choices:

- We can match the two elements (Amod) of the list and continue recursively aligning the rest
- We can insert the destination element before the current element in the source list (Ains) and recursively compute an alignment between whatever we have in the source list and the tail of the destination.
- We can delete the element from the source list (Adel) and recursively compute the alignment between the rest of the source and the destination.

This approach is inspired by the way an edit script between two strings can be computed, there is one major difference though: while in the string case we can assume deletions and insertions to be somewhat equivalent in cost (thus we can safely try to maximize one of the two) in our case, where the elements we are inserting or deleting are arbitrary big subtrees it is not obvious if we should try and maximize insertions or deletions.

The solution to this problem is simple at this step: we simply enumerate all possible alignments, avoiding to skew the algorithm into preferring insertions over deletions or viceversa.


### Atoms

Having figured out all the alignments between two lists of constructor fields we still have to decide what to do in the case where we match two elements.
Here we need to make a distinction between the possibly recursive fields and the constant ones.
In the case of constant fields like `Int`s or `String`s, a transformation between two values of this type is simply a pair recording the source value and the destination value.
In the case of a possibly recursive datatype we are essentially left with the problem we started from: transforming a value of a datatype into another. To do so, we simply start all over again, recursively computing a spine and an alignment between constructor fields.

## Implementation

The AST defined in the Parser module represents the parse result of clojure source code. To set up the stage for the rest of the algorithm we must start building up our universe from the AST, this process is completely mechanical and could be automated via template haskell.

### Setting up the universe

An AST consists of a family of datatypes, with a main one which represents the outer structure of the language, and a number of other possibly mutually recursive datatypes appearing as arguments to the constructors of the main datatype.
This is a simple example of such a family

```
data SExpr = List SExprList | Operation SExpr SExpr | Value Int
  deriving (Eq, Show)

data SExprList = SNil | SCons SExpr SExprList
  deriving (Eq, Show)
```

For each type that appears as an argument to a constructor in our family of datatypes, we will construct an atom, representing that type.
Following the example above, we will define

```
data U = KInt | KSExpr | KSExprList
```

We will also need to associate with each atom a singleton, this will allow us to relate the atoms back to the constructors in the original family.

```
data Usingl :: U -> * where
  Uint    :: Int       -> Usingl KInt
  Usexpr  :: SExpr     -> Usingl KSExpr
  UsexprL :: SExprList -> Usingl KSExprList
```

Now, we can group all the constructors appearing in our original family of datatypes under the `Constr` type, in a similar way that we did for the atoms.
To makes things easier we prepend to the name of each constructor a tag representing which family the constructor came from.
In this case `C1` is `SExpr` and `C2` is `SExprList`.

```
data Constr =
    C1List
  | C1Operation
  | C1Value
  | C2SNil
  | C2SCons
```

Now we just need a way to relate constructors to the correct datatype in the family. To achieve this, we define the `ConstrFor` datatype, which can be viewed as a proof that a certain constructor builds element of a certain family.

```
data ConstrFor :: U -> Constr -> * where
  C1ListProof      :: ConstrFor KSExpr C1List
  C1OperationProof :: ConstrFor KSExpr C1Operation
  C1ValueProof     :: ConstrFor KSExpr C1Value
  C2SNilProof      :: ConstrFor KSExprList C2SNil
  C2SConsProof     :: ConstrFor KSExprList C2SCons
```

Finally we must encode one last bit of information: the "shape" of each constructor. To do so, we can use a closed type family which can be viewed as a function on types. This function takes a `Constr` and returns a list of atoms representing the arguments the constructor accepts.

```
type family TypeOf (c :: Constr) :: [U] where
  TypeOf C1List      = '[KSExprList]
  TypeOf C1Operation = '[KSExpr, KSExpr]
  TypeOf C1Value     = '[KInt]
  TypeOf C2SNil      = '[]
  TypeOf C2SCons     = '[KSExpr, KSExprList]
```

Since `TypeOf` return something of kind `[U]` we will define another GADT named `All`, that maps a function `k -> *` over an argument of kind `[k]` giving us something of kind `*` that we can more easily manipulate and pass around.

The definition for `All` is straightforward:

```
data All (k -> *) :: [k] -> * where
  An ::                    All p '[]
  Ac :: p x -> All p xs -> All p (x ': xs)
```

With this setup we can finally construct the `View` datatype, this loosely corresponds to a generic view as sum of products of a datatype, and simply deconstructs each term of a type into a constructor and a list of arguments applied to that constructor

```
data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u
```

### The actual puzzle

We are now going to sketch an implementation for {...}

#### Spine

We will start from the spine: as mentioned in the previous section, a spine represents the common structure between two elements of a type. In constructing it we must account for three different cases: if the two elements are the same, the spine is trivially a copy, if the top level constructors match, the spine consists of this information and a way to transform the paris of constructor fields and lastly if two constructors don't match, the spine must record this and also contain a way to transform the list of source fields into the list of destination fields.
This can be captured in Haskell by the following GADT:

```
data Spine (at :: U -> *)(al :: [U] -> [U] -> *) :: U -> * where
  Scp  :: Spine at al u
  Scns :: ConstrFor u s -> All at (TypeOf s) -> Spine at al u
  Schg :: ConstrFor u s -> ConstrFor u r
       -> al (TypeOf s) (TypeOf r)
       -> Spine at al u
```
The third parameter of the spine (the `U`) represents the underlying type for which we are trying to compute the transformation, the other two parameters `al` and `at` are respectively a function between products that describes what to do with the different constructor fields and a predicate between atoms which describes what to do with the paired fields in case we have the same constructor.
The `Scp` constructor corresponds to the first case, in which we need to record no additional information other than the fact that the two elements are equal.
The `Scns` constructor corresponds to the second case: the first argument records the common constructor for the two elements and the second represents the list of paired atoms to which we apply the `at` predicate.
The `Schg` constructor represents a change of constructor: the first two arguments record the source and destination constructors, and the third argument is the `al` function applied to the constructor fields of the source and destination constructor respectively.

#### Alignments

Now we need to define a type representing alignments, similarly to the spine, the alignment is parametrized by a predicate `at` which describes how to treat the underlying atoms.
The specification outlined above is captured by the following GADT
```
data Al (at :: U -> *) :: [U] -> [U] -> * where
  A0   :: Al at '[] '[]
  Ains :: Usingl u -> Al at xs ys -> Al at xs (u ': ys)
  Adel :: Usingl u -> Al at xs ys -> Al at (u ': xs) ys
  Amod :: at u -> Al at xs ys -> Al at (u ': xs) (u ': ys)
```
A0 represents the empty alignment, Ains and Adel take as first argument a singleton representing a runtime instance of the type u and, together with an alignment for the rest of the list, give us the alignment with an insertion (resp. deletion) as explained in the section above.
In the Amod case, the first argument is the predicate on the underlying atom and the rest is as in the previous cases.

##### Computing Alignments

Given the Al type defined in the previous section, computing an alignment is pretty trivial. Since we don't know apriori which alignment is more efficient we non-deterministically compute all the possible
The number of alignments can grow very quickly; to make things worse, as we are dealing with alignments of arbitrarily large subtrees which prevents us from optimizing towards insertions or deletions. It is easy to see that in some cases, prioritizing deletions can be more profitable and in other it may be better to do the opposite; this uncertainty stems from the fact that at the time we are calculating the alignment we have no information about the size of the subtrees we are considering.

There is one optimization we can introduce, despite this limitation: in the case where we can match a pair of elements then we can avoid computing an insertion followed by a deletion (resp. a deletion followed by an insertion) since the case in which we match is at least "as good" as the case in which we perform the two different operations in sequence, regardless of the actual cost we are assigning to each operation.

To implement this we must add a parameter that tracks the operation that was taken at the previous step, we will call this the `Phase`. We can define an `alignOpt` function with the same signature as `align` but parametrized with the `Phase`. The optimized version will simply avoid performing an insertion if the last step was a deletion and vice versa. We just have to take care of calling the optimized function for insertion and deletions in the branch in which we could perform a match.

## Biblio

[1] Type-directed diff
[2] http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf
