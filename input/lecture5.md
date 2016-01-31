# Parsers derivation

## Parsers and Functional Programming

* Parsers in functional languages have been studied extensively

  - [Functional Parsers by J. Fokker](http://www.staff.science.uu.nl/~fokke101/article/parsers/)
  - [Monadic Parser Combinatiors by G. Hutton](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf)

* A parser can be modeled as a *function* of, for instance, the following type

  ```haskell
  type Parser a = String -> (a, String)
  ```

  A parser takes an string to parse, parses the string, and as a result returns
  a structure of type `a` and the *unconsumed* suffix of the input string.

  <div class="alert alert-info">
  Functional programming is a natural fit to build parsers. They are functions!
  </div>

* In fact, parsers are DSL in Haskell.

## Monadic parsers

* Monadic parsers are powerful enough to describe *context-sensitive* grammars

  <div class="alert alert-info">
  The grammar itself can depend on the input!
  </div>

  ```haskell
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  ```

  Can you see the dependency in the type of bind?

* Efficiency of parsers often revolves around the implementation of the choice
  operator

  ```haskell
  (+++) :: Parser a -> Parser a -> Parser a
  ```

  The parser does not know which option to follow, so it is common to simple try
  all the possibilities and backtracking when needed.

  In a monadic parser, we need to wait for one of the parsers to succeed or fail
  (memory leak?).

* Related work

  - Weaker notion of sequencing than monads ([Arrows by John
    Hughes](http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf))

    * It only supports context-free grammars

  - Special designed choice combinators: *asymmetric choice* where the right
    hand-side parser only runs if the left hand-side fails; *deterministic
    choice* where a symbol look ahead can resolved which parser to use.

    * Deterministic parsers [Deterministic, Error-Correcting Combinator Parsers
      by S. Swierstra and
      L. Duponcheel](http://www.staff.science.uu.nl/~swier101/Papers/1996/DetErrCorrComPars.pdf)

    * Non-deterministic parsers [Combinator Parsers: From Toys to Tools by S. Swierstra](http://www.cs.nott.ac.uk/~pszgmh/papers/18.ps)

  In any of the work mentioned above, there is a *general choice* combinator
  which still suffers from inefficiencies.

## This lecture

* It is based on the article [FUNCTIONAL PEARL Parallel Parsing
  Processes by K. Claessen](http://www.cse.chalmers.se/edu/year/2015/course/TDA342_Advanced_Functional_Programming/Papers/parser-claessen.pdf)

* Programming monadic parsing library
  - Efficient choice combinator (breadth-first rather than deep-first search)
  - Non-deterministic grammars

* No special annotation

* **Derived**!

* Used by GHC `Read` type class


## How are we going to derive the parser library?

* **1)** Create a EDSL for parsers
* **2)** Observe typical usage patterns
* **3)** Introduce new constructor for such patterns
* **4)** Simplify data type and derive operations for the new constructors
* **5)** Identify other sources of inefficiencies, change the data type, and
  derive the new definition for the optimizations

## A simple EDSL for parsing


* A parser `Parser s a` takes a stream of symbols of type `s`, parses it, and
  produces a value of type `a`

* API for the parsers

  ```haskell
  {-- Type --}
  data Parser s a
  {-- Constructors --}
  symbol :: Parser s s
  fail   :: Parser s a
  return :: a -> Parser1 s a
  {-- Combinators --}
  (+++)  :: Parser s a -> Parser s a -> Parser s a
  (:>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b ```

  Function `symbol` returns the next symbol. Function `fail` aborts
  parsing. Function `return` and `(:>>=)` are the monadic primitives.
  Function `(+++)` is the choice operator.

* Before we get into the implementation details, let us describe some laws that
  we expect from the API -- in that manner, we can detect early on if we are doing
  things right! Moreover, *we will use these laws later to derive an efficient implementation of
  the library.*


  - Monads

    <table class="table table-bordered">
    <thead>
      <tr>
      <th>Monadic primitives</th>
      <th>Laws</th>
      </tr>
    </thead>

    <tr>
    <td> **L1** (Left Identity): </td>
    <td>  ```haskell return a >>= f ≡ f a ```
    </td>
    </tr>

    <tr>
    <td> **L2** (Right Identity): </td>

    <td> ```haskell p >>= return ≡ p ```
    </td>
    </tr>

    <tr>
    <td> **L3** (Associativity): </td>

    <td> ```haskell  (p >>= f) >>= g ≡ p >>= (\x -> f x >>= g) ```
    </td>
    </tr>
    </table>

  - Miscellaneous I

    <table class="table table-bordered">
    <thead>
      <tr>
      <th>`fail`, `(+++)`, and `(>>=)`</th>
      <th>Laws</th>
      </tr>
    </thead>

    <tr>
    <td> **L4**: </td>
    <td>  ```haskell fail >>= f ≡ fail ```
    </td>
    </tr>

    <tr>
    <td> **L5**: </td>

    <td> ```haskell (p +++ q) >>= f ≡ (p >>= f) +++ (q >>= f) ```
    </td>
    </tr>
    </table>

  - Miscellaneous II
    <table class="table table-bordered">
    <thead>
      <tr>
      <th>More on `fail` and `(+++)` </th>
      <th>Laws</th>
      </tr>
    </thead>
    <tr>
    <td> **L6**: </td>
    <td>  ```haskell fail +++ q ≡ q```
    </td>
    </tr>
    <tr>
    <td> **L7**: </td>
    <td> ```haskell  p +++ fail  ≡ p ```
    </td>
    </tr>
    </table>

    - Choice operator
    <table class="table table-bordered">
    <thead>
      <tr>
      <th>On `(+++)`</th>
      <th>Laws</th>
      </tr>
    </thead>

    <tr>
    <td> **L8** (Associativity): </td>
    <td>  ```haskell (p +++ q) +++ r ≡ p +++ (q +++ r) ```
    </td>
    </tr>

    <tr>
    <td> **L9** (Commutativity): </td>

    <td> ```haskell  p +++ q ≡ q +++ p ```
    </td>
    </tr>
    </table>

  - Key law for efficiency!
    <table class="table table-bordered">
    <thead>
      <tr>
      <th>On `(>>=)`, `(+++)`, and `symbol` </th>
      <th>Laws</th>
      </tr>
    </thead>

    <tr>
    <td> **L10**: </td>
    <td>  ```haskell (symbol >>= f) +++ (symbol >>= g) ≡
                      symbol >>= (\c -> f c +++ g c) ```
    </td>
    </tr>
    </table>

## Reference semantics

* Any semantics we associate to elements of type `Parser s a` must obey the laws
  shown above

* We take a reference semantics, i.e., a semantics that we will all our
  implementation against in order to see if our implementation is correct

* The semantic function `[| _ |]`, also called `run`, is defined as follows
  (we use `{| |}` to denote multisets and `\/` for multiset union).

   ```haskell
   [| _ |] :: Parser s a -> [s] -> {| (a, [s]) |}
   [| symbol   |] (c : s) = {| (c, s) |}
   [| symbol   |] []      = {| |}
   [| fail     |] s       = {| |}
   [| p +++ q  |] s       = [| p |] s  \/  [| q |] s
   [| return a |] s       = {| (a, s) |}
   [| p >>= f  |] s       = {| (b, s_f) | (a, s_p)  <- [| p   |] s
                                        , (b, s_f)  <- [| f a |] s_p
                            |} ```

* Using this semantics we can prove (exercise) a the laws about parsers given
  before.

  ```haskell
  data Parser1 s a where
    {-- Constructors --}
    Symbol  ::  Parser1 s s
    Fail    ::  Parser1 s a
    {-- Combinators --}
    Choice  ::  Parser1 s a -> Parser1 s a -> Parser1 s a
    Return  ::  a -> Parser1 s a
    (:>>=)  ::  Parser1 s a -> (a -> Parser1 s b) -> Parser1 s b ```

    We call it `Parser1` since it is our first attempt

* What about our `run` function? We need to define the semantics of parsers.

  The semantic function `[| _ |]`, also called `run`, is defined as follows
  (we use `{| |}` to denote multisets and `\/` for multiset union).

run :: Parser1 s a -> [s] -> {| (a, [s]) |}
[| symbol   |] (c : s) = {| (c, s) |}
[| symbol   |] []      = {| |}
[| pfail    |] s       = {| |}
[| p +++ q  |] s       = [| p |] s  \/  [| q |] s
[| return x |] s       = {| (x, s) |}
[| p >>= f  |] s       = {| (y, s'') | (x, s')  <- [| p   |] s
                                     , (y, s'') <- [| f x |] s'
                         |}




* The semantics of a parser of type 'Parser1 s a' is a function from a string of 's'
  to a multiset of results paired with the remaining parts of the input string. We
  use a multiset to capture the fact that we don't care about the order of the
  results.

  The semantic function [| _ |], also called run, is defined as follows
  (we use {| |} to denote multisets and \/ for multiset union).


## Basic parsing laws

## Semantics

## Commutativity of choice

## Choice and symbol

## Inefficiencies

## Removing bind

## Removing choice

## Associativity of bind


##
