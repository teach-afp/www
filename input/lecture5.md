# Parser derivation

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

    * Deterministic parsers: [Deterministic, Error-Correcting Combinator Parsers
      by S. Swierstra and
      L. Duponcheel](http://www.staff.science.uu.nl/~swier101/Papers/1996/DetErrCorrComPars.pdf)

    * Non-deterministic parsers: [Combinator Parsers: From Toys to Tools by S. Swierstra](http://www.cs.nott.ac.uk/~pszgmh/papers/18.ps)

  In all of the work mentioned above, there is a *general choice* combinator
  which still suffers from inefficiencies.

## This lecture

* It is based on the article [FUNCTIONAL PEARL Parallel Parsing
  Processes by K. Claessen](./assets/files/parser-claessen.pdf)

* Programming monadic parsing library
  - Efficient choice combinator (breadth-first rather than deep-first search)
  - Non-deterministic grammars

* No special annotation

* **Derived**!

* Used by GHC `Read` type class via
  [`Text.ParserCombinators.ReadP`](https://hackage.haskell.org/package/base-4.19.0.0/docs/src/Text.ParserCombinators.ReadP.html).


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
  return :: a -> Parser s a
  {-- Combinators --}
  (+++)  :: Parser s a -> Parser s a -> Parser s a
  (:>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  ```

  Function `symbol` returns the next symbol. Function `fail` aborts
  parsing. Functions `return` and `(:>>=)` are the monadic primitives.
  Function `(+++)` is the choice operator.

* Before we get into the implementation details, let us describe some laws that
  we expect from the API — in that manner, we can detect early on if we are doing
  things right!

  <div class="alert alert-info">
  This is the **domain knowledge**, which we will use later to gain
  performance!
  </div>

    * Monads

      <table class="table table-bordered">
      <thead>
        <tr>
        <th>Monadic primitives</th>
        <th>Laws</th>
        </tr>
      </thead>

      <tr>
      <td> **L1** (Left Identity): </td>
      <td>
      ```haskell
      return a >>= f ≡ f a
      ```
      </td>
      </tr>

      <tr>
      <td> **L2** (Right Identity): </td>

      <td>
      ```haskell
      p >>= return ≡ p
      ```
      </td>
      </tr>

      <tr>
      <td> **L3** (Associativity): </td>

      <td>
      ```haskell
      (p >>= f) >>= g ≡ p >>= (\x -> f x >>= g)
      ```
      </td>
      </tr>
      </table>

    * Bind distributes over choice

      <table class="table table-bordered">
      <thead>
        <tr>
        <th>`fail`, `(+++)`, and `(>>=)`</th>
        <th>Laws</th>
        </tr>
      </thead>

      <tr>
      <td> **L4**: </td>
      <td>
      ```haskell
      fail >>= f ≡ fail
      ```
      </td>
      </tr>

      <tr>
      <td> **L5**: </td>

      <td>
      ```haskell
      (p +++ q) >>= f ≡ (p >>= f) +++ (q >>= f)
      ```
      </td>
      </tr>
      </table>

    * Choice forms a commutative monoid with unit `fail`

      <table class="table table-bordered">
      <thead>
        <tr>
        <th>More on `fail` and `(+++)` </th>
        <th>Laws</th>
        </tr>
      </thead>
      <tr>
      <td> **L6** (Left unit): </td>
      <td>
      ```haskell
      fail +++ q ≡ q
      ```
      </td>
      </tr>
      <tr>
      <td> **L7** (Right unit): </td>
      <td>
      ```haskell
      p +++ fail  ≡ p
      ```
      </td>
      </tr>
      <tr>
      <td> **L8** (Associativity): </td>
      <td>
      ```haskell
      (p +++ q) +++ r ≡ p +++ (q +++ r)
      ```
      </td>
      </tr>

      <tr>
      <td> **L9** (Commutativity): </td>

      <td>
      ```haskell
      p +++ q ≡ q +++ p
      ```
      </td>
      </tr>
      </table>

    * Key law for efficiency!
      <table class="table table-bordered">
      <thead>
        <tr>
        <th>On `(>>=)`, `(+++)`, and `symbol` </th>
        <th>Laws</th>
        </tr>
      </thead>

      <tr>
      <td> **L10**: </td>
      <td>
      ```haskell
      (symbol >>= f) +++ (symbol >>= g) ≡
          symbol >>= (\c -> f c +++ g c)
      ```
      </td>
      </tr>
      </table>

## Reference semantics

* Any semantics we associate to elements of type `Parser s a` must obey the laws
  shown above.

* We take a reference semantics, i.e., a semantics that we will compare our
  implementation against in order to see if our implementation is correct.

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
                            |}
  ```

* Using this semantics we can prove (exercise) the laws about parsers given
  before.

  For instance, here is the proof of **L10** for the case of a non-empty input
  string:

  ```haskell
   ==  { Def. of [| p +++ q |] }
     [| symbol >>= f |] (c:s)  \/  [| symbol >>= g |] (c:s)
   ==  { Def. of [| p >>= f |] and [| symbol |] }
     [| f c |] s  \/  [| g c |] s
   ==  { Def. of [| p +++ q |] "backwards" }
     [| f c +++ g c |] s
   ==  { Def. of [| p >>= f |] and [| symbol |] "backwards" }
     [| symbol >>= (\c -> f c +++ g c) |] (c:s)
  ```

  <div class = "alert alert-info">
  Exercise: prove or test the rest of the laws
  </div>

* The reference semantics is useful for reasoning, but inefficient.

* There are three sources of possibly inefficiency that we can
  identify:

  <table class="table table-bordered">
  <thead>
  <tr>
  <th> Source </th>
  <th> Reason</th>
  </tr>
  </thead>

  <tr class="alert alert-warning">
  <td> Definition of `(+++)` </td>
  <td>  Union of bags
  </td>
  </tr>

  <tr class="alert alert-warning">
  <td> Definition of `(>>=)` </td>

  <td> Creation of many intermediate results (e.g., `[| p |] s` and `[| f a |]`)
  </td>
  </tr>

  </table>


## Parser0: our first implementation
[code](https://github.com/teach-afp/afp-code/blob/master/L5/Parser0.hs)

* Every constructor and combinator is a constructor in the `Parser` data type.

  ```haskell
  data Parser0 s a where
    {-- Constructors --}
    Symbol  ::  Parser0 s s
    Fail    ::  Parser0 s a
    {-- Combinators --}
    Choice  ::  Parser0 s a -> Parser0 s a -> Parser0 s a
    Return  ::  a -> Parser0 s a
    (:>>=)  ::  Parser0 s a -> (a -> Parser0 s b) -> Parser0 s b
  ```

  We call it `Parser0` since it is our first attempt.

* Constructors and combinators (trivial)

  ```haskell
   {- | Constructors -}
   symbol = Symbol
   pfail  = Fail

   {- | Combinators -}
   (+++)  = Choice
  ```

* Monadic operations (trivial)

  ```haskell
   instance Monad (Parser0 s) where
     return = Return
     (>>=)  = (:>>=)
  ```

* What about our `run` function?

  To start with, and for simplicity, we use lists instead of bags to denote the
  semantics of parsers.

  ```haskell
   type Semantics s a = [s] -> [(a,[s])]
  ```

  The run function maps the constructors to their semantics.

  ```haskell
   run0 :: Parser0 s a -> Semantics s a
   run0 Symbol          [] = []
   run0 Symbol      (s:ss) = [(s,ss)]
   run0 Fail            _  = []
   run0 (Choice p q)    ss = (run0 p ss) ++ (run0 q ss)
   run0 (Return x)      ss = [(x,ss)]
   run0 (p :>>= f)      ss = [(y,s2) | (x,s1) <- run0 p ss,
                                       (y,s2) <- run0 (f x) s1]
  ```

  <div class = "alert alert-info">
  We have the same sources of inefficiency as the reference semantics!
  (i.e., definition of `Choice` and `(:>>=)`)
  </div>

## Parser1: removing bind
[code](https://github.com/teach-afp/afp-code/blob/master/L5/Parser1.hs)

* The use of list comprehension in `run0 (p :>>= f)` builds a lot of
   intermediate lists which might be costly

* How do we simplify it?

  We move towards an *intermediate* representation, where the bind takes place
  when constructing the program — not when running it!

* Methodology:

  * Remove `(:>>=)` from the data type
  * Try to define `(>>=)` anyways, and analyze the *usage patterns which
    we cannot write*
  * Introduce new constructors to capture such cases
  * Simplify the data type with the new constructors and *derive the definition
    for `(>>=)`*

* Let us try to define `(>>=)`

* ```haskell
   Fail >>= k
  ```

  By **L4**, we know that

  ```haskell
   Fail >>= k ≡ Fail
  ```

   Success! (Nothing to be done)

* ```haskell
   Choice p q >>= f
  ```

  By **L5**, we know that

  ```haskell
   Choice p q >>= f ≡ Choice (p >>= f) (q >>= f)
  ```

   Success! (Nothing to be done)

* ```haskell
   Return x >>= f
  ```

  The first monad law already tells us that this is just `(f x)`.

  Success! (Nothing to be done)


* ```haskell
   Symbol >>= k
  ```

  There is no **L**-rule for this case! Let us capture this usage pattern in a
  new constructor

  ```haskell
   SymbolBind k ≡ Symbol >>= k
  ```

  Observe that
  ```haskell
   k :: s -> Parser0 s a
  ```

  Therefore, we have that

  ```haskell
   SymbolBind :: (s -> Parser0 s a) -> Parser0 s a
  ```

* We obtain `Parser1` from the definition of `Parser0`, where `SymbolBind` gets
  introduced and `(:>>=)` gets removed

  ```haskell
   data Parser1 s a where
       SymbolBind ::  (s -> Parser1 s a) -> Parser1 s a
       Fail       ::  Parser1 s a
       Choice     ::  Parser1 s a -> Parser1 s a -> Parser1 s a
       Return     ::  a -> Parser1 s a
  ```

  Observe that there is no `(:>>=)`

* What about the `run` function?

  ```haskell
  run1 :: Parser1 s a -> Semantics s a
  run1 Fail                _ = []
  run1 (Choice p q)       ss = run1 p ss ++ run1 q ss
  run1 (Return x)         ss = [(x,ss)]
  run1 (SymbolBind k)     ss = ?
  ```

  It is mainly as before, but the intermediate results generated by `(:>>=)`
  are not there.

  In the definition of `run1`, the new interesting case is

  ```haskell
  run1 (SymbolBind k) ss = ?
  ```

  We are going to derive it by using the reference semantics.

  We know, by the definition of `SymbolBind`, that

  ```haskell
  [| SymbolBind k |] = [| symbol >>= k |]
  ```

  By the reference semantics of `(>>=)`, we have that

  ```haskell
  [| symbol >>= k |] ss = {| (b, s_k) | (a, ss_p)  <- [| symbol |] ss
                                      , (b, ss_k)  <- [| k a |] ss_p
                          |}
  ```

  So, we have two cases:

  ```haskell
  [| symbol >>= k |] [] = {| (b, s_k) | (a, ss_p)  <- [| symbol |] []
                                      , (b, ss_k)  <- [| k a |] ss_p
                          |}
  ```

  By the reference semantics of `symbol`, we have

  ```haskell
  [| symbol >>= k |] [] = {| (b, ss_k) | (a, ss_p)  <- {| |}
                                       , (b, ss_k)  <- [| k a |] ss_p
                          |}
  ```

  By multi-set comprehension, we conclude that

  ```haskell
  [| symbol >>= k |] [] = {| |}
  ```

  On the other hand, we have the following equation

  ```haskell
  [| symbol >>= k |] (s:ss) = {| (b, s_k) | (a, ss_p)  <- [| symbol |] (s:ss)
                                          , (b, ss_k)  <- [| k a |] ss_p
                              |}
  ```

  By applying the reference semantics of `symbol`, we have that

  ```haskell
  [| symbol >>= k |] (s:ss) = {| (b, s_k) | (a, ss_p)  <- {| (s, ss) |}
                                          , (b, ss_k)  <- [| k a |] ss_p
                              |}
  ```

  By multi-set comprehension, we conclude that

  ```haskell
  [| symbol >>= k |] (s:ss) = {| (b, ss_k) | (b, ss_k)  <- [| k s |] ss |}
  ```

  which, by multi-set comprehension, is equivalent to

  ```haskell
  [| symbol >>= k |] (s:ss) = [| k s |] ss
  ```

  To summarize, we obtain

  ```haskell
  [| symbol >>= k |] []     = {| |}
  [| symbol >>= k |] (s:ss) = [| k s |] ss
  ```

  Therefore, we conclude that

  ```haskell
  run1 (SymbolBind k) []     = []
  run1 (SymbolBind k) (s:ss) = run1 (k s) ss
  ```

* Constructors and combinators? (the non-proper morphisms are mainly as before)

  ```haskell
  {- | Constructors -}
  symbol = SymbolBind Return
  pfail  = Fail

  {- | Combinators -}
  (+++)  = Choice
  ```

  Observe that `symbol` is defined as `SymbolBind Return`. Is it true that
  `symbol` only extracts a symbol from the input?

  ```haskell
  SymbolBind Return ≡ Symbol >>= Return
  ```

  By **L2** (Right Identity), we know that

  ```haskell
  SymbolBind Return ≡ Symbol
  ```

  So, `SymbolBind` corresponds to the notion of `Symbol` in `Parser0`!

* What about `return`?

  Function `return` is just as before.

  ```haskell
   return = Return
  ```

* The interesting case is `(>>=)`

  How are we going to define it?

  So far, we have that

  ```haskell
  Fail       >>= k = Fail
  Choice p q >>= f = Choice (p >>= f) (q >>=f)
  Return a   >>= k = k a
  ```

  What about our recently introduced constructor (`SymbolBind`)?

  ```haskell
  SymbolBind f >>= k = ?
  ```

  By definition of `SymbolBind`, we know that

  ```haskell
  SymbolBind f >>= k ≡ Symbol >>= f >>= k
  ```

  By **L3** (Associativity of monads), we have that

  ```haskell
  Symbol >>= f >>= k ≡ Symbol >>= (\s -> f s >>= k)
  ```

  By our definition of `SymbolBind`, we have that

  ```haskell
  Symbol >>= (\s -> f s >>= k) ≡ SymbolBind (\s -> f s >>= k)
  ```

  So, we finally have that

  ```haskell
  SymbolBind f >>= k = SymbolBind (\s -> f s >>= k)
  ```

* We can now define `Parser1` as a monad

  ```haskell
  {- | Monadic instance for Parser1 -}
  instance Monad (Parser1 s) where
     return = Return
    Fail         >>= k = Fail
    Choice p q   >>= k = Choice (p >>= k) (q >>= k)
    Return x     >>= k = k x
    SymbolBind f >>= k = SymbolBind (\s -> f s >>= k)
  ```

  <div class = "alert alert-info">
  Observe that the definition of `(>>=)` was derived from the
  domain knowledge and monadic laws. We cannot get it wrong!
  </div>


## Transforming parsers of type `Parser0` into parsers of type `Parser1`

* Is `Parser1` as expressive as `Parser0`? In other words, can any parser you
  wrote of type `Parser0` be reformulated as a parser of type `Parser1`?

  Yes! We can write a function which transform a `Parser0` into a `Parser1`

  ```haskell
  cast :: P0.Parser0 s a -> Parser1 s a
  ```

  To avoid name crashes, all the constructors and types from `Parser0` are
  qualified as `P0`

  Let us see the easy cases.

  ```haskell
  cast :: P0.Parser0 s a -> Parser1 s a
  cast P0.Symbol       = SymbolBind Return -- L1
  cast P0.Fail         = Fail
  cast (P0.Choice p q) = Choice (cast p) (cast q)
  cast (P0.Return x)   = Return x
  ```

  The core of the translation is bind!

  ```haskell
  cast (P0.Symbol P0.:>>= k)       = SymbolBind (cast . k)
                                     -- def of SymbolBind

  cast (P0.Fail P0.:>>= _)         = Fail
                                     -- Parser law, L4.

  cast ((P0.Choice p q) P0.:>>= k) = Choice (cast (p P0.:>>= k))
                                            (cast (q P0.:>>= k))
                                     -- Parser law, L5

  cast ((P0.Return x) P0.:>>= k)   = cast (k x)
                                     -- monad law, L1

  cast ((p P0.:>>= f) P0.:>>= k)   = cast (p P0.:>>= (\x -> f x P0.:>>= k))
                                     -- monad law, L3
  ```

   <div class = "alert alert-info">
   Observe that for every case, there is some law which helps to derive the
   translation of bind!
   </div>

## Parser2: improving choice
[code](https://github.com/teach-afp/afp-code/blob/master/L5/Parser2.hs)

* If we observe the `run1` function again

  ```haskell
  run1 :: Parser1 s a -> Semantics s a
  run1 (SymbolBind k)     [] = []
  run1 (SymbolBind k) (s:ss) = run1 (k s) ss
  run1 Fail                _ = []
  run1 (Choice p q)       ss = (run1 p ss) ++ (run1 q ss)
  run1 (Return x)         ss = [(x,ss)]
  ```

  We have another source of inefficiency. Can you see it?

  The list append `(++)` is linear in its first argument which means that left
  nested applications `(+++)` get a quadratic behaviour, e.g., consider
  expressions of the form `((s1 ++ s2) ++ s3)`.

* How can we optimize `Choice`, i.e. `(+++)`?

  Similar as we did with bind, we remove it from our data type.  The choice
  operator `(+++)` then takes place when building the program — not when
  running it!

* The new data type for parsers

  ```haskell
  data Parser2 s a where
    SymbolBind ::  (s -> Parser2 s a) -> Parser2 s a
    Fail       ::  Parser2 s a
    Return     ::  a -> Parser2 s a
  ```

* Let us try to define `(+++)`

  For `Fail` is easy due to laws **L6** and **L7**.

  ```haskell
  (+++) :: Parser2 s a -> Parser2 s a -> Parser2 s a
  Fail +++ _    = Fail
  q    +++ Fail = Fail
  ```

  For `SymbolBind`, we know by **L10** that

  ```haskell
  SymbolBind f +++ SymbolBind q = SymbolBind (\s -> f s +++ q s)
  ```

  If `SymbolBind` is combined with `Fail` instead, we know the result (see **L6** and
  **L7**).

  The tricky case is when `SymbolBind` is pattern-matched with `Return`.

  ```haskell
  SymbolBind f +++ Return x     = ?
  Return x     +++ SymbolBind f = ?
  ```

  It seems that `Return` is stopping us from defining `(+++)`. In fact, what is
  the definition of `(+++)` when it only deals with `Return`?

  ```haskell
  Return x +++ Return y = ?
  ```

* For these cases, we therefore introduce a new constructor.

  ```haskell
  ReturnChoice x p ≡ Return x +++ p
  ```

  Observe that, by **L7**,

  ```haskell
  Return x ≡ ReturnChoice x Fail
  ```

  `ReturnChoice` can encode `Return`!

* Therefore, let us take the definition of `Parser2` and replace `Return` with
  `ReturnChoice`

  ```haskell
  data Parser2 s a where
      SymbolBind   ::  (s -> Parser2 s a) -> Parser2 s a
      Fail         ::  Parser2 s a
      ReturnChoice ::  a -> Parser2 s a -> Parser2 s a
  ```

* Let us *now* define `(+++)` by using parser laws, commutative, and associative
  laws.

  ```haskell
  (+++) :: Parser2 s a -> Parser2 s a -> Parser2 s a
  SymbolBind f     +++ SymbolBind g     = SymbolBind (\s -> f s +++ g s)
                                          -- L10
  p                +++ Fail             = p
                                          -- L6
  Fail             +++ q                = q
                                          -- L7

  ReturnChoice x p +++ q                = ?

  p                +++ ReturnChoice x q = ?
  ```

  We derive the tricky cases.

  By definition of `ReturnChoice`, we have that
  ```haskell
  ReturnChoice x p +++ q ≡ (Return x +++ p) +++ q
  ```

  By **L8** (associativity of `(+++)`), we have that

  ```haskell
  (Return x +++ p) +++ q ≡ Return x +++ (p +++ q)
  ```

  By the definition of `ReturnChoice`, we obtain

  ```haskell
  Return x +++ (p +++ q) ≡ ReturnChoice x (p +++ q)
  ```

  <div class = "alert alert-info">
  Exercise: derive the definition for `p +++ ReturnChoice x q`
  </div>

  ```haskell
  (+++) :: Parser2 s a -> Parser2 s a -> Parser2 s a
  SymbolBind f     +++ SymbolBind g     = SymbolBind (\s -> f s +++ g s)
                                          -- L10
  p                +++ Fail             = p
                                          -- L6
  Fail             +++ q                = q
                                          -- L7

  ReturnChoice x p +++ q                = ReturnChoice x (p +++ q)

  p                +++ ReturnChoice x q = ReturnChoice x (p +++ q)
  ```


* So, we obtain `(+++)` defined, but we should fix `(>>=)` since
  we replaced `Return` with `ReturnChoice`

  ```haskell
  {- | Monadic instance for Parser2 -}
  instance Monad (Parser2 s) where
    return x = ReturnChoice x Fail
    Fail             >>= k = Fail
    (SymbolBind f)   >>= k = SymbolBind (\s -> f s >>= k)
    ReturnChoice x p >>= k = ?
  ```

  By definition of `ReturnChoice`, we have that

  ```haskell
  ReturnChoice x p >>= k ≡ (Return x +++ p) >>= k
  ```

  By **L5**, we have that

  ```haskell
  (Return x +++ p) >>= k ≡ (Return x >>= k) +++ (p >>= k)
  ```

  By **L1** (Left Identity), we conclude that

  ```haskell
  (Return x >>= k) +++ (p >>= k) ≡ k x +++ (p >>= k)
  ```

  So, we obtain that

  ```haskell
  ReturnChoice x p >>= k = k x +++ (p >>= k)
  ```

  ```haskell
  {- | Monadic instance for Parser2 -}
  instance Monad (Parser2 s) where
    return x = ReturnChoice x Fail
    Fail             >>= k = Fail
    (SymbolBind f)   >>= k = SymbolBind (\s -> f s >>= k)
    ReturnChoice x p >>= k = k x +++ (p >>= k)
  ```

* We have completed the implementation of `(+++)` and `(>>=)` which gets
  computed when constructing parsers — not when running them!

* Let us see the run function.

  We take `run1` for parsers of type `Parser1`, remove the case for `Choice`,
  and see what happens when placing `ReturnChoice` in the place of `Return`.

  ```haskell
  run2 :: Parser2 s a -> Semantics s a
  run2 (SymbolBind k)     [] = []
  run2 (SymbolBind k) (s:ss) = run2 (k s) ss
  run2 Fail                _ = []
  run2 (ReturnChoice x p) ss = ?
  ```

  We are going to try deriving the definition. However, since we are dealing
  with the run function, we need to consider the ideal semantics of parsers.

  By definition of `ReturnChoice`, we obtain that

  ```haskell
  [| ReturnChoice x p |] ss ≡  [| Return x +++ p |] ss
  ```

  By the semantics of `(+++)`, we obtain that

  ```haskell
  [| Return x +++ p |] ss ≡ [| Return x |] ss \/ [| p |] ss
  ```

  By the semantics of `Return x`, we have that

  ```haskell
  [| Return x |] ss \/ [| p |] ss ≡ {| (x, ss) |} \/ [| p |] ss
  ```

  Summarizing, we have that

  ```haskell
  [| ReturnChoice x p |] ss ≡ {| (x, ss) |} \/ [| p |] ss
  ```

  So, we complete the definition of `run2` as follows.

  ```haskell
  run2 :: Parser2 s a -> Semantics s a
  run2 (SymbolBind k)     [] = []
  run2 (SymbolBind k) (s:ss) = run2 (k s) ss
  run2 Fail                _ = []
  run2 (ReturnChoice x p) ss = (x, ss) : run2 p ss
  ```


## Transforming parsers of type `Parser1` into parsers of type `Parser2`

* Is `Parser2` as expressive as `Parser1`? In other words, can any parser you
  wrote of type `Parser1` be reformulated as a parser of type `Parser2`?

  Yes! We can write a function which transforms a `Parser1` into a `Parser2`

  ```haskell
  cast2 :: Parser1 s a -> Parser2 s a
  ```

  <div class = "alert alert-info">
  Exercise: write `cast2`
  </div>

## Parser3: optimizing `(>>=)`

* There is still one remaining source of inefficiency.

* If you look at the definition of `(>>=)`, you'll see that it is linear in the
  size of its first argument.

  This means that we get a similar problem to the use of `(++)`, namely a
  quadratic behaviour for left nested uses of `(>>=)`.

* In order to fix this we cannot use the method we've been using so far, there
  is no constructor to remove to fix the problem. Instead, we have to use
  another technique, called a "context passing" implementation.

* Read more about it in the paper.

## Summary

* Parsers laws ≡ domain knowledge

* Detect inefficiencies and introduce changes

* Derivation to synthesize the right code (no hacking!)

  - Leveraging domain and monad laws
