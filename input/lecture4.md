# Functors, Applicative, and Monads

## A simple monadic EDSL for input/output (deep embedding)
[code](https://bitbucket.org/russo/afp-code/src/e1107f839f5424f1a209e2e1393d278237e480f1/L4/EDSL_Deep1.hs?at=master&fileviewer=file-view-default)

* We will develop a simple I/O EDSL in Haskell

  ```haskell
  -- Types
  data Program a
  -- Constructors
  return :: a -> Program a
  putC   :: Char -> Program ()
  getC   :: Program (Maybe Char)
  -- Combinators
  (>>=) :: Program a -> (a -> Program b) -> Program b
  -- Run function
  type IOSem a
  run :: Program a -> IOSem a  ```

* We will use monads to handle the I/O effects!

* Scenario

  <div class="container">
     <img class="img-responsive col-md-11"
       src="./assets/img/terminal.png">
  </div>


* Types for inputs and outputs

   ```haskell
   type Input   =  String
   type Output  =  String  ```

* Following the deep embedding guidelines, we have a constructor in `Program`
   per constructor / combinator.

    ```haskell
    data Program a where
      Put    :: Char -> Program ()
      Get    :: Program (Maybe Char)
      Return :: a -> Program a
      Bind   :: Program a -> (a -> Program b) -> Program b ```

    Observe the use of GADTs!

    - In the definition of `Get`, why using `Maybe Char` instead of `Char`?
    - We need to indicate the "end of input" somehow, i.e., by using `Nothing`

    ```haskell
    getC = Get
    putC = Put ```

* What's the implementation of the type `IOSem`, i.e., the semantics of a program?

  ```haskell
  type IOSem a = Input  -> (a, Input, Output)
  ```

  It is a function which takes an input and returns a result (of type `a`), the
  *left over* input (of type `Input`), and an output (of type `Output`).

* The `run` function

  ```haskell
  run :: Program a -> IOSem a
  run (Put c)    inp =     (()     ,inp   , c:"")
  run (Get)      ""  =     (Nothing, ""   , ""  )
  run (Get)      (c:cs) =  (Just c ,cs    , ""  )
  run (Return x) inp =     (x      ,inp   , ""  )
  run (Bind p g) inp =     (someb  ,someinpp, someoutp ++ someoutpp)
     where   (somea, someinp, someoutp) = run p inp
             pb = g somea
             (someb, someinpp, someoutpp) = run pb someinp  ```

* Let us write an "echo" program

  ```haskell
  echo :: Program ()
  echo = getC >>= f
    where f Nothing  = return ()
          f (Just c) = putC c ```

  To run it, we need to give it an input

  ```haskell
  run echo "a"
  > ((), "", "a") ```

  <div class = "alert alert-info">
  **Exercise**: write a program which does a *double echo*, i.e., it reads a character
  from the input and writes it twice into the output
  </div>

## A simple monadic EDSL for input/output (intermediate embedding)
[code](https://bitbucket.org/russo/afp-code/src/b0fa3655c8e45ae63daa0d9c16de3ef4f1bb7f9c/L4/EDSL_Deep2.hs?at=master&fileviewer=file-view-default)

* It is often good to move away a bit from the pure deep embedding towards some
  kind of "normal form" ("optimized", "elemental" embedding). In our case, we can
  start by looking at how `Put` and `Get` can be used. The only combinator in
  our language is `Bind` `(>>=)` so lets looks at the different cases for the
  first argument to `Bind`.

  ```haskell
       Put c >>= f
         Get >>= f
    Return x >>= f
   (m >>= g) >>= f
  ```

* ```haskell
   Put c >>= f ```

   From the types of `Put` and `Bind`  we note that `f` must have type

   ```haskell
   () -> m a  ```

   which is basically just a value of type `m a`. Another way to think
   about it is that `Put` does not really return any useful value (the
   actual "putting" is implemented as a "side-effect"). So the function
   after bind can ignore its argument.

   ```haskell
   Put c >>= \_ -> p ≡ Put c >> p ```

   We will now give a name to this new combination

   ```haskell
   PutThen c p ≡ Put c >> p ```

   This is a `Program` which starts by printing `c` and the behaves like `p`.

* ```haskell
  Get >>= f ```

  In a similar way we can introduce a new name for the combination of `Get`
  and `Bind`:

  ```haskell
  GetBind f ≡ Get >>= f ```

* ```haskell
  Return x >>= f ```

  The third combination would be `ReturnBind`

  ```haskell
  ReturnBind x f ≡ Return x >>= f ```

  but the first monad law already tells us that this is just `(f x)` so no
  new constructor is needed for this combination.

* For the last combination, i.e., `(m >>= g) >>= f`, the associative monadic
  law tell us how we can rewrite it.

* We then define `Program` using the first two combinations and return

  ```haskell
  data Program a where
    PutThen :: Char -> Program a         -> Program a
    GetBind :: (Maybe Char -> Program a) -> Program a
    Return  :: a                         -> Program a
  ```

  Observe that the data type is a **regular Haskell data type** and it does not use
  any of the features of a GADT.

* One way to think about `PutThen` and `GetBind` is with *continuations*. For
  instance, `PutThen c p` writes a character into the output of the program and
  continues behaving as `p`. Similarly, `Get f` reads maybe a character `mc`
  from the input and continues behaving as program `f mc`.

   ```haskell
   putC :: Char -> Program ()
   putC c = PutThen c $ Return () ```

   Function `putC` writes a character in the output and then it behaves as `Return ()`

   ```haskell
    getC :: Program (Maybe Char)
    getC = GetBind Return ```

    Function `getC` reads maybe a character from the input and, once that is
    done, it behaves as `Return ()`

* The bind is not going to be a part of the `Program` data structure, but rather
  part of the `instance Monad`, i.e., the `bind` is not deeply implemented as a
  constructor (intermediate embedding)

## Calculating `(>>=)` for `Program` as a monad (intermediate embedding)

* What is the definition for bind?

   ```haskell
   instance Monad Program where
      return  = Return
      (>>=)   = bindP

   bindP :: Program a -> (a -> Program b) -> Program b
   bindP (PutThen c p)  k   =  ?
   bindP (GetBind f)    k   =  ?
   bindP (Return x)     k   =  ?  ```

   <div class ="alert alert-info">
   We can *calculate* the correct definition of bindP using
   the definitions of `PutThen`, `GetBind`, and the monadic laws!
   </div>

*  ```haskell
   bindP (Return x) k = ? ```

   ```haskell
     bindP (Return x)    k
   = Def. >>=
     (Return x) >>= k
   =  Law 1.  return x >>= f ≡ f x
     k x
   ```

   ```haskell
   bindP (Return x) k = k x  ```

*  ```haskell
   bindP (GetBind f) k = ? ```

   ```haskell
     bindP (GetBind f)   k
   = Def. of (>>=)
     (GetBind f) >>=  k
   = Def. GetBind
     (Get >>= f) >>=  k
   = Law 3.  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
     with m = Get, f = f, g = k
     Get >>= (\x -> f x >>= k)
   = Def. GetBind
     GetBind (\x -> f x >>= k)
   = Def. of (>>=)
     GetBind (\x -> bindP (f x) k) ```

   ```haskell
   bindP (GetBind f) k = GetBind (\x -> bindP (f x) k) ```

* ```haskell
     bindP (PutThen c p) k = ? ```

   ```haskell
     bindP (PutThen c p) k
   = { Def. of (>>=) }
     (PutThen c p) >>= k
   = { Def. of PutThen }
     (Put c >> p) >>= k
   =
     (Put c >>= \_ -> p) >>= k
   = Law3 with m = Put c, f = \_->p, g = k
     Put c >>= (\x -> (\_->p) x >>= k)
   = simplify
     Put c >>= (\_ -> p >>= k)
   = Def. of >>
     Put c >> (p >>= k)
   = Def. of PutThen
     PutThen c (p >>= k) ```

  ```haskell
  bindP (PutThen c p) k =  PutThen c (bindP p k) ```

* So, we can now complete defining `Program` as a monad

  ```haskell
  -- | It turns out that bind can still be defined!
  instance Monad Program where
    return  = Return
    (>>=)   = bindP

  -- | Bind takes the first argument apart:
  bindP :: Program a -> (a -> Program b) -> Program b
  bindP (PutThen c p)  k   =  PutThen c (bindP p k)
  bindP (GetBind f)    k   =  GetBind (\x -> bindP (f x) k)
  bindP (Return x)     k   =  k x ```

## A simple monadic EDSL for input/output (shallow embedding)

* <div class = "alert alert-info">
  This is an exercise for you to do!
  </div>

  [Please, check the code skeleton for that](https://bitbucket.org/russo/afp-code/src/c5e03aa3cd3074246e8a698c5f70041d9ddd92b1/L4/EDSL_Shallow.hs?at=master&fileviewer=file-view-default)


## Monads

* A structure that represents computations defined as sequences of steps.
  <div class = "alert alert-info">
  The bind operator `(>>=)` defines what it means to chain operations of a
  monadic type.
  </div>

* In this lecture, we will learn about two other structures useful in functional
  programming
  - Functors
  - Applicative functors

## Structure-preserving mappings

* We are familiar with the `map` function over lists
  ```haskell
  map (+1) [2,3,4,5,6] ```

  which *applies* the function `(+1)` to every element of the list*, and produces
  the list

  ```haskell [3,4,5,6,7]
  ```

* We can generalize the concept of `map` to work on trees

  ```haskell
  data Tree a = Leaf a | Node (Tree a) (Tree a)

  mapTree :: (a -> b) -> Tree a -> Tree b
  mapTree f (Leaf a) = Leaf (f a)
  mapTree f (Node t1 t2) = Node (mapTree f t1)
                                (mapTree f t2) ```

* As we did with lists, we can apply the function `(+1)` at every element of the
  data structure.

  For instance,
  ```haskell
  mapTree (+1) (Node (Leaf 2) ((Node (Node (Leaf 3) (Leaf 4)) (Leaf 5))))
  ```
  produces the following tree.
  ```haskell
  Node (Leaf 3) (Node (Node (Leaf 4) (Leaf 5)) (Leaf 6)) ```

* In both cases, the structure of the data type (i.e., lists and trees) is preserved

* General pattern:

  <div class="alert alert-info">
  It is useful to apply functions to data types elements while respecting the
  structure (shape) of it
  </div>

  <div class="container">
     <img class="img-responsive col-md-10"
       src="./assets/img/functor.png">
  </div>


## Functors

* A functor is composed of two elements: a *data type definition* (*container*)
  and a generalized `map`-function called `fmap`

* In Haskell, `fmap` is an overloaded function, i.e., defined for every
  container which support a `map`-like operation

  ```haskell
  class Functor d where
     fmap :: (a -> b) -> d a -> d b
  ```

* A functor must obey the following laws.

  <table class="table table-bordered">
  <thead>
    <tr>
    <th>Name</th>
    <th>Law</th>
    </tr>
  </thead>

  <tr>
  <td> Identity: </td>
  <td>  ```haskell fmap id ≡ id ```
  </td>
  </tr>

  <tr>
  <td> Map fusion (or composition): </td>

  <td> ```haskell fmap (f . g) ≡ fmap f . fmap g ```
  </td>
  </tr>
  </table>
  Observe that *map fusion* allows to reduce two traversals to one!

* Let us consider again the `Tree` example

  ```haskell
  instance Functor Tree where
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)
  ```
  Now, we can write
  ```haskell
  (+1) `fmap` (Node (Leaf 2) ((Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)))) ```

## Functors: more examples

* `Maybe` data type

   ```haskell
   (+1) `fmap` (Just 10)
   ```
* Generalized trees

  ```haskell
  data TreeG a = LeafG a | BranchG [TreeG a]

  instance Functor TreeG where
   fmap f (LeafG a)    = LeafG (f a) ```

  What about `BranchG`? We will use the `fmap` from lists!

  ```haskell
  fmap f (BranchG ts) = BranchG (fmap (fmap f) ts)
  ```
  The outermost `fmap` is the one corresponding to lists.

  For instance, the expression
  ```haskell
  (+1)
  `fmap` BranchG [LeafG 10,
                  BranchG [LeafG 11, LeafG 12],
                  BranchG [LeafG 13, LeafG 14, LeafG 15]] ```
  produces
  ```haskell
  BranchG [LeafG 11,BranchG [LeafG 12,LeafG 13],BranchG [LeafG 14,LeafG 15,LeafG 16]]
  ```

* To summarize:

  <div class = "alert alert-info">
  Functors simplifies boilerplate code, i.e., destructing a container in order
  to create another one!
  </div>

  <div class = "alert alert-info">
  As data types can be built from other ones, so do functors! (thus achieving modularity)
  </div>


## Multi-parameter functions map to multiple containers

* In a more general case, sometimes we would like to transform elements in a
  container (data type) based on applying a "*multi-parameter function*" to
  *multiple containers* (the quotes are there since in Haskell every function
  has exactly one argument due to curryfication)

* Keep in mind that `fmap` takes a function of type `a -> b` and not
  "multi-parameter" ones, e.g., `a -> b -> c`

* Let us take as an example to see what happens when `fmap` is used with a
  "multi-parameter" function.

  ```haskell
   mp_fmap :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
   mp_fmap f ma mb = let m_b_to_c = fmap f ma
                     in  undefined ```

  Once we apply `fmap` to the first container, we have a container (`m_b_to_c`)
  with a function of type `b -> c` in it.

  To keep applying `f`, what we need to do is to **take its partial application
  out of the container `m_b_to_c` and mapped over `mb`**, where its
  next argument is located.

  ```haskell
  mp_fmap :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  mp_fmap f ma mb = let m_b_to_c = fmap f ma
                    in case m_b_to_c of
                            Nothing -> Nothing
                            Just fa -> fmap fa mb ```

  What happens if function `f` had more arguments?

  ```haskell
  mp_fmap2 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
  mp_fmap2 f ma mb mc = case fmap f ma of
                             Nothing -> Nothing
                             Just fa -> case fmap fa mb of
                                            Nothing  -> Nothing
                                            Just fab -> fmap fab mc ```

  We would need to implement more code to extract partial application of
  functions out from containers, applied them, and wrap the result in another
  container!

  *Applicative functors are conceived to do precisely this work!*


## Applicative functors

* An *applicative functor* (or functor with application) is a functor with the following operations

  ```haskell
  class Functor d => Applicative d where
        pure  :: a -> d a
        (<*>) :: d (a -> b) -> d a -> d b  ```

* Observe that an applicative functor **is** a functor

* The pure operation creates a container with the given argument. The interesting
   operation is "application" `(<*>)`, which we can describe it graphically as
   follows

  <div class="container">
     <img class="img-responsive col-md-9"
       src="./assets/img/applicative2.png">
  </div>

  It takes a container of functions and a container of arguments and returns a
  container of the result of applying such functions.

* An applicative functor must obey the following laws

  <table class="table table-bordered">
  <thead>
    <tr>
    <th>Name</th>
    <th>Law</th>
    </tr>
  </thead>

  <tr>
  <td> Identity: </td>
  <td>  ```haskell pure id <&#42;> vv ≡ vv ```
  </td>
  </tr>

  <tr>
  <td> Composition: </td>

  <td> ```haskell pure (.) <&#42;> ff <&#42;> gg <&#42;> zz ≡ ff <&#42;> (gg <&#42;> zz) ```
  </td>
  </tr>

  <tr>
  <td> Homomorphism: </td>

  <td> ```haskell pure f <&#42;> pure v ≡ pure (f v) ```
  </td>
  </tr>

  <tr>
  <td> Interchange: </td>

  <td> ```haskell ff <&#42;> pure v ≡ pure ($ v) <&#42;> ff ```
  </td>
  </tr>

  </table>

  In the rules above, double letters indicate that the denote element is in a
  container, e.g., ff means that it is a function f inside a container, vv is a
  value which is inside a container and so on.

  One of the most interesting rules is *interchange*. Before explaining it, let
  us see the types of the expressions involved.

  ```haskell
  ff :: d (a -> b)
  vv :: d a
  pure ($ v) :: d ((a -> b) -> b)
  ```
  The rule says that instead of obtaining a `d b` as `ff <*> pure v`, where
  ``ff`` is a container with a function and `pure v` is its argument, it is
  possible to apply function `($ v)` to the container `ff`.

## Applicative Maybe

* Let us go back to our example using `Maybe`

  ```haskell
   instance Functor Maybe where
      fmap f Nothing  = Nothing
      fmap f (Just a) = Just (f a)

   instance Applicative Maybe where
       pure           = Just
       Nothing <*> vv = Nothing
       Just f  <*> vv = fmap f vv ```

* What can we do know with that?

  ```haskell
  -- xx :: Maybe String
  -- yy :: Maybe String
  pure (++) <*> xx <*> yy
  ```

  We can apply concatenation on strings store in containers. All the wrapping
  and unwrapping is handled by the applicative functor.

* Applicative functors common pattern of use is as follows.

  ```haskell
  pure f <*> xx <*> yy <*> zz ...
  ```

  More precisely, a function `f` in a container as the leftmost term follow by its container
  arguments!

  To make this pattern to look better, the applicative interface provides a
  *derived* operation called `(<$>)` which removes the `pure` from the leftmost term.

  ```haskell
  (<$>) :: Functor d => (a -> b) -> d a -> d b
  ```

  So, the expression
  ```haskell
  pure f <*> xx <*> yy <*> zz ...
  ```
  becomes
  ```haskell
  f <$> xx <*> yy <*> zz ...
  ```

## Relation with monads
[Blog on Applicative Functors](https://pbrisbin.com/posts/applicative_functors/)

* Observe what we have written using the applicative functor `Maybe`

  ```haskell
  pure (++) <*> xx <*> yy ```

* If we consider `Maybe` as a monad instead (which we defined in the previous lecture), we can
  achieve the same things.

  ```haskell
  do x <- xx
     y <- yy
     return (x ++ y) ```

* What is the difference between `Maybe` as an applicative functor or a monad?

* In the case above, both programs produce **the same result** (`x++y`). Observe
  that `x` gets bounded but it is not used until the `return` instruction -- the
  same occurs with `y`. However, the effects in a monadic program or an
  applicative one could be run in a different order.

  <div class = "alert alert-info">
  The difference between monad and applicative functors is the difference
  between *sequential* vs. *parallel* execution of side-effects
  </div>

* To appreciate this difference, let us consider a dramatic side-effect:
  *launching a missile*. We need to write a program which launches two missiles
  to a given target.

  **Monadic code**

  <div class="container">
     <img class="img-responsive col-md-9"
       src="./assets/img/missile_monad.png">
  </div>

  <div class = "alert alert-info">
  The side-effects are sequentially executed!
  </div>

  **Applicative code**

  The applicative structure does not impose an order on the execution of the
  *container arguments*. Observe that the effects could be run in parallel if
  possible.

  <div class="container">
     <img class="img-responsive col-md-9"
       src="./assets/img/missile_app.png">
  </div>

* Let see closely the types

  <div class="container">
     <img class="img-responsive col-md-9"
       src="./assets/img/monad_app_types.png">
  </div>


* What is better? Monads, Applicative Functors?

  [FUNCTIONAL PEARL Applicative
  programming with effects by C. McBride and
  R. Paterson](http://strictlypositive.org/IdiomLite.pdf)

  <div class = "alert alert-warning">
  The moral is this: if you’ve got an Applicative functor, that’s good; if
  you’ve also got a Monad, that’s even better! And the dual of the moral is this:
  if you want a Monad, that’s good; if you only want an Applicative functor,
  that’s even better!
  </div>

  <div class = "alert alert-info">
  Theory can prove that every monad is an applicative functor and that
  every applicative functor is a functor.
  </div>

  <div class="container">
     <img class="img-responsive col-md-9"
       src="./assets/img/monad-proposal.png">
  </div>

  From GHC 7.10, if you define a monad, i.e., give an instance of the type class
  `Monad`, you also need to give an instance of `Applicative` and `Functor`

  ```haskell
  class Functor d     => Applicative d where
  class Applicative d => Monad d       where ```

  In GHC 7.8, you get a warning but your program still compiles. For more
  information check the
  [Functor-Applicative-Monad](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)
  proposal.



## Not a functor

* Not every data type is a functor

  ```haskell
   type NotAFunctor = Equal
   newtype Equal a = Equal {runEqual :: a -> a -> Bool}

   fmapEqual :: (a -> b) -> Equal a -> Equal b
   fmapEqual _f (Equal _op) = Equal $ \_b1 _b2 -> error "Hopeless!" ```

* What is the problem?

  Values of type `a` are in "negative position", i.e., they are given to the
  function (not produced by it).

## A functor, not applicative

* Not every functor is applicative

  ```haskell
  data Pair r a = P r a

  instance Functor (Pair r) where
      fmap f (P r a) = P r (f a)
  ```

* Observe that the value of type `r` is kept as it is in the container (the
  functor does not create new elements, but modify existing ones)

* If we want to create a container, we need an `r` but `pure` only receives an `a`

  ```haskell
  instance Applicative (Pair r) where
     pure x  = P (error "Hopeless!") x
     f <*> v = error "Hopeless!"  ```

## Applicative, not a monad

* Not every applicative functor is a monad

  ```haskell
  data AppNotMonad a = AppNotMonad Bool

  instance Functor AppNotMonad where
     fmap f (AppNotMonad b) = AppNotMonad b  ```

  Observe that the functor does not change the container at all. In the
     definition above, `AppNotMonad b :: a` in the argument of `fmap` and
     `AppNotMonad b :: b` on the right hand side of the definition. The trick
     here is that the definition of `AppNotMonad` uses a phantom type.

* So, thanks to that phantom type we can define an applicative functor that only
  handles `AppNotMonad True` as the underlying container.

  ```haskell
  instance Applicative AppNotMonad where
    pure x = AppNotMonad True -- You choose either True or False
    AppNotMonad b1 <*> AppNotMonad b2 = AppNotMonad (b1 == b2)
  ```
  The last method of the class could have been defined as  `AppNotMonad b1` or `AppNotMonad b2`
  or any other expressions which returns the value `AppNotMonad True`.

* When defining a monad, however, the second argument of the bind (of type
  `a -> AppNotMonad b`) requires an `a` but we only have booleans in `AppNotMonad`!

   ```haskell
   instance Monad AppNotMonad where
                    return x = AppNotMonad True
       (AppNotMonad t) >>= f = error "Hopeless!"  ```

   (Function `return x` could have been also defined as `AppNotMonad False`)

   You might try to write

   ```haskell
   instance Monad AppNotMonad where
                    return x = AppNotMonad True
       (AppNotMonad t) >>= f = AppNotMonad t ```

   that type-checks, but there is (at least) one monadic law that it does not
   hold. Can you see which one?

## Structures learned so far

* Monads
  - **Sequential** construction of programs

  - Useful to implement side-effects (e.g., error handling, logging, stateful
    computations, etc.)

      * Simplify code, i.e., it hides plumbing needed to handle the side-effects)

* Applicative functors

   - Useful to apply "multi-parameters" functions to multiple container
     arguments

       * **Simplify code**, i.e., it hides the plumbing to take out functions and
         its arguments from containers, applying the function, and place the
         result back in a container.

   - Side-effects could potentially be executed in parallel

   - They are more generic than monads

* Functors

   - Useful to map functions into containers, i.e., transform data inside
     containers without breaking them.

       * **Simplify code**, i.e., it hides the plumbing of destructing the
         container to obtain the value, apply the function, and put the result
         in a container.

   - The most general structure
