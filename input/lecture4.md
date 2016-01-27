# Functors, Applicative, and Monads

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
  which *applies the function `(+1)` to every element of the list*, and produces
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
       src="./assets/img/applicative.png">
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
     pure x = P (error "Hopeless!") x
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

   Function `return x` could have been also defined as `AppNotMonad False`

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
