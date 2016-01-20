# Monads and Functors


## Some programming features in imperative languages
* Error handling
  - Exceptions (e.g., Java)
  - Set some global variable and abort computation (e.g., `errno` in C)
* Logging
  - Writing to a file
  - Having a global reference where to store the log

## What about in Haskell? (Pure functional language)
  - Data flow is explicit!
  - Let us take a concrete example

     ```haskell
     -- | Abstract syntax
     data Expr = Con Int | Div Expr Expr

     -- | Simple interpreter for simple arithmetic expressions
     interp :: Expr -> Int
     interp (Con i)     = i
     interp (Div e1 e2) = i1 `div` i2
         where i1 = interp e1
               i2 = interp e2

     -- | Succesful division
     ex_ok    = Div (Con 10) (Con 5)
     ```

  - What if we accidentally divide a number by `0`?
    ```haskell
    -- | Crashing!
    ex_crash = Div (Con 1) (Con 0)
    ```
    Everything crashes!

## Handling errors explicitly

- The error handling mechanism that we consider is

  <div class="alert alert-info">
      If something goes run, the *whole* computation aborts!
  </div>

  (We do not consider recovery options)
- Let us modify the interpreter to implement our error handling mechanism
   ```haskell
   data E a = Value a | Wrong

   interpE :: Expr -> E Int
   interpE (Con i)     = Value i
   interpE (Div e1 e2) = case maybe_i1 of
                           Wrong -> Wrong
                           Value i1 -> case maybe_i2 of
                                       Wrong -> Wrong
                                       Value i2 -> Value $ i1 `div` i2
       where maybe_i1 = interpE e1
             maybe_i2 = interpE e2
   ```

* Basically, **every recursive call needs to be checked for errors**! If one of
  them fails, then the program should abort.
* Consider what would happen if `Expr` had many other recursive constructors

## Logging

* We want to count the number of divisions that our interpreter is performing.
  - After all, we know that they are more expensive then other arithmetic
    operations.
  - This information could be used to improve future interpreter optimizations

  <div class="alert alert-info">
      We count the number of divisions computed by an expression
  </div>

- Let us modify the interpreter to log the number of divisions.
  ```haskell
   data L a = L (a, Int)

   interpL :: Expr -> L Int
   interpL (Con i)      = L (i, 0)
   interpL (Div e1 e2)  = L (i1 `div` i2, divs1 + divs2)
      where L (i1, divs1) = interpL e1
            L (i2, divs2) = interpL e2   ```

* Basically, the results of **every recursive call*** needs to be inspected to
  obtain the number of divisions performed by them.
* Consider what would happen if `Expr` had many other recursive constructors

## Side-effects & pure functional programming

<table class="table table-bordered">
  <thead>
    <tr>
    <th>Imperative programming</th>
    <th>Pure functional programming</th>
    </tr>
  </thead>

  <tr>
  <td> Error handling </td>
  <td> Plumbing (check if any subcomputation has failed) </td>
  </tr>
  <tr>
  <td> Logging </td>
  <td> Plumbing (consider subcomputations' logs) </td>
  </tr>
</table>


## Monads
* What are monads?
  - Special data structures useful to write programs
  - Any program?
    <div class="alert alert-info">
     Programs with side-effects!
    </div>

* What is so special about such data types?
    <div class="alert alert-danger">
     It is general! (supports many different side-effects)
    </div>
    <div class="alert alert-danger">
     Monads hide the plumbing! (simplifies code)
    </div>

* How is so general?
   <div class="alert alert-danger">
    Monads control the order of evaluation
   </div>
   Roughly speaking, the trick is how the `;` is defined! Different definitions
   for `;` allows to handle different side-effects.

## Construction of programs

* Programs can be conceived as a *sequence* of instructions put together

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/monad_seq.png">
  </div>

* **Every instruction computes some data**

* Importantly, **an instruction can affect what subsequent ones do**. More
  precisely,

  <div class="container">
     <img class="img-responsive col-md-8"
      src="./assets/img/monad_seq_2.png">
  </div>

  Functions `f(r1)` and `g(r2)` encode the dependency among instructions.

* What are the types for `f` and `g`, respectively?
  - Let us first introduce the types for instructions.

    <div class="container">
     <img class="img-responsive col-md-8"
      src="./assets/img/monad_seq_type_instr.png">
    </div>

* What is the type of the connector `(>>=)`?

    <div class="container">
     <img class="img-responsive col-md-6"
      src="./assets/img/monad_bind.png">
    </div>

    <div class="container">
     <img class="img-responsive col-md-10"
      src="./assets/img/monad_bind2.png">
    </div>

## Systematic error handling

* The connector is placed in the "right place" to abort any computation as soon as
  an instruction fails

    <div class="container">
     <img class="img-responsive col-md-10"
      src="./assets/img/monad_error.png">
    </div>

* A data type for handling errors

  ```haskell
  data E a = Value a | Wrong
  ```

* Is `E a` a monad? Is `E a` such special data type?
  - Not yet, we need to define the connector `(>>=)`
  -
    ```haskell
    class Monad m where
       return :: a -> m a
       (>>=)  :: m a -> (a -> m b) -> m b
    ```

    ```haskell
    instance Monad E where
      return = Value
      Wrong   >>= f = Wrong
      Value a >>= f = f a
    ```

    Monad `E` is known as the `Maybe` monad!

## Error handling in the interpreter

-

    ```haskell
    m_interpE :: Expr -> E Int
    m_interpE (Con i)     = return i
    m_interpE (Div e1 e2) = m_interpE e1 >>=
                            (\i1 -> m_interpE e2 >>=
                                   (\i2 -> return (i1 `div` i2)))
    ```

    Observe that the code has no traces of error handling, e.g., pattern matching
    on every recursive call. It is handled by the monad!
