
## This course ##

* Advance Programming Language Features
  - Type systems
  - Programming techniques

* In the context of Functional Programming
  - Haskell

* Applications
  - Signals processing, graphics, web programming, security
  - Domain Specific Languages

## Self study ##

* You need to read *yourself*

* Find out information *yourself*

* *Solve* problems *yourself*

* With a lo of help from us!
  - All information is on the web page for the course

* [Discussion board](https://groups.google.com/forum/#!forum/afp2016)

* Office hours
  - A few times a week
   <div class="alert alert-danger">
   Hours coming soon!
   </div>

## Organization ##

* **2** Lectures per week
  - Including a few guest lectures
  - Two exercise sessions (we solve, for example, previous exams)

* **3** Programming assignments (labs)
  - Done in pairs (use the discussion groups to pair up)
  - No scheduled lab supervision (use the office hours instead!)

* **1** Written exam
  <div class="alert alert-info">
  **Final grade: 60% labs + 40% exam**
  </div>

## Getting help ##

* Course homepage
  - It should be comprehensive -- complain if it is not!

* Discussion board (Google group)
  - Everyone should become a member
  - Discuss general topics, find lab partner, etc.
  - **Don't post (partial or complete) lab solutions

* Send e-mails to teachers (myself and the assistants)
  - Organizational help, lectures, etc. (Lecturer)
  - Specific help with programming labs (Assistants)

* Office hours
  - A few times a week
   <div class="alert alert-danger">
   Hours coming soon!
   </div>

## Recalling Haskell ##

* Purely functional language
  - Functions vs. Actions
  - Referential transparency

* Lazy evaluation
  - Things are evaluated at most once

* Advance (always evolving) type system
  - Polymorphism
  - Type classes
  - Type families
  - etc.

## Functions vs. Actions ##

* Consider
```haskell
f :: String -> Int
```
Only the knowledge about the string is needed to *produce* the result. We say
that `f` is a pure function.

* Input and output are key for real world programs!

* Haskell has a distinctive feature with respect to other programming languages
  <div class="alert alert-info">
  Pure code is separated from that which could affect the external world!
  </div>

* How?
  <div class="alert alert-info">
  Types!
  </div>

* Code which has side-effects in the real world has type `IO a` (for some `a`)
  ```haskell
  g :: String -> IO Int
  ```
  As `f`, this function produces an *action* which, when executed, produces an
  integer. However, it might

  - **use anything** to produce it, e.g., data found in files, user input,
    randomness, and

  - **modify anything**, e.g., files, send packages over the network, etc.

## Programming with `IO` ##

[code](https://bitbucket.org/russo/afp-code/src/76efb6f9850e4f82ce7b3ef6724b03768a3c2a1c/L1/Lect1.hs?at=master&fileviewer=file-view-default)

* Interacting with the user

  ```haskell
  hello :: IO ()
  hello =
    do putStrLn "Hello! What is your name?"
       name <- getLine
       putStrLn ("Hi, " ++ name ++ "!")
  ```

* Let us write a program that enumerates and prints a list of strings.

  ```bash
  > printTable ["1g saffran", "1kg (17dl) vetemjöl", "5dl mjölk",
                "250g mager kesella", "50g jäst", "1.5dl socker",
                "0.5tsk salt"]
  1: 1g saffran
  2: 1kg (17dl) vetemjöl
  3: 5dl mjölk
  4: 250g mager kesella
  5: 50g jäst
  6: 1.5dl socker
  7: 0.5tsk salt
  >
  ```

  ```haskellln
  printTable :: [String] -> IO ()
  printTable = prnt 1  -- Note the use of partial application
   where
    prnt :: Int -> [String] -> IO ()
    prnt _i []      = return ()
    prnt i (x:xs)   = do putStrLn (show i ++ ": " ++ x)
                         prnt (i+1) xs
  ```

* IO actions are first class, i.e., you can pass them around and store them as
  any other value.

* Can we write `printTable` differently?

  - Let us create a list of actions and then sequentially show them.

  ```haskellln
  printTable2 :: [String] -> IO ()
  printTable2 xs = sequence_ [
                               putStrLn (show i ++ ":" ++ x)
                               | (x,i) <- xs `zip` [1..length xs]
                             ]
  sequence_ :: [IO ()] -> IO () -- Prelude
  ```

## Referential transparency ##

* What is it?

  "...An expression may contain certain 'names' which stand for unknown
  quantities, but it is normal in mathematical notation to presume that
  different occurrences of the same name refer to the same unknown
  quantity... "

  "...the meaning of an expression is its value and there are no other effects,
  hidden or otherwise, in any procedure for actually obtaining it. Furthermore,
  the value of an expression depends only on the values of its constituent
  expressions (if any)..."

  Source:
  [Introduction to Functional Programming by R. Bird and P. Walder (First
  edition, page 4)](https://www.google.se/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwiAjceq75LKAhUmJnIKHdT3BZcQFggeMAA&url=http%3A%2F%2Fusi-pl.github.io%2Flc%2Fsp-2015%2Fdoc%2FBird_Wadler.%2520Introduction%2520to%2520Functional%2520Programming.1ed.pdf&usg=AFQjCNF7fvckPQD0PLe0f8aZ4OFxZ41yKQ&sig2=KdJ2BqyfK__SLvzRvo0Q6g)

* What does it buy us?

  - Equational reasoning, i.e., expressions can be freely changed by others that
    denote the same value.

   <div class="alert alert-danger">
   Do the graphic!
   </div>

  - A classic property for function `reverse`:
  ```haskell
  reverse (reverse xs) = xs
  ```

* What about programs with I/O?

  - In Haskell, expressions of type `IO a` (for some type `a`) are pure
    expressions which **denote** (describe) I/O actions.

  - In other words, an expression of type `IO a` is not the computation itself
    but rather a *pure* description of it.

  - This enable us to also do equational reasoning on IO actions.

  - Unfortunately, we do not always have the definition of the functions
    describing I/O effects (e.g., `putStrLn`, `getLine`, etc.). Nevertheless, we
    can still do some reasoning based on the underlying structure of `IO`
    (monad). For instance, the code

    ```haskell
    do putStrLn "Hi!"
       name <- getLine
       return ("hi!" ++ name)
    ```

    is equivalent to

    ```haskell
    do putStrLn "Hi!"
       name <- getLine
       return 42
       return ("hi!" ++ name)
    ```

## Referential transparency in practice ##

* In practice, changing expressions by others denoting the same values might have
  consequences in
  - Memory consumption
  - Performance
  - Energy consumption
  - etc.

* Evaluation of expressions often trigger a lot of side-effects (memory
   allocation, garbage collector, etc.) even though they are pure.

   <div class="alert alert-info">
   That expressions denote the same value does not mean that they are equally
   convenient to use in practice!
  </div>

## Evaluation orders ##

* Eager evaluation

  - ML uses this strategy
  - It reduces variables as soon as they get bound, e.g., it reduces functions'
    arguments first.
    <div class="alert alert-info">
    Graph in lambda calculus
    </div>
    <table class="table table-bordered">
    <thead>
    <tr>
    <th>Eager evaluation</th>
    </tr>
    </thead>

    <tr class="success">
    <td> Programmer dictates the execution order </td>
    </tr>

    <tr class="success">
    <td> The runtime is usually small  </td>
    </tr>

    <tr class="danger">
    <td> It promotes early error propagation </td>
    </tr>

    <tr class="danger">
    <td> Evaluation of unnecessary expressions </td>
    </tr>

    <tr  class="danger">
    <td> Programmers need to organize the code for optimal
    execution based on the reduction order </td>
    </tr>
    </table>


* Lazy evaluation

  - Haskell is a lazy language
  - Expressions are evaluated *at most once*
  - Expressions are evaluated *only when needed*
  - Expressions are never evaluated twice

  (We will explore more in detail what this means)

  <div class="alert alert-info">
  Graph in lambda calculus
  </div>

## Observing evaluations in Haskell ##

* Use `error "message"` or `undefined` to see whether something gets evaluated

   ```haskell
   testLazy2 = head [3, undefined, 17]
   testLazy3 = head (3:4:error "no tail")
   testLazy4 = head [error "no first elem", 17, 13]
   testLazy5 = head (error "no list at all")```

## Lazy evaluation: skipping unnecessary computations ##

* Consider the following functions

  ```haskell
  expn :: Integer -> Integer
  expn n | n <= 1    = 1
  | otherwise = expn (n-1) + expn (n-2)

  choice :: Bool -> a -> a -> a
  choice False  f  _t  =  f
  choice True   _f  t  =  t
  ```

* Function `expn` is "expensive" to compute. Do you see why?

* What does it happen when running?

  ```haskell
  testChoice1 :: Integer
  testChoice1 = choice False 17 (expn 99)

  testChoice2 :: Integer
  testChoice2 = choice False 17 (error "Don't touch me!")
  ```

## Lazy evaluation: programming style ##

* Programs separate the
  - **construction**
  - and **selection** of data for a given purpose

* Modularity: "It makes it practical to modularise a program as a generator
  which constructs a large number of possible answers, and a selector which
  chooses the appropriate one."

  [Why Functional Programming Matters by John Hughes](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf)

## Lazy evaluation: when is a value "needed"? ##

* An argument is evaluated when a pattern match occurs

  ```haskell
  strange :: Bool -> Integer
  strange False = 17
  strange True  = 17

  testStrange = strange undefined
  ```

* *Primitive functions* also evaluate their arguments

## Lazy evaluation: at most once? ##

```haskelln
ff :: Integer -> Integer
ff x = (x - 2) * 10

foo :: Integer -> Integer
foo x = ff x + ff x

bar :: Integer -> Integer -> Integer
bar x y = ff 17 + x + y

testBar =  bar 1 2 + bar 3 4
```

* `ff x` gets evaluated twice in `foo x`

* `ff 17` is evaluated twice in `testBar`

* Why is that?

* In lazy evaluation, *bindings* are evaluated at most once!

  - We can adapt `foo` above to evaluate `ff x` at most once by introducing a
  local binding

  ```haskell
  foo :: Integer -> Integer
  foo x = ffx + ffx
  where ffx = ff x
  ```

  - The evaluation happens *at most once* in the corresponding scope!

  - What about `f 17`? How can we change `bar` to evaluate it at most once?

  ```haskell
  bar :: Integer -> Integer -> Integer
  bar x y = ff17 + x + y

  ff17 :: Integer
  ff17 = ff 17 ```

     We introduce a top-level binding, which are really evaluated at most once.

## Lazy evaluation: infinite lists ##

* Because of laziness, Haskell is able to denote infinite structures

* They are not compute completely!

* Instead, Haskell only computes the needed parts from them

* Infinite lists examples
  ```haskellln
  take n [3..]
  xs `zip` [1..]
  ```

* We can write "generic code" which gets "instantiate" to the appropriated
  case.

  ```haskell
  printTable3 :: [String] -> IO ()
  printTable3 xs =
  sequence_ [ putStrLn (show i ++ ":" ++ x)
  | (x,i) <- xs `zip` [1..]
  ]
  testTable3 = printTable3 lussekatter
  ```
  Observe that `zip` takes an infinite lists but it will only use as many
   elements as the `length xs`

* Other examples

  -  Raising functions to a positive power
  ```haskell
  iterate :: (a -> a) -> a -> [a]
  iterate f x = x : iterate f (f x)
  ```

  ```bash
  > iterate (2*) 1
  [1,2,4,8,16,32,64,128,256,512,1024,...]
  ```

  - Repeating a number infinitely

    ```haskell
    repeat :: a -> [a]
    repeat x = x : repeat x
    ```

  - Creating periodic lists
    ```haskell
    cycle :: [a] -> [a]
    cycle xs = xs ++ cycle xs
    ```

   - Alternative definitions

   ```haskell
   repeat :: a -> [a]
   repeat = iterate id

   cycle :: [a] -> [a]
   cycle xs = concat (repeat xs)
   ```

## Lazy evaluation: infinite lists exercises ##

* Problem: let us define the function

  ```haskell
  replicate :: Int -> a -> [a]
  replicate = ?
  ```
  such that
  ```bash
  > replicate 5 'a'
  "aaaaa"
  ```

  ```haskell
  replicate :: Int -> a -> [a]
  replicate n x = take n (repeat x)
  ```

* Problem: grouping lists elements into lists of equal size

  ```haskell
  group :: Int -> [a] -> [[a]]
  group = ?
  ```
  ```bash
  > group 4 "thisthatok!!"
  ["this", "that", "ok!!"]
  ```

  ```haskell
  group n = takeWhile (not . null)
        . map (take n)
        . iterate (drop n)
  ```
  Function composition `(.)` connects data processing "stages" -- like Unix
  pipes!

* Problem: prime numbers

  ```haskell
  primes :: [Integer]
  primes = ?
  ```

  ```bash
  > take 4 primes
  [2,3,5,7]
  ```

  ```haskell
  primes :: [Integer]
  primes = sieve [2..]
   where
    sieve (p:xs)  = p : sieve [ y | y <- xs, y `mod` p /= 0 ]
    sieve []      = error "sieve: empty list is impossible"
  ```

  This algorithm is commonly mistaken for Eratosthenes' sieve -- see that paper
  [The Genuine Sieve of
  Eratosthenes](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf) for more
  details.



<table class="table table-bordered">

<thead>
      <tr>
         <th>Lazy evaluation</th>
      </tr>
</thead>

<tr class="success">
  <td> Avoid unnecessary computations </td>
</tr>

<tr class="success">
  <td> It provides error recovery </td>
</tr>

<tr class="success">
  <td> It allows to describe infinity data structures </td>
</tr>

<tr class="danger">
  <td> It is hard to do complexity analysis </td>
</tr>

<tr class="danger">
  <td> It is hardly suitable for time-critical operations </td>
</tr>

<tr  class="danger">
  <td>  </td>
</tr>
</table>
