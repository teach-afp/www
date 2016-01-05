
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



## Lazy evaluation ##

  - Haskell is a lazy language
  - Expressions are evaluated *at most once*
  - Expressions are evaluated *only when needed*
  - Expressions are never evaluated twice

(We will explore more in detail what this means)

## Observing when expressions get evaluated ##


```haskell
fun :: Maybe Int -> Int
fun mx  | isNothing mx   = 0
        | otherwise      = x + 3
 where
  x = fromJust mx

fromJust :: Maybe a -> a -- also available in module Data.Maybe
```

adfasdf
sdafasd

que esta pasando?
