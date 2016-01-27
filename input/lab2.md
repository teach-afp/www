Deadlines
---------

<table class="table table-bordered">
<thead>
<tr>
    <th>Assignment 1</th>
    <th>Date</th>
</tr>
</thead>

<tr>
    <td class="success" > [Part I](#part1) </td>
    <td class="alert-info" >  February 11th (Thursday, course week 4) </td>
</tr>

<tr>
    <td class="success" > [Part II](#part2) </td>
    <td class="alert-info" >  February 25th (Thursday, course week 6) </td>
</tr>
</table>


Description
-----------

This assignment consists of 2 parts. In the first part, you will design,
implement and test a Monad for programs that can be replayed.
In the second part use this monad to implement a simple library for writing
programs with web forms, implement an optimisation to your Replay monad that
makes it much more useful, and use your web library to implement
a simple application.


A monad for replaying computations
----------------------------------

Consider the following scenario: a program is running.
At some point, the program decides to stop, and produces a *trace*: a log of
the computation so far.

The user would later on like to resume the program, making it run from the
exact same point where it previously stopped. Not only should the program
resume from where it stopped, but all values of all variables should be
precisely the same as when the program decided to stop.
The trace should contain all information necessary to do this.

The trace can thus be used as an input trace for rerunning the program,
without repeating previously performed computations.

In this lab you will implement a monad called `Replay` for expressing such
computations, capable of stopping to ask the user questions.
This is the interface to the `Replay` monad:

```haskell
instance Monad (Replay q r)

io  :: (Show a, Read a) => IO a -> Replay q r a
ask :: q -> Replay q r r

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
```

The monad is parameterised on the types of questions (`q`) and answers
(`r`, for result). There are two primitive operations in the monad besides
`return` and `>>=`:

* `io` allows us to perform `IO` actions in our computation.
  If there is already a result for an `io` computation in the input trace,
  the computation is not performed and the result from the trace is used
  instead.
* `ask` stops the whole program to ask the user a question, returning the
  question along with the trace of the computation so far.
  The user of the program can then examine the question. When the user wants
  to continue the program they restart it with the generated trace, extended
  with an answer to the question.
  
  If the question has already been answered in the input trace, the answer
  from the trace is used instead of stopping execution, and the program
  continues on to the next `ask`.

The `run` function takes a `Replay` program and a trace (possibly produced by
a previous run of the program), and reruns the program using this trace.
If the program stops with a question `q`, the result of the run function is
`Left (q, t)`, where `t` is the new trace. If there are no more questions,
`run` results in `Right x` where `x` is the result of the whole computation.

It is important to note that `Trace` is going to be something that we can
serialise. For our purposes this means being able to convert from and to a
`String`, which we can then for instance write to a file.


<a name="example"></a>
### An example

Here is an example of a program in the `Replay` monad. We have chosen the
question and answer types to both be `String`s:

```haskell
import Data.Time

example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- io getCurrentTime
  io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)
```

The example starts by getting the current time and printing a message,
then stops the whole program asking "What is your age?".
If the program is rerun from that point with an answer, it prints another
message and then stops again, asking: "What is your name?".
If the program is rerun from that point, a message is printed,
and the total time for the whole process is calculated.

To understand how to deal with traces, here is a possible impementation for
`Trace`:

```haskell
type Trace r = [Item r]

data Item r = Answer r | Result String
  deriving (Show,Read)

emptyTrace = []
addAnswer t r = t ++ [Answer r]
```

When we run the above example, providing it with the empty trace, we get
the following result:

```
GHCi> do x <- run example []; print x
Hello!
Left ("What is your age?",[Result "1164117157",Result "()"])
```

We can see that the program printed the message, and then stopped,
with the question "What is your age?", and a trace. The trace records what
has happened in the program so far. When we have found the answer to
the question, (for example 27), we can re-run the computation,
augmenting the trace with an extra element:

```
GHCi> do x <- run example [Result "1164117157", Result "()", Answer "27"]; print x
You are 27.
Left ("What is your name?",[Result "1164117157",Result "()",Answer "27",Result "()"])
```

Again, the program prints its message, and stops with a question and a trace.
Notice that we are running the *same* program (namely example) again,
but now with a different trace!
Again, we augment that trace with the answer we want to give,
running the program again:

```
GHCi> do x <- run example [Result "1164117157", Result "()", Answer "27", Result "()",
Answer "Starbuck"]; print x
Starbuck is 27 years old
Total time: 19 seconds
Right 27
```

We can see that the program printed "Starbuck is 27 years old" on the screen,
and calculated how long time the whole process took (from the beginning).
The program then terminated normally with the result 27.

We can automate the process of re-running the program and augmenting it
with the answer every time. This is done by another run function that re-runs
the program with an answer every time the program stops and asks a question.
Here is how:

```haskell
running :: Replay String String a -> IO a
running prog = play emptyTrace
 where
  play t = do
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q,t') -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (addAnswer t' r)
      Right x -> return x
```

There is very little reason for why we would like to use this particular
run function, because it does not use the full generality of the Replay monad.
Here is how it works on the example:

```
GHCi> running example
Hello!
Question: What is your age? 59
You are 59
Question: What is your name? Adama
Adama is 59 years old
Total time: 5 seconds
```

Note that, in the above, the same program is re-run with a new trace,
each time the answer to the question is provided by the user.

<a name="part1"></a>
Part I
------

The first part of this assignment is to implement the `Replay` monad.

### Task 1: the `Replay` monad

First create a Haskell module called Replay in an empty directory,
then create a cabal library package called `replay` by running `cabal init`
and answering all questions.

Look through the resulting cabal file and make sure you understand
what it does and that your module is included in the package.
Add any dependencies that you know you will need.
Also you should relax the dependency on base to 4.* or similar.
Implement a monad `Replay` with the following interface:


```haskell
-- Types
Replay q r a
Trace r

-- Operations
instance Monad (Replay q r)
instance Show r => Show (Trace r)
instance Read r => Read (Trace r)

io  :: (Show a, Read a) => IO a -> Replay q r a
ask :: q -> Replay q r r

emptyTrace :: Trace r
addAnswer  :: Trace r -> r -> Trace r

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
```

You can decide yourself how to implement the type `Trace r`,
but it needs to be a type that can be written to and read from a file.
(Ergo, it needs to be an instance of `Show` and `Read`.)
Make sure that the [example](#example) above can actually be run correctly!

<a name="task2"></a>
### Task 2: generalise the interface
**For grades 4 and 5**

Turn your Replay monad into a monad transformer `ReplayT m`, so that any
underlying monad can be used, instead of only the IO monad.
Add a function `liftR` for lifting computations from the underlying monad.
Define `Replay` in terms of `ReplayT`:

```haskell
type Replay q r a = ReplayT IO q r a

liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
```

Remember to generalise all functions to the new interface where possible.
Consider which functions are primitive and which are derived.

**Question:** why is it not possible to make your transformer an instance of
`MonadTrans`?

**Note:** If you do this task, you should *not* submit the non-generalised
version from task 1.

### Task 3: testing the `Replay` monad

Once you have implemented your Replay monad transformer you should make sure
that it works properly. Here is a testing framework to help you get started:
[Test.hs](TODO: link to code here).
Put the framework in a subdirectory called test, then add this to your
.cabal file to create a test suite:

```
Test-Suite test-replay
    type:            exitcode-stdio-1.0
    hs-source-dirs:  test
    main-is:         Test.hs
    build-depends:   base, replay
```

If done correctly this sequence of commands should execute your test suite:

```
cabal configure --enable-tests
cabal build
cabal test
```

The idea of the framework is to test replay computations with integers as
answers and whose only IO action is to increment a counter a number of times.
We check that when running the program on a list of input integers
we get the correct result and that the counter has the right value.

You should write down enough test cases that you are confident that your
implementation is correct. Try to think about possible corner cases.

**For grades 4 and 5**

Use the generalised interface from [Task 2](#task2), and replace
the `IO` monad with a `State` monad.

**For grade 5**

Use QuickCheck to generate random test cases.
