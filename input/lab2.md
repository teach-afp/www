Deadlines
=========

<table class="table table-bordered">
<thead>
<tr>
    <th>Assignment 2</th>
    <th>Date</th>
</tr>
</thead>

<tr>
    <td class="success">[Part I](#part-i)</td>
    <td class="alert-info">Sunday, February 11th (course week 4)</td>
</tr>

<tr>
    <td class="success">[Part II](#part-ii)</td>
    <td class="alert-info">Sunday, February 25th (course week 6)</td>
</tr>
</table>


Description
===========

This assignment consists of 2 parts. In the first part, you will design,
implement and test a monad for programs that can be replayed.
In the second part use this monad to implement a simple library for writing
programs with web forms, implement an optimisation to your replay monad that
makes it much more useful, and use your web library to implement
a simple application.


A monad for replaying computations
==================================

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
`return` and `(>>=)`:

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

It is important to note that a `Trace` is going to be something that we can
serialise. For our purposes this means being able to convert from and to a
`String`, which we can then for instance write to a file.


An example
----------

Here is an example of a program in the `Replay` monad. We have chosen the
question and answer types to both be `String`s:

```haskell
import Data.Time (getCurrentTime, diffUTCTime)

import Replay

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

When we run the above
[example](https://bitbucket.org/russo/afp-code/src/master/assignment2/replay-0.1.0.0/executable/Example.hs),
providing it with the empty trace, we get
the following result:

```haskell
*Main> x <- run example emptyTrace
Hello!
*Main> x
Left ("What is your age?",[Result "1164117157",Result "()"])
```

We can see that the program printed the message, and then stopped,
with the question "What is your age?", and a trace. The trace records what
has happened in the program so far. When we have found the answer to
the question, (for example 27), we can re-run the computation,
augmenting the trace with an extra element:

```haskell
*Main> let getTrace (Left (_,t)) = t    -- non-exhaustive
*Main> x <- run example (addAnswer (getTrace x) "27")
You are 27
*Main> x
Left ("What is your name?",[Result "1164117157",Result "()",Answer "27",Result "()"])
```

Again, the program prints its message, and stops with a question and a trace.
Notice that we are running the *same* program (namely example) again,
but now with a different trace!
Again, we augment that trace with the answer we want to give,
running the program again:

```haskell
*Main> x <- run example (addAnswer (getTrace x) "Starbuck")
Starbuck is 27 years old
Total time: 19 seconds
*Main> x
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
      Left (q,t2) -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (addAnswer t2 r)
      Right x -> return x
```

There is very little reason for why we would like to use this particular
run function, because it does not use the full generality of the replay monad.
Here is how it works on the example:

```haskell
*Main> running example
Hello!
Question: What is your age? 59
You are 59
Question: What is your name? Adama
Adama is 59 years old
Total time: 5 seconds
59
```

Note that, in the above, the same program is re-run with a new trace,
each time the answer to the question is provided by the user.


Part I
======

The first part of this assignment is to implement the `Replay` monad.

Task 1: The `Replay` monad
--------------------------

First create a Haskell module called "Replay" in an empty directory,
then create a cabal library package called
["replay"](https://bitbucket.org/russo/afp-code/src/master/assignment2/replay-0.1.0.0/replay.cabal)
by running `cabal init`
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
Make sure that the [example](#an-example) above can actually be run correctly
and that the [monad
laws](https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Monad)
are satisfied.

Task 2: Generalise the interface
--------------------------------

**For grades 4 and 5**

Turn your replay monad into a monad transformer `ReplayT m`, so that any
underlying monad can be used, instead of only the IO monad.
Add a function `liftR` for lifting computations from the underlying monad.
Define `Replay` in terms of `ReplayT`:

```haskell
type Replay q r a = ReplayT IO q r a

liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
```

Remember to generalise all functions to the new interface where possible.
Consider which functions are primitive and which are derived.

<div class="alert alert-info">
**Question**: Why is it not possible to make your transformer an instance of
[`MonadTrans`](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Class.html#t:MonadTrans)?
</div>

**Note:** If you do this task, you should *not* submit the non-generalised
version from task 1.

Task 3: Testing the `Replay` monad
----------------------------------

Once you have implemented your replay monad transformer you should make sure
that it works properly. Here is a small testing framework to help you get
started: [Test.hs](https://bitbucket.org/russo/afp-code/src/master/assignment2/replay-0.1.0.0/test/Test.hs).
Put the framework in a subdirectory called "test", then add this to your
.cabal file to create a test suite:

```bash
Test-Suite test-replay
    type:            exitcode-stdio-1.0
    hs-source-dirs:  test
    main-is:         Test.hs
    build-depends:   base, replay
```

If done correctly this sequence of commands should execute your test suite:

```bash
cabal configure --enable-tests
cabal build
cabal test
```

The idea of the framework is to test replay computations with integers as
answers and whose only IO action is to increment a counter a number of times.
We check that when running the program on a list of input integers
we get the correct result and that the counter has the right value.

You should write down enough test cases that you are confident that your
implementation is correct. Try to think about possible corner cases
and concrete instances of the monad laws.

**For grades 4 and 5**

Use the generalised interface from [task 2](#task-2:-generalise-the-interface), and replace
the `IO` monad with a `State` monad so that `runProgram` can be given
the non-monadic type `Program -> Input -> Result`.

**For grade 5**

Use QuickCheck to generate random test cases.


Part II
=======

In the second part of the assignment you will extend your library with a
web programming DSL on top of the replay monad and write an example
application.

Web programming
---------------

In this section we will explore programming web forms using the replay monad.
We suggest to use the light weight Haskell web server
[scotty](https://hackage.haskell.org/package/scotty).

A simple but dated way to implement web-based systems is as follows.
A web form is sent to the user. The user fills in some information and sends
it back to the server.
The server looks at it and sends new content, which is filled by the user,
and so forth.

A problem with programming web forms is the user's state: the inputted data so
far, and the replies from the web server.
It needs to be stored somewhere: either in the server, or in the client.

If it is saved in the server each client needs to somehow be identified,
and the server state code can get quite complex: it is not clear what to do
when the user uses the browser's "Back" button to go back to an earlier state,
or the browsers "Clone" button, often resulting in web pages with inconsistent
information and a really poor user experience.

In this part, we will explore the second alternative: store the state
(or trace, if you will) on the client using the `Replay` monad.

Right below is a very simple web form program in Haskell using scotty.
It produces a very simple HTML page, which contains a form.
The user can provide information in the form and click OK.
The program then gets the input of the form, and sends back a new web page.
The client browser then reloads with this new web page.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty     (ActionM, scotty, get, post, rescue, html, param)
import Data.Text.Lazy (Text)

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = do
        i <- getInput
        html (page i)

    getInput :: ActionM Text
    getInput = param "text_input_id" `rescue` \ _ -> return ""

    page :: Text -> Text
    page s = mconcat $
        [ "&lt;html&gt;&lt;body&gt;"
        , "&lt;p&gt;Input was: ", s, "&lt;/p&gt;"
        , "&lt;form method=post&gt;"
        , "&lt;p&gt;Type something here:&lt;/p&gt;"
        , "&lt;input name=text_input_id&gt;"
        , "&lt;input type=submit value=OK&gt;"
        , "&lt;/form&gt;"
        , "&lt;/body&gt;&lt;/html&gt;"
        ]
```

[This program](https://bitbucket.org/russo/afp-code/src/master/assignment2/replay-0.1.0.0/executable/TestScotty.hs)
can either be compiled and then run, or you can just issue
`runghc TestScotty.hs`. It will run a web server locally on port 3000,
so you should be able to access it with your web browser on address
http://localhost:3000/.

Whatever is filled in in the form is given as input to the scotty program
when the user presses OK. Try experimenting with adding more `<input>` tags to
the form and see how this affects the program.
Both [GET and POST](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods) requests are supported. Try changing the method in
the form tag to `get`.
You should see the inputted value in the
[query string](https://en.wikipedia.org/wiki/Query_string) of the address.

*Make sure that you can run the above program and understand what is going on
before you continue!*


Task 1: A library for web forms
-------------------------------

The idea is now that you will now extend your replay library with web form
programming, based on your `Replay` monad.
This library should have the following interface:

```haskell
type Web a = Replay Form Answers a

type Form = ?

type Answers = ?

runWeb :: Web ? -> ActionM ()
```

In other words, the library provides a monad `Web` that is used to communicate
with a user, by exchanging web-pages, and receiving answers.

The idea is that the programmer can send HTML forms to the user by using `ask`.
When the user enters data in the form and sends it back, the `Web` program is
re-run up to the point where `ask` was used, and continues with the answer
that was gotten from the user.

In order for this to work, you need to do the following:

* Decide how to represent the type `Form`. This somehow needs to be
  a representation of a web form, that later on will be sent to the user.
  You must support forms with an arbitrary number of fields: it is not enough
  to give a web interface like the simple string-based example from the
  introduction.

* Decide how to represent the type `Answers`. This needs to represent the answers
  you get from the user. It needs to be able to hold all the values filled in by the user.

* Decide how to implement the function `runWeb`.
  The implementation will be very similar to that of the function `running`,
  but it also needs to take care of some web stuff. In particular:

  - Run the program in the scotty monad `ActionM` with the right trace,
    and grabbing the result, which will be a question (web form) and a trace.
    You can run `IO` computations in this monad using [`liftIO`](https://hackage.haskell.org/package/base/docs/Control-Monad-IO-Class.html#v:liftIO).
    This can also be used to make your server print debug information.

  - Store the trace as *hidden* information on the generated web form.
    Hidden information can be introduced by adding a tag like
    `<input type="hidden" name="name" value="value">` in a form.

    The value of the hidden input fields may not support certain characters,
    such as `"` and `<`. You might need to encode it.
    One idea is to use [base64 encoding](https://en.wikipedia.org/wiki/Base64),
    for which there exists Haskell implementations:
    [base64-string](https://hackage.haskell.org/package/base64-string).
    Note that the encoding function in this implementation inserts line breaks
    every 76 characters!

  - The scotty library uses [Data.Text.Lazy](https://hackage.haskell.org/package/text/docs/Data-Text-Lazy.html).
    You can convert to and from `String`s using `pack` and `unpack`.

  - After the web page is generated, it is sent to the user from scotty.
    When the answer comes back from the user the `Web` program is restarted,
    with the input from the user, including the previous trace stored in the
    `hidden` field. (If there is no hidden trace, use the empty trace.)

    The function `runWeb` needs to retrieve all this information, and re-run
    the `Web` program with the trace, providing the answer to the question
    that was asked.

You may provide any extra functionality, or change the type `Web`
if you feel the need for it.


Task 2: Optimising the `Replay` monad
-------------------------------------

**For grades 4 and 5**

The `Replay` monad remembers all results ever computed,
even if they are not affecting the current computation anymore.
This is not very space-efficient.

For example, if we would ask for the user's age like this:

```haskell
askAge :: Replay String String Integer
askAge = do
  birth <- askRetry "What is your birth year?" readMaybe
  now   <- io getCurrentYear
  return (now - birth)
  where
    getCurrentYear = getYear <$> getCurrentTime
    getYear        = let fst (x,_,_) = x in fst . toGregorian . utctDay

askRetry :: q -> (r -> Maybe a) -> Replay q r a
askRetry q p = go
  where
    go = do
      r <- ask q
      -- ask again if answer 'r' does not satisfy condition 'p'
      maybe go return (p r)
```

Then every time we use this function both results would be remembered
in the trace forever, although we know we will never actually use the
fact that we now know the user's birth year and the current year.
Instead, we would like to just remember the age, and *shortcut* the trace
there.

Implement a function:

```haskell
cut :: (Monad m, Read a, Show a) => ReplayT m q r a -> ReplayT m q r a
```

The idea with this function is that `cut m` should produce the same
results as `m`; it only affects the replay behaviour.
Namely, when we replay the computation `cut m` and it has completed,
only the result of `m` is remembered, and none of the things that happened
inside `m` to make this happen.

Thus the user can use `cut` to annotate their `Replay` programs and make them
more space-efficient. For example, `askAge` above could be implemented as
follows:

```haskell
askAgeCut :: Replay String String Integer
askAgeCut = cut askAge
```

In short, when you replay `cut m`, one of three things can happen.

1. The replay-trace has not been here before. In this case,
   we enter `m` and look inside.

2. The replay trace has been here before, but the last time we ran the program,
   somewhere inside `m` we stopped at an occurrence of `ask`.
   In this case, we enter `m` and trace normally.

3. The replay trace has been here before and completed the computation `m`.
   In this case, we never look inside `m`, but the trace knows what the
   result of `m` was and we short cut the tracing behaviour.

In order to implement `cut`, you need to adapt your type `Trace`.
There needs to be a way for the trace to keep track of the whether we should
enter `m`, or if we already know the result of `m`.
Don't forget to add test cases for `cut`.
Again, think about corner cases such as `cut (return 0)`.


Task 3: An interesting web program
----------------------------------

Implement a more realistic example that uses your web library.
Add it as an executable called
["web"](https://bitbucket.org/russo/afp-code/src/master/assignment2/replay-0.1.0.0/replay.cabal#replay.cabal-38)
in your cabal package,
similarly to how the example in lab 1 was implemented.
The following elements should occur in your program:

* The interaction with the user should go in several stages, and information
  from previous interactions should be used later on.

* There should be a way for the user to make mistakes in filling out the
  forms, and a way for your program to point this out and to give the user
  a chance to fix the mistakes.
  For instance, when asking the user about their age you should check
  that the answer is a number and if it is not, scold the user
  appropriately and let them try again.

* **For grade 4 and 5**
  You should use `cut` at the right places, to minimise the size of your
  traces.

Hint: remember that the monads scotty provides (`ActionM` and `ScottyM`) are
instances of `MonadIO` and thus supports `IO` actions, so your program can
interact with the server machine in any way.
For instance you could make a little command shell that first takes commands
such as `ls`, `cd` and `cat`. For some commands it will then require
parameters to be supplied (i.e. a file name).
It then displays the contents of directories and files.

Another idea would be one that fetches a given URL and transforms it in some
way, for instance by asking the user for a two words and replacing one with
the other. It then displays the modified page or writes it to a file on
the web server and displays a link.

A third idea is to implement a simple small spreadsheet matrix, where entries
may either be numbers, or expressions referring to other cells (or blocks of
them).

These are considered ambitious to very ambitious for the purpose of grading;
the minimal requirements are lower.


More information
================

(Feel free to send in suggestions on other good resources to put here.)

* For more information on HTML, there are lots of tutorials to be found on
  the web. Here is a reasonable one:
  [HtmlCodeTutorial](https://www.html.com/).

* Query strings can not be arbitrarily long ([RFC2616](https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.2.1)).
  It is therefore suggested to use `POST` instead of `GET`.
  However, browsers by default do make a `GET` request when entering a
  web page from elsewhere than from a form, so your library needs to
  support `GET` at least when loading the page initially.

* Don Stewart's guide on [How to write a Haskell Program](https://wiki.haskell.org/How_to_write_a_Haskell_program)
  covers cabal in detail, but is a few years old.


Extra assignments
=================

These are completely voluntary, you can get grade 5 on the assignment
without doing any of these.

* Add a layer for dealing with forms to your `Web` monad.
  For example, it would be nice to not have give explicit names (as strings)
  to the form components. Instead, one can imagine "creating" form components
  (such as input boxes and buttons) in the `Web` monad, and being able to
  ask for their value after the user has entered the form.

* Is it possible for your DSL to support both the web front end and some other,
  like [gtk2hs](https://hackage.haskell.org/package/gtk) or
  [Haste](https://haste-lang.org)?

* To protect your program from being fooled, add some form of encryption and/or
  verification of your traces.
  What safety guarantees can you give with this protection?

* Add a form of compression for traces, to avoid excessive amounts of data
  being sent to and fro. Simply using `show` leads to lots of extra clutter.
  One simple thing to do is to avoid printing the full constructor names
  from the trace type.
  Another thing is to capture repetition; many traces will have lots of
  repetitions of `Result "()"` for example.
  Would being able to say `Repeat 24 (Result "()")` bring down this cost
  significantly, or is it a case of premature optimisation?
  What happens if you try a compression algorithm?

* Similarly, examine the possibilities to use a more suited form of
  serialisation than using `Read` and `Show`.
  What libraries can you choose from here?

* These days, [Ajax](https://en.wikipedia.org/wiki/Ajax_%28programming%29) is
  much more common than page-reloading forms.
  What would need to be passed from the scotty back end in such a setting?
  Would you change the serialisation format?
  Implement a small prototype.
  Spend minimal time on the JavaScript programming, or use
  [Haste](https://haste-lang.org) for the browser part.


Submission
==========

Clean code
----------

Before you submit your code, *clean it up!* Submitting clean code is
*really important*, and simply the polite thing to do.
After you feel you are done, spend some time on cleaning your code;
make it simpler, remove unnecessary things, etc.
We will reject your solution if it is not clean. Clean code:

* Does not have long (> 80 characters) lines
* Has a consistent layout
* Has type signatures for all top-level functions
* Has good comments for all modules, functions, data types and instances. The comments should look good when compiled to HTML with Haddock.
* Has no junk (junk is unused code, code which is commented out, unnecessary comments)
* Has no overly complicated function definitions
* Does not contain any repetitive code (copy-and-paste programming)


What to submit
--------------

Your submission needs to include the following information:

* **For [part I](#part-i):**
  your cabal package, containing:
  - a library containing your `Replay` monad and
  - a test suite for your library.

* **For [part II](#part-ii):**
  your cabal package from part I, extended with:
  - your web library, contained in one or more separate modules (that is,
    do not implement your web library in the same module as `Replay`);
  - an example executable using your web library.

* **For both:** `report.txt` or `report.pdf`, a file containing documentation of
  what you have done. Also give the motivations you were asked to give in the
  assignments, answers to questions, and instructions on how to use your
  language.

Use `cabal sdist` to generate the source tarball.
Make sure the tarball is working by extracting it in another directory and
running `cabal configure --enable-tests`, `cabal build`, `cabal test` and
`cabal haddock` and checking that everything looks right.

Where to submit
---------------

Go to [the Fire system](https://afp-lp3-18.frs.cse.chalmers.se/)!
