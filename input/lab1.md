## Deadlines

<table class="table table-bordered">
<thead>
<tr>
    <th>Assignment 1</th>
    <th>Date</th>
</tr>
</thead>

<tr>
    <td class="success" > Part I </td>
    <td class="alert-info" >  January 28th (Thursday, course week 2) </td>
</tr>

<tr>
    <td class="success" > Part II </td>
    <td class="alert-info" >  February 4th (Thursday, course week 3) </td>
</tr>
</table>


## Description

This lab assignment asks you to implement a library for so-called turtle
graphics. The library will be interfaced to as an embedded language. Several
extensions are then made to the original turtle language.

We recommend that you implement the graphics part of the lab using
[HGL](http://hackage.haskell.org/package/HGL), a simple graphics library with
just enough features for this assignment. If you prefer (or if you plan on using
3D) you may use one of the [OpenGL](https://wiki.haskell.org/Opengl) libraries
or you may use [gtk2hs](https://wiki.haskell.org/Gtk2Hs). If you would like to
use a different graphics library please ask before starting the assignment.

## Turtle graphics

The idea of turtle graphics was originally part of the [Logo programming
language](https://en.wikipedia.org/wiki/Logo_(programming_language). It
originated from an environment where a real robot (the "turtle") could move
around and act on simple commands. It was successfully used at MIT to teach
children to learn programming ([check this blog post to see
why](https://originzx.wordpress.com/2015/09/23/logo-a-programming-language-for-children-with-visionary-ideas/)). The
two basic commands it understood were:

```haskell
forward n
right d
```

Here, `n` is the number of steps the robot should move forward, and `d` the
number of degrees the robot should turn. More information can be found on the
[Turtle graphics wikipedia page](http://en.wikipedia.org/wiki/Turtle_graphics)
(or in a local copy of the [Logo
primer](http://www.cse.chalmers.se/edu/year/2015/course/afp/logo_primer_2007.pdf)). The
idea is that you will implement a part of this turtle graphics language. Your
program will be able to produce something like the following output:

<img class="img-responsive"
     src="./assets/img/turtle-tree.gif"
>

Below are a number of tasks. Needless to say, please *read these carefully* and
make sure you do not miss parts of an assignment!

Most assignments require coding and descriptions with motivations of what you
have done. Most of the descriptions should be in the form of [Haddock
comments](https://www.haskell.org/haddock/) for modules and fuctions. There are
also several questions in this description, make sure you answer all of them in
your report.

## Part I

The first part of this assignment is just to get you started. You should get
started on Part II before the deadline for Part I.

### Task 1 - Free code!

Download and unpack this [stub cabal
package](https://bitbucket.org/russo/afp-code/src/9999649d46bc2fdafc3009a9e8e1b14b7a82b5be/assignment1/turtle-graphics-0.1.0.0/?at=master)
or the [OpenGL/GLUT
version](https://bitbucket.org/russo/afp-code/src/9999649d46bc2fdafc3009a9e8e1b14b7a82b5be/assignment1/turtle-graphics-glut-0.1.0.0/?at=master). The
archive contains a file structure and some useful code snippets for you to build
your implementation on. You are free to modify the package however you wish or
build a new package from scratch, but if you deviate significantly from the
structure in the stub file you may want to explain why your version is an
improvement.

Make sure "cabal configure", "cabal build" and "cabal haddock" works (switch the
contents of the files to match the graphics framework you are using if not
HGL). Look through the contents of the package and the generated documentation,
run the generated executable and make sure it works. Fill in or replace the
fields in the .cabal file with the appropriate information.

Familiarise yourself with the graphics library you are using by writing a simple
program which opens a window and draws something in it (the provided code
already does this for HGL, but make sure you understand how it is used).

### Task 2 - Library interface

The turtle graphics language should be implemented as an embedded language
library in Haskell. The turtle language should be provided to the user as a
single module, exporting abstract datatypes and operations.

An important part of creating an embedded language is to think carefully about
what interface you want to offer to the user. Think about compositionality (how
easy is it to combine simpler programs to build more complex ones?), and
abstraction (hiding irrelevant implementation details from the users).

For instance, your library might define and export the following things
(different types are conceivable, as long as they implement the same basic
functionality):

```haskell
type Program  -- the abstract type of a turtle program
forward :: Double -> Program
right   :: Double -> Program
```

Other turtle commands you should provide are:
- `penup` and `pendown`: stop drawing and start drawing respectively,
- `color`: changes the color of the turtle's pen,
- `die`: "kills" the turtle (rendering it unable to perform any more actions),
- `idle`: a program that does nothing,
- `limited`: makes the turtle stop what it is doing after a specified period of
   time (by some definition of time not directly related to minutes and
   seconds),
- `lifespan`: kills the turtle after a specified period of time,
- `backward` and `left`: self explanatory,
- `times`: repeats a turtle program a certain number of times, and
- `forever`: that repeats a program forever.

You will also need a sequencing operator `(>*>)` to perform commands one after
another.


For running programs, in addition to the graphical interface your program should
provide a simple textual interface that prints what happens in sequential order,
i.e., prints a description of the actions that the turtles perform at each
"step" in time (which lines are drawn, what action turtles perform or any other
representation of what is going on will suffice). The textual interface needs to
be productive for infinite turtle programs like those created with forever (it
should go on printing the actions indefinitely). The graphical interface only
needs to handle infinite programs for grades 4 or 5.

Write down the interface of your library, like in the example above, listing the
types you plan to export (you may sketch definitions for them, but it is not
needed for this part) and type signatures for the operations. When appropriate
add explanations of what your operations are intended to do. Make sure you do
not forget to add at least one run function for your programs (the types of the
run functions may be a bit sketchy at this stage, but explain what each of them
do in their comments).

### Task 3 - Example

Write down the **spiral** example from the Logo programming language using your
interface. In their syntax it looks like:

<div class="row">
        <div class="col-md-6">
          <img class="img-responsive"
               src="./assets/img/turtle-spiral.gif"
          >
        </div>
        <div class="col-md-6">
         ```bash
         to spiral :size :angle
                 if :size > 100 [stop]
                 forward :size
                 right :angle
                 spiral :size + 2 :angle
               end
         ```
        </div>
</div>

You will not be able to run your spiral example yet, but it should type check.
Also, make one version of the spiral example that goes on forever.

<div class="alert alert-info">
**Question**: Can you define the limited version in terms of the unlimited one?
</div>

Finally make a
program that would draw a finite spiral and when done starts drawing an infinite
spiral where the finite spiral ended.

---

## Part II

In this part of the assignment you will implement your turtle language and write
a report.

### Task 1 - Implementation

Implement the library you designed in Part I. Clearly separate primitive and
derived operations, and try to keep the set of primitive operations as small as
possible.

At this point you might realise that the interface you have designed is missing
some operations, or that parts of it are difficult to implement. Do not hesitate
to change the interface in these cases--just be clear about what you changed and
motivate why the change was needed (in your report or in the documentation for
the relevant functions).

Make sure that you carefully define the borders of your library by exporting
only the things you want a user to see (the interface you have designed).

<div class="alert alert-info">
**Question**: What definition of time do you use (what can a turtle achieve in a
single time unit)?
</div>

### Task 2 - Parallel composition

Add a parallel composition combinator to your turtle language. One possible interface parallel composition could have is:

```haskell
(<|>) :: Program -> Program -> Program
```

When you run a turtle program `p <|> q`, there will be two turtles, one running
`p` and the other running `q`, in parallel. In your textual interface you should
show which actions occur in parallel. Importantly, it must be the case that
operator `(<|>)` could potentially be applied in any part of your program.

<div class="alert alert-info">
**Questions**: What happens after a parallel composition finishes? Is your
parallel composition commutative, is it associative? (To answer this question
you must first define what it means for programs to be equal.) What happens if a
turtle runs forever only turning left in parallel with another turtle running
the spiral example? Does your textual interface handle this situation correctly,
if not - how would you fix it?
</div>

<div class="alert alert-info">
**Question**: How does parallel composition interact with lifespan and limited?
(lifespan does not need to correspond realistically to actual life spans, just
specify how it works.)
</div>

###  Task 3 - Additional operators (only required for grade 4 or 5)

Add a new module TurtleExtras to your cabal package. This module should contain
some derived operators that you think may be a useful addition to the
language. Try to add higher level components like squares and other geometrical
shapes as well as operators that capture common patterns.

Operators that demonstrate the flexibility of your turtle language are
encouraged. See the [grading section]() for more hints.

### Task 4 - Examples

Implement a few examples that together use all of the constructs you have
implemented. (Do this in a different module that imports the module defining the
embedded language.) Make sure that you have at least one program that does not
terminate, and show that the textual interface can handle this.

Please, choose your "favorite" turtle program, resulting in a cool picture or
animation. This program should be the main function of your executable module,
so building your cabal package yields an executable that runs the program in
graphical mode.

### Task 5 - Thoughts and reflections

Please, address all the following points:

1. Start by answering all the questions in the assignment description above.

2. Did you use a shallow or a deep embedding, or a combination? Why? Discuss in
a detailed manner how you would have implemented the Program type if you had
chosen the other approach. What would have been easier/more difficult?

3. Compare the usability of your embedding against a custom-made implementation
of a turtle language with dedicated syntax and interpreters. How easy is it to
write programs in your embedded language compared to a dedicated language? What
are the advantages and disadvantages of your embedding?

4. Compare the ease of
implementation of your embedding against a custom-made implementation. How easy
was it to implement the language and extensions in your embedded language
compared to a dedicated language? What are the advantages/disadvantages of your
embedding?

5. In what way have you used the following programming language features:
higher-order functions, laziness, polymorphism?

**Required only for grade 5** (but helpful for grade 4): Characterize the
  relationships between your operators as a set of algebraic laws. For
  inspiration look at the laws of [algebraic
  semirings](http://en.wikipedia.org/wiki/Semiring), also look at the laws for
  the [monoid
  typeclass](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html)and
  possibly other type classes like
  [Applicative/Alternative](http://hackage.haskell.org/package/base-4.3.1.0/docs/Control-Applicative.html). Also,
  consider the following:

* How do `<|>` and `>*>` interact?
* How do idle and die interact with your combinators?
* How do forever and times interact with your combinators?
* Can you find any law that is unexpected, unintuitive or undesired, but follows from your operators?

<div class="alert alert-info">
**Question**:Is your program data type a Monoid? Under which operations? There may
be several possible Monoid instances. Would it be a Monoid if some small change
was made to your operators?
</div>

## Hints about coding and writing

* The report should be well structured. Avoid adding trivial or
  misleading/incorrect information to your report.
* The report should be self-contained. When answering a question, include it in
  the text. When referring to a piece of code, include it. If it is too lengthy,
  refer to exactly where it is.
* Using a consistent style for code is nice. Easily readable code is not the
  most important aspect of this lab but it does make a difference. Consider using
  [HLint](http://hackage.haskell.org/package/hlint) for suggestions on better
  readability of your code.
* Use [Hoogle](http://hackage.haskell.org/package/hlint) or similar to search
  for the type signature of a function you need before implementing it, chances
  are you just need to import another module.
* Short and understandable code is best. Long and understandable code is better
  than short and obfuscated.


## Submission

### Clean code

Before you submit your code, *clean it up!* Submitting clean code is really
important, and simply the polite thing to do. After you feel you are done, spend
some time on cleaning your code; make it simpler, remove unnecessary things,
etc. We will reject your solution if it is not clean. Clean code:

* Does not have long lines (< 80 characters)
* Has a consistent layout
* Has type signatures for all top-level functions
* Has good comments for all modules, functions, data types and instances. The comments should look good when compiled to HTML with Haddock.
* Has no junk (junk is unused code, commented code, unnecessary comments)
* Has no overly complicated function definitions
* Does not contain any repetitive code (copy-and-paste programming)

### Files to submit

* For **Part I** the submission format is not so important, use the stub cabal
  package or just submit one or two .hs files if you prefer -- as long as they
  contain appropriate commenting (you do not need to write any functions except
  the example for the first part, just type signatures). Make sure to answer the
  question asked in part one about defining the finite spiral using the infinite
  one! Omitting this is one of the few ways to fail on the first part.
* For **Part II**, your submission needs to include the following:
  - Your cabal package, containing your solution. Use `cabal sdist` to generate
    the source tarball. Make sure the tarball is working by extracting it in
    another directory and running `cabal configure`, `cabal build` and
    `cabal haddock` and checking that everything looks right.
  - Files `report.txt` or `report.pdf`, which contain documentation about what
    you have done. Please, give the motivations you were asked to give in the
    assignments, answers to questions, and how to use your language.

### Where?

[Go to the Fire System!](https://xdat09.ce.chalmers.se/2016/lp3/afp/)

## Grading

One of the focuses for this lab is DSL design. This means that it is difficult
to formulate precise requirements for each grade without solving the problem for
you (by telling you which design to use).

This does not mean that we judge your submission arbitrarily. If you come and
talk to us during office hours **we can give you more precise descriptions of how
you need to improve your submission for a higher grade**.

Here are some qualities that count towards higher grades:

* The data types you use should not be needlessly complicated. The intended use
  of the data type should preferably be self-explanatory to someone who is
  familiar with the problem you are solving.
* Few primitive operations is usually good (although this can be taken to an
  extreme). If an operator can easily be defined using other primitive
  operators, it should be.
* Separation of concerns is good. For instance if you need to consider the
  existence of the lifespan operation when you code some other feature you may be
  doing something wrong.
* The examples should be interesting. Programs that use the advantages of EDSLs
  are encouraged, i.e. you should use your knowledge of Haskell to make more
  interesting programs using less code.
* The (optional) "additional operators" you define should demonstrate the
  flexibility of your language. Show for instance how a general operator can be
  defined and used to define different useful programs. Try to think of patterns
  that may recur in turtle programs and define operators to make them more
  concise. Think of useful higher order programs (programs that take programs as
  parameters).
* The parallel operator should fit well into your language. Its behaviour should
  be easy to describe without lots of special cases and arbitrary choices.

If you do not undestand what is meant by any of these we encourage you to ask us
during office hours.

**Requirement for grade 4**
* You need to implement the TurtleExtras module.
* The graphical interface must handle infinite programs.

**Requirement for grade 5**
* The relationships between your operators and some relevant algebraic laws must
  be defined as a list of equations. You do not need to prove or test the
  equalities, but the more obvious they are from your code the better.
* The behavior of the parallel operator is clearly defined and contrasted with
  other alternatives.
