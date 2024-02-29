Assignment 3 - Variant 1: public Haskell library

See also [Variant 2](lab3-agda.html) (Agda).

Description
===========

This lab assignment will require you to read, understand, evaluate and extend an
advanced Haskell code base.  You will choose an existing Open Source library
package (preferably from
[Hackage](http://hackage.haskell.org/packages/hackage.html)) and use it for a
small project.


Part I
======

This part of the lab is just for you to choose a Haskell library and motivate
your choice. You should start the next part before the deadline of this one.

First choose a package on
[Hackage](http://hackage.haskell.org/packages/hackage.html) which:

* You think is interesting.
* Covers some of the categories DSL, Types, and Spec from the [learning outcomes](./inf.html#aim-and-context) of the course.
* Is still actively maintained (check for public source repo, issue tracker, open pull requests, last commits...).

Then your task is to write a short report (1&ndash;2 pages) explaining why you chose
this package, which learning outcomes you think it relates to, and how it
relates to them. This report will be extended in Part II &mdash; Task 2. Also, write a
preliminary description of what you intend to do for Part II &mdash; Task 1.  Note
that for larger libraries, it may be intractable to consider the whole
library. If so, you may select some well defined part of the library.


Part II
=======

You will now work with the library you have chosen.

Task 1: Writing code
--------------------

In this part you will write Haskell code that uses or extends your chosen
library. The code could be in any one of these categories (or possibly a mix of
them):

* **Specification / QuickCheck testing**: Write a test suite of QuickCheck
  properties and generators for the library (or a part of it). Your test suite
  must demonstrate that you have in-depth understanding of both QuickCheck and
  (part of) the library under test. Can you find any bugs? If so, report them to
  the maintainer of the package &mdash; or even better: fix them! If you would like to
  use another testing library than QuickCheck for property based testing or
  specification, please contact us.

* **Modification / extension**: Show how the library could be changed to improve
it in some aspect, or extend the library with some useful feature. Implementing
the suggested feature is of course the best thing, but since this is a short
project we understand if the result is limited to a partial solution or even a
sketch of the implementation along with a description (in your report) of what
difficulties you faced when trying to implement it.

* **Advanced usage / tutorial**: Show some examples of what the library can be
  used for. The examples must demonstrate a deep understanding of the library
  (or part of it). Note: This task is potentially much simpler than the other
  two. Making a good tutorial is enough for a passing grade on this assignment,
  but is unlikely to yield a higher grade.

The expected work load for assignment 3 (all parts together) is around 40
hours. Try to demonstrate that you master (some of) the learning outcomes of the
AFP course.

Task 2: Documentation
---------------------

Write a report about you have been doing in the lab. The report should include:

* **Description of the library interface**: If the library has poor
documentation, this is typically done by updating it with proper
([Haddock](http://www.haskell.org/haddock/)) comments for the functions and
modules it exports. You can then send this to the package's maintainer. If the
documentation is already in good shape, write a summary in your report.

* **Description of the library implementation**: An explanation of some
  interesting techniques used in the implementation and perhaps other
  interesting things you have discovered about the library!

* **Analysis of your code**: An overview of the code you have been writing and a
few highlights, for example particular problems that you faced and how you
solved them.

Use concepts you have learned in the course to express yourselves.


Submission
==========

Clean code
----------

Before you submit your code, *clean it up!* Submitting clean code is *really
important*, and simply the polite thing to do. After you feel you are done, spend
some time on cleaning your code; make it simpler, remove unnecessary things,
etc. We will reject your solution if it is not clean. Clean code:

- Does not have long (> 80 characters) lines
- Has a consistent layout
- Has type signatures for all top-level functions
- Has good comments for all modules, functions, data types and instances. The comments should look good when compiled to HTML with Haddock.
- Has no junk (junk is unused code, code which is commented out, unnecessary comments)
- Has no overly complicated function definitions
- Does not contain any repetitive code (copy-and-paste programming)

What to submit
--------------

Your submission needs to include the following information:

* Your Haskell files, containing your code. Typically this should be a cabal
  package.
* **report.txt** or **report.pdf**, a file containing your report.

Where to submit
---------------

On the Canvas course page.
