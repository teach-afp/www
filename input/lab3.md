Deadlines
=========

<table class="table table-bordered">
<thead>
<tr>
    <th>Assignment 3</th>
    <th>Date</th>
</tr>
</thead>

<tr>
    <td class="success">[Part I](#part-i)</td>
    <td class="alert-info">Sunday, March 4th (course week 7)</td>
</tr>

<tr>
    <td class="success">[Part II](#part-ii)</td>
    <td class="alert-info">Sunday, March 11th (course week 8)</td>
</tr>
</table>


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

Then your task is to write a short report (1-2 pages) explaining why you chose
this package, which learning outcomes you think it relates to, and how it
relates to them. This report will be extended in Part II - Task 2. Also, write a
preliminary description of what you intend to do for Part II - Task 1.  Note
that for larger libraries, it may be intractable to consider the whole
library. If so, you may select some well defined part of the library.  Troubles
deciding on a package? Visit the [package suggestion
page](http://www.cse.chalmers.se/edu/year/2015/course/TDA342/packages.html).


Part II
=======

You will now work with the library you have chosen.

Task 1 - Writing code
---------------------

In this part you will write Haskell code that uses or extends your chosen
library. The code could be in any one of these categories (or possibly a mix of
them):

* **Specification / QuickCheck testing**: Write a test suite of QuickCheck
  properties and generators for the library (or a part of it). Your test suite
  must demonstrate that you have in-depth understanding of both QuickCheck and
  (part of) the library under test. Can you find any bugs? If so, report them to
  the maintainer of the package - or even better: fix them! If you would like to
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
AFP course.  If you have questions, talk to the teaching assistants (Dan and
Anton).

Task 2 - Documentation
----------------------

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

Clean code
----------

Before you submit your code, Clean It Up! Submitting clean code is Really
Important, and simply the polite thing to do. After you feel you are done, spend
some time on cleaning your code; make it simpler, remove unnecessary things,
etc. We will reject your solution if it is not clean. Clean code:

- Does not have long lines (< 80 characters)
- Has a consistent layout
- Has type signatures for all top-level functions
- Has good comments
- Has no junk (junk is unused code, commented code, unnecessary comments)
- Has no overly complicated function definitions
- Does not contain any repetitive code (copy-and-paste programming)

Submission
----------

Your submission needs to include the following information:

* Your Haskell files, containing your code. Typically this should be a cabal
  package.
* **report.txt** or **report.pdf**, a file containing your report.

Go to [the Fire system](https://afp-lp3-17.frs.cse.chalmers.se/)!
