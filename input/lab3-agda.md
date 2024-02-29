Assignment 3 - Variant 2: Agda

See also [Variant 1](lab3.html) (Haskell).


Description
===========

This lab assignment will require you to implement a library in Agda
using dependent types to express properties of its data structures
and operations.

Part I
======

Choose a domain you want to implement in Agda.
This could be a data structure, a small programming language etc.

Or you find a popular Agda library and a part of it that you want to extend.
(Make sure the library is actively maintained and accepts contributions.)

Write a short report (1&ndash;2 pages) explaining how you intend to represent
the data types and operations in Agda, which properties they have,
and how you intend to use dependent types to represent invariants
of your data structures and operations.
Also spell out some properties that you intend to prove that are not captured by the invariants (if any).

This report will be extended in Part II &mdash; Task 2.

Links to libraries (slightly outdated):

- https://wiki.portal.chalmers.se/agda/Main/Libraries
- https://github.com/agda/agda-pkg


Part II
=======

Task 1: Implementation
----------------------

Implement the types, operations, and properties described in Part I.

During the course of implementation you might discover
that you have to revise the specification you outlined in Part I,
this is perfectly fine.

The expected work load for assignment 3 (all parts together) is around 40
hours. Try to demonstrate that you master (some of) the learning outcomes of the
AFP course.

Task 2: Documentation
---------------------

Write a report about you have been doing in the lab. The report should include:

* **Description of the library interface**

* **Description of the library implementation**: An explanation of some
  interesting techniques used in the implementation (and perhaps other
  interesting things you have discovered about the library)!

* **Analysis of your code**: An overview of the code you have been writing and a
few highlights, for example particular problems that you faced and how you
solved them.

Use concepts you have learned in the course to express yourselves.

If you chose to extend a public library and you managed to produce a mature result
that fits well into the library, consider making a merge request.


Submission
==========

Clean code
----------

Before you submit your code, *clean it up!*
Bring the code into a publishable form, with good comments.

If you aim at contributing to a public library,
make sure you follow the style and contribution guide of that library.


What to submit
--------------

Your submission needs to include the following information:

* Your Agda file, containing your code.
  The code should build with a recent release of Agda (e.g. 2.6.4.1).

  If you contribute to a public library, and the code is not self-contained,
  submit a link to your merge request.
  This can also be a merge request into your own fork.
  The merge base should be a commit from the public library
  (so that the merge request contains the whole of your contribution).

* **report.txt** or **report.pdf**, a file containing your report.

Where to submit
---------------

On the Canvas course page.
