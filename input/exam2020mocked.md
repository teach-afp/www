Advanced Functional Programming TDA342/DIT260 (**Mocked** Online Exam)
=========


<div class="alert alert-danger">
Dear students, here you can see a **mocked (fake) exam** so that you get familiar
with the way to submit, how to write your answers, and the submission system of
Canvas. Please, raise your questions and comments in the "Discussions" section
of the course.
</div>

<table class="table table-bordered">
<tr>
    <td class="success">Date</td>
    <td class="alert-info">Sunday, 15 March 2020</td>
</tr>

<tr>
    <td class="success">Time</td>
    <td class="alert-info">Sunday, 15 March 2020, 15:00 - Wednesday, March 18 2020, 18:00 </td>
</tr>

<tr>
    <td class="success">Submission window</td>
    <td class="alert-info">Wednesday 18 2020, 18:00 - 18:10</td>
</tr>
</table>

Preliminaries
======

<div class="alert alert-info">
  Below you find the information that it is often on the **first page of a paper exam**
</div>


* Remember to take ten minutes to submit your online exam. There is a submission
  window (12:30 - 12:40), but if you finish the exam before, you can submit it
  as you did when submitting regular assignments ahead of time.

* The maximum amount of points you can score on the exam: 60 points. The grade for the
  exam is as follows:
  - Chalmers students:
    * 3: 24 - 35 points
    * 4: 36 - 47 points
    * 5: 48 - 60 points
  - GU students:
    * Godkänd: 24-47 points
    * Väl godkänd: 48-60 points
  - PhD student: 36 points to pass.

* Results will be available within 21 days from the exam date.

* Permitted materials

* Notes:

  - Read through the exam first and plan your time.
  - Answers preferably in English, some assistants might not read Swedish.
  - If a question does not give you all the details you need, you may make reasonable
    assumptions. Your assumptions must be clearly stated. If your solution only works
    under certain conditions, state them.
  - *As a recommendation*, consider spending around 1h 20 minutes per
    exercise. However, this is only a recommendation.
  - To see your exam: by appointment (*send email to Alejandro Russo*)

Preliminaries about this online exam
======

<div class="alert alert-info">
  Below you find the information that is related to **the online exam**
</div>

* Please note that this is an exam to be carried out individually, and since
  this is an exam from home, we will be very strict with plagiarism.

* This exam considers that you will have open books as well as Internet access,
  i.e., access to the course's content and code,
  [Hoogle](https://hoogle.haskell.org/), etc. -- and you can use all of such
  resources!

* The exam consists on *programming exercises* and *multiple questions*.
  - You will get a source code skeleton for each coding exercise that you need
  to complete.
  - Each question is to be answered as a comment in your source code. There are
  specific places in the source code indicating where to write your answer.

* The exam is designed to **not need any special Haskell package**. It is
  enough to use the ones imported by each source file.

What and how to submit
======

* You should submit the code skeleton **completed with your solution and
  answers**. The code skeleton can be found below.

  <table class="table table-bordered">
  <tr>
    <td class="success">Code skeleton</td>
    <td class="alert-info">Download here</td>
  </tr>
  </table>

* Before you submit your code, please clean it up! We will use the same
  requirements for clean code as in the course's assignments, that is, clean code
  - does not have long (> 80 characters) lines
  - has a consistent layout
  - has type signatures for all top-level functions
  - has good comments for all modules, functions, data types and instances.
  - has no junk (junk is unused code, code which is commented out, unnecessary comments)
  - has no overly complicated function definitions
  - does not contain any repetitive code (copy-and-paste programming)

* **Use `cabal sdist` to generate the source tarball that you will submit**. Make
  sure the tarball is working by extracting it in another directory and running
  `cabal configure` and `cabal build` and checking that everything looks right.

* **In addition, submit the files `Ex1.hs`, `Ex2.hs`, and `Ex3.hs` with your
  solutions and answers in Canvas and make sure that they are not part of any
  directory/folder**, i.e., we want just plain `.hs` files. This helps us to grade
  your submission fast since Canvas does not understand `.tar.gz` files.


Exercise 1 (20 points)
===========

File `src/Exam/Ex1.hs`

You have seen before the Haskell function

```haskell
reverse :: [a] -> [a]
```

In this exercise, we will explore a bit more.

Task 1 (10 pts)
------

1. Implement the function

   ```haskell
   freverse :: [a] -> [a]
   ```

   which simply implements `reverse` using `foldr`.

   ```haskell
   foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
   ```

2. Question: Is `freverse` faster than Haskell `reverse`? Justify your answer.


Task 2 (10 pts)
------

Write a Quick Check property that test that your function, `freverse` behaves
the same as Haskell's `reverse`.


Exercise 2 (20 points)
===============

It is often very useful to take the last element of a list.

Task 1 (20 pts)
------

Using function `freverse` from the previous exercise, write a function which
takes the last element of a list:

```haskell
myLastElem :: [a] -> a
```

Exercise 3 (20 points)
======

Monad are very cool abstractions.

Given the following data type definition:
```haskell
data X a = X a
```

Task 1
------

1. Give the instance `Monad` for `X` as the identify monad.

2. Can you give instances for `Applicative` and `Functor` using the methods from
`Monad`? Justify your answer.
