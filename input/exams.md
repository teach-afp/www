To pass the course, you must have passed the assignments (labs) and have passed
the exam ([see more information](./course_inf.html)). For the exam, the only
permitted material is a dictionary.

## Past exams

Some previous exams are available below for reference.

### Exams by Alejandro Russo

* [October 2015](./assets/exams/2015-08.pdf)

* [October 2014](./assets/exams/2014-08.pdf)

* [March 2014](./assets/exams/2014-03.pdf)

* [March 2013 (with
  JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2013-03.pdf)

* [March 2012 (with
  JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2012-03.pdf)

* [March 2011 (with
  JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/exam1103.pdf)

### Exams by K.V.S. Prasad

* [March 2015](./assets/exams/2015LP3examPrasad.pdf)

* [October 2014](./assets/exams/2014LP1examPrasad.pdf)

* [October 2013](./assets/exams/2013examPrasad.pdf)

* [October 2012](./assets/exams/2012examPrasad.pdf)

  Hints to solve the exercises:

  - **Q1** : (a) The scenario q1, q2, p1, q1 gives n=0 at the end. The scenario p1, q1, q2, p2, p1, q1 gives n=1 at the end.

  - **Q2** : The abbreviated version leaves out p1, q1, p4 and q4. Each state
      now shows where the program counters of p and q are, and the values of
      wantp and wantq. Draw the state diagram and show there is no state with p5
      and q5.

  - **Q3** : (a) The state diagram is on p108 of the text-book (b) the
      definitions of wait and signal are on p109 and p110. (c) See algorithm 6.8
      on p119.

  - **Q4** : (a) See p 161. (b). eating(i) becomes true only by executing
    takeForks(i) completely, or by by being unblocked in releaseForks(i+1) or
    releaseForks(i-1). In both cases, we have fork[i].

  - **Q5** & **Q6** : These are programming problems, not involving formal
      reasoning.

  - **Q7** : (a) the processes can livelock, looping p- to p3 and q- to q3. The
     invariant is that exactly one of C, Lp and Lq is true, (b) We did this in
     class, in my 3rd lecture. If p does not progress, Lp must be false. So q
     must progress, and will then set C to true. Assuming fairness, p must then
     progress.

* [October 2011 (old notation defined during the lectures in 2011)](./assets/exams/2011examPrasad.pdf)

* [Sample questions from 2003 (old notation from the previous text-book)](./assets/exams/2003examPrasad.pdf)


### Exams by others

* [August 2010 (with
  JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2010-08.pdf)

* [October 2010 (with
  JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2010-10.pdf)

* [October 2009 (with
  JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2009-10-sol.pdf)

* [October 2007 (with
  JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2007-10-sol.pdf)

<!-- * [March 2013 (with -->
<!--   JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2013-03.pdf) -->

<!-- * [October 2012, by -->
<!--   K.V.S. Prasad](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2012-10.pdf) -->

<!--   Hints to solve the exercises:  -->

<!--   - **Q1** : (a) The scenario q1, q2, p1, q1 gives n=0 at the end. The scenario p1, q1, q2, p2, p1, q1 gives n=1 at the end. -->

<!--   - **Q2** : The abbreviated version leaves out p1, q1, p4 and q4. Each state -->
<!--       now shows where the program counters of p and q are, and the values of -->
<!--       wantp and wantq. Draw the state diagram and show there is no state with p5 -->
<!--       and q5. -->

<!--   - **Q3** : (a) The state diagram is on p108 of the text-book (b) the -->
<!--       definitions of wait and signal are on p109 and p110. (c) See algorithm 6.8 -->
<!--       on p119. -->

<!--   - **Q4** : (a) See p 161. (b). eating(i) becomes true only by executing -->
<!--     takeForks(i) completely, or by by being unblocked in releaseForks(i+1) or -->
<!--     releaseForks(i-1). In both cases, we have fork[i].  -->

<!--   - **Q5** & **Q6** : These are programming problems, not involving formal -->
<!--       reasoning. -->

<!--   - **Q7** : (a) the processes can livelock, looping p- to p3 and q- to q3. The -->
<!--      invariant is that exactly one of C, Lp and Lq is true, (b) We did this in -->
<!--      class, in my 3rd lecture. If p does not progress, Lp must be false. So q -->
<!--      must progress, and will then set C to true. Assuming fairness, p must then -->
<!--      progress. -->

<!-- * [March 2012 (with -->
<!--   JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2012-03.pdf) -->

<!-- * [March 2011 (with -->
<!--   JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/exam1103.pdf) -->

<!-- * [August 2010 (with -->
<!--   JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2010-08.pdf) -->

<!-- * [October 2010 (with -->
<!--   JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2010-10.pdf) -->

<!-- * [October 2009 (with -->
<!--   JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2009-10-sol.pdf) -->

<!-- * [October 2007 (with -->
<!--   JR)](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/exam/2007-10-sol.pdf) -->


## More exams?

The list above will give you a pretty good idea what to expect from an exam. Of
course, the course has been run by different teachers and each of them has its
own style. If you still want more exams, you can get a copy of them from the
*Studieexpedition*.  The structure of the exam and the type of questions is not
drastically changing throughout the years. It is true that the used programming
languages can be different but the problems the past exams ask to solve are
relevant.
