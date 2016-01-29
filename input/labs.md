
## Assignments

There are three programming assignments, which you should do in pairs. If you
have a good reason for doing the assignments by yourself, please contact the
lecturer.

* You need to pass all three assignments in order to pass the course.
* Each of the assignments is divided into two parts with separate deadlines.
* The assignments have to be handed in using [the Fire system](https://xdat09.ce.chalmers.se/2016/lp3/afp/).

## Deadlines

Please read these early and carefully!

<div class="alert alert-danger">
Deadlines are hard!
</div>

If you for some reason cannot make the deadline, contact us before the deadline,
and tell us what your reason is, together with a realistic proposal of a new
personal deadline for you. You may then get an extension of the deadline.


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

<table class="table table-bordered">
<thead>
<tr>
    <th>Assignment 2</th>
    <th>Date</th>
</tr>
</thead>

<tr>
    <td class="success" > Part I </td>
    <td class="alert-info" >  February 11st (Thursday, course week 4) </td>
</tr>

<tr>
    <td class="success" > Part II </td>
    <td class="alert-info" >  February 25th (Thursday, course week 6) </td>
</tr>
</table>


<table class="table table-bordered">
<thead>
<tr>
    <th>Assignment 3</th>
    <th>Date</th>
</tr>
</thead>

<tr>
    <td class="success" > Part I </td>
    <td class="alert-info" >  February 29th (Monday, course week 7) </td>
</tr>

<tr>
    <td class="success" > Part II </td>
    <td class="alert-info" >  March 13rd (Sunday, course week 8) </td>
</tr>
</table>

Your last attempt has to be submitted before the final deadline. If you fail to
do this, your submission will be rejected.

<div class="alert alert-info"> **Final Deadline**: March 20th (Sunday, exam
week) </div>


## Grading

Assignments will be graded on a scale of 3 to 5 and will count towards your
final grade on the course.  Your last attempt has to be submitted before the
final deadline. If you fail to do this, your submission will be rejected.

<div class="alert alert-info">
Assistants will send you feedback between
three to four *working days* after submission.
</div>


## On cheating

<div class="alert alert-danger">
Cheating is taken very seriously!

Cases being suspected of cheating will be reported to the [Disciplinary
Commitee](https://student.portal.chalmers.se/en/chalmersstudies/joint-rules-and-directives/RulesofDiscipline/Pages/TheDisciplinarycommiteeanditswork.aspx)
for further investigation. In the worst case, this can lead to the student's
exclusion from services such as lectures, computer rooms and exams.
</div>

## Legit cooperation

Not all the form of cooperation among students are considered cheating.  Here
follow the rules of cooperation between students in this course.

* One is allowed to **orally** discuss exercises and programming assignments
  with one another.

* For the **programming assignments**, one is allowed to work in **groups of
size two**.  Once you have cooperated on an assignment with a particular person,
you must submit your answer to that assignment together with that person, and
can not cooperate with anyone else.

* Apart from with your own lab partner, you are *not allowed to share any piece
of code with another student*, by any means. Examples of ways which you cannot
use to share code are:
  - e-mailing code,
  - printing out your code and giving it on paper,
  - stealing other people's print-outs,
  - faxing,
  - dictating code over the phone,
  - copying files with or without permission,
  - reading someone else's email,
  - reusing code from the web, etc.

Violating any of these rules might not be considered cheating by itself, but
**violating any of these rules without informing the lecturer is definitely considered
cheating**.

<div class="alert alert-danger">
Remember: it is equally wrong to give your code to another student as it is to
use another student's code.
</div>

It is possible that exceptions to these rules are granted, but only you should
talk to the lecturer as soon as the problem arises.

## GHC setup for Chalmers machines

The labs in this course require you to use a fairly recent version of GHC.
Unfortunately, the version provided by default on the Chalmers machines is
quite ancient. If you want to use the Chalmers machines for your lab work,
you will need to add the directory `/chalmers/sw/unsup64/phc/b/binh` to your
`$PATH`.

One way to achieve this is to run the following in a terminal:

```
echo 'export PATH=/chalmers/sw/unsup64/phc/b/binh:$PATH' >> $HOME/.bashrc
```

After executing the above command, close your terminal window and open a new
one. Then check that everything works as expected:

```
ghc --version
```

GHC should then print:

```
The Glorious Glasgow Haskell Compilation System, version 7.10.2
```

If the version printed is 7.10.2, then you're all set up and good to go. Note
that you only need to perform this setup *once* on your Chalmers account,
as these changes persist across reboots.
