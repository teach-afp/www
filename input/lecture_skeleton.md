
## Introduction 

* Teaching team 

* Why concurrent programming?

  - In general 
  
  - In this course



* Gentle start 

  - Java 

## Teaching team 

* Check the [information tab](./course_inf.html)

<div class="container">
<iframe src="./course_inf.html" width=100% height=800>
</iframe>
</div>

## Why concurrency? 

<img class="img-thumbnail"  
     src="./assets/img/anykey.png" 
     height="30%"
     width="30%"  
     style="float:right" > 

* Where is John von Neumann?

* Using the processor efficiently in the presence of I/O

  - Operating systems 

  - Distributed systems 

  - Real-time systems

<img class="img-thumbnail"  
     src="./assets/img/cockpit.png" 
     height="30%"
     width="30%"  
     style="float:right" > 

* Modelling inherently concurrent systems 

  - Software controllers which handle responses from 
    several physical sources 
 
* The world is concurrent! 

* Multi-core machines

  - Cell phones
 
  - Laptops 

* Performing computationally expensive tasks using several 
  processors 

<img class="img-thumbnail"  
     src="./assets/img/multicore.png" 
     height="30%"
     width="30%" >
<img class="img-thumbnail"  
     src="./assets/img/weather.png" 
     height="30%"
     width="30%" >


## Concurrency vs. Parallelism 

### <span class="label label-default">Parallell programmering ≠ parallel programming</span>

* Parallel 
 
  - Physically at the same time 

* Concurrent 

  - Logically at the same time, but might be implemented without any real parallelism

* The book covers parallel programming too – but we will briefly focus on it in the course


## Course (general) goals 

* Introduction to the problems common to many computing disciplines:

  - Operating systems
  - Distributed systems
  - Real-time systems

* Appreciation of the problems of concurrent programming

  - Classic synchronisation problems

* Understanding of a range of programming language constructs for concurrent
  programming

* Ability to apply these in practice to synchronisation problems in concurrent
  programming

* Practical knowledge of the programming techniques of modern concurrent
  programming languages


## Practical course information 

* About the [course activities](./time_inf.html) 

  - Two lectures per week 

  - Six to eight supervision/exercise hours

* Assignments 

  - Four programming assignments – “labs” (two in Java and two in Erlang)

* [Written Exam](./exams.html)

  - 4 hours
  - Closed book 

* [Literature](./lit_inf.html) 

 
* [Web page of the course](http://www.cse.chalmers.se/edu/course/TDA383/)

  - Intended to answer most basic questions

  - Email <a href="mailto:tda383-dit390-concurrent-programming-period-1-2015-2016@googlegroups.com">tda383-dit390-concurrent-programming-period-1-2015-2016@googlegroups.com </a>

## Gentle start 

* Introduction to concurrent programming

* Basic understanding

  - Concurrent programming concepts
  - Threads
  - State, Execution, Scheduling

* Synchronisation problems

* Introduction to programming languages
  - Java

## A summer job 

* Cremona decide to employ experts to increase sales. 
  Their solution:


<p class="cremona text-center"> Buy @ Cremona!</p>

* The message must be flashed every three seconds (For the example, you need the
  [JFLash class](./assets/java/JFlash.java))

  <pre class="prettyprint lang-java linenums"> 
  import javax.swing.*;

  public class Main6 {

      private JFlash window;
      private Thread buyThread;
      private final int buy_pause = 3000;

      public Main6() {
          window = new JFlash("Cremona");
          SwingUtilities.invokeLater(window);
          buyThread = new Thread() {
                  public void run() { 
                      while (true) {
                          window.flash("Buy @ Cremona!");
                          try { Thread.sleep(buy_pause); }
                          catch (InterruptedException e) {}
                      }
                  }
              };
          buyThread.start();
      }

      public static void main(String[] args) {
          new Main6();
      }
  }
  </pre>

  - [Here, the code](./assets/java/Main6.java)


* The program does not increase sales as predicted. A psychologist is called in
  to help! 

  - An additional message is needed: the sign must flash *Free beer!* every 5 seconds

<p class="cremona text-center"> Free beer! </p>

* The program is now more complex


  <img class="img-thumbnail"  
       src="./assets/img/cremonatime.png">
  

  <pre class="prettyprint lang-java">
  final int buy_pause = 3000;
  final int beer_pause = 5000;
  int next_buy = buy_pause;
  int next_beer = beer_pause;
  ...

  buyThread = 
    new Thread() {
        public void run() { 
           while (true) {
              if (next_buy < next_beer) {
                 try { Thread.sleep(next_buy); }
                 catch (InterruptedException e) {} ;
                 window.flash("Buy @ Cremona"); 
                 next_beer = next_beer - next_buy;
                 next_buy = buy_pause;
              } 
              else if (next_buy > next_beer) {
                   try { Thread.sleep(next_beer); }
                   catch (InterruptedException e) {} ;
                   window.flash("Free beer!"); 
                   next_buy  = next_buy - next_beer ; 
                   next_beer = beer_pause ; 
              }
              else {
                 try { Thread.sleep(next_buy); }
                     catch (InterruptedException e) {} ;
                     window.flash("Buy @ Cremona! - Free beer!");
                     next_buy  = buy_pause ; 
                     next_beer = beer_pause ; 
             }
           }
         } 
       }
  </pre>

* A more natural solution is to run the two simple algorithms *concurrently*:
  
  <pre class="prettyprint lang-java linenums">
  final int buy_pause = 3000;
  final int beer_pause = 5000;
  buyThread = 
    new Thread() {
        public void run() {
              while (true) {
                  window.flash("Buy @ Cremona!");
                  try { Thread.sleep(buy_pause); }
                  catch (InterruptedException e) {}
              }
            }
    };
  beerThread = 
    new Thread() {
        public void run() {
              while (true) {
                  window.flash("Free beer!");
                  try { Thread.sleep(beer_pause); }
                  catch (InterruptedException e) {}
              }
        }
    };
  </pre>

  - [Here, the code](./assets/java/Main7.java) 

## Java threads 

* Java threading framework

  - The `Thread` class provides the API and generic behaviours A concrete thread
    must provide a `run()` method which is the code that the thread will execute
    when started

* Providing thread `run()` method

  - Inheritance

  <pre class="prettyprint lang-java">
  class Buy extends Thread {
  //some init
     public void run() { 
            while (true) {
                  window.flash("Buy @ Cremona!");
                  //add napping here
            }
     }
  }
  </pre>

  - Implement interface `Runnable`

  <pre class="prettyprint lang-java">
    class Buy implements Runnable {
    //some init
     public void run() { 
            while (true) {
                  window.flash("Buy @ Cremona!");
                  //add napping here
            }
     }
    }
  </pre>

  - Using anonymous inner classes
  <pre class="prettyprint lang-java">
   buyThread = new Thread() {
      public void run() { 
	    while (true) {
		  window.flash("Buy @ Cremona!");
		  //add napping here
	    }
      }
   };
  </pre>

## Running Java threads

* Invoking the `run()` method in a new thread

  - Inheritance

    <pre class="prettyprint lang-java">
    buyThread = new Buy(…);
    buyThread.start();
    </pre>

  - Interface 

   <pre class="prettyprint lang-java">
    buyThread = new Thread(new Buy(…));
    buyThread.start();
   </pre>

  - Anonymous classes 

   <pre class="prettyprint lang-java">
    buyThread = new Thread(public void run() {...});
    buyThread.start();
   </pre>

## Napping in Java 

* A sleeping thread can be interrupted, hence the need for the catch/try clause

  <pre class="prettyprint lang-java">
  try {
     Thread.sleep(milliseconds);
  }
  catch (InterruptedException e) {
     //Panic: do something here!
  }
  </pre>

* More on this later

## Concurrent programming languages

* Using such languages we are going to 

  - Explore concurrency problems and solutions

  - Understand how modern programming languages support concurrent programming

* Main course programming languages

  - Java 

  - Erlang

## Threads scheduling 

* On a uniprocessor system threads appear to run at the same time but in fact
  their execution must be interleaved
 

  <img class="img-thumbnail"  
       src="./assets/img/interleaving.png">


* The job of switching between threads is performed by the scheduler

  - Part of the run-time system, or
 
  - Performed using the operating system’s processes and scheduler

* Many different methods of scheduling exist

* *Cooperative scheduling*

  - a thread runs until it is willing to release the processor (e.g. sleep or
    termination)

* *Preemptive scheduling* 

  - a thread is interrupted in order to let other threads continue
    (e.g. time-slicing)

  - Erlang have a preemptive scheduler

  - Most modern JVM’s are also preemptive

## Types of process behaviour 

* A thread 

<img class="img-thumbnail"  
     src="./assets/img/proclady.png" 
     height="30%"
     width="30%" > 

* Independent 

  - Relatively rare; Rather uninteresting

    <table border="0">
    <tr>
    <td>
    <img class="img-thumbnail"  
       src="./assets/img/proclady.png" > 
    </td>
    <td>
    <img class="img-thumbnail"  
       src="./assets/img/proclady.png" >
    </td>
    <td> 
    <img class="img-thumbnail"  
       src="./assets/img/proclady.png">
    </td>
    </tr>
    </table>

* Competing 

  - Typical in OS and networks, due to shared resources 

    <table border="0">
    <tr>
    <td>    
    <img class="img-thumbnail"  
       src="./assets/img/proclady.png" >
    <td>
    <span class="glyphicon glyphicon-arrow-right"></span>
    </td>
    <td>
    <img class="img-thumbnail"  
       src="./assets/img/sticks.png" >
    <td> 
    <span class="glyphicon glyphicon-arrow-left"></span>
    </td>
    <td>
    <img class="img-thumbnail"  
       src="./assets/img/proclady.png" > 
    </td>
    </tr>
    </table>

  - Deadlock 

      <table border="0">
      <tr>
      <td>    
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png" width="70%"> 
      </td>
      <td>
      <img class="img-rounded"   
         src="./assets/img/stick.gif"> 
      </td>
      <td>
      <span class="glyphicon glyphicon-arrow-right"></span>
      </td>
      <td>
      <img class="img-thumbnail"  
         src="./assets/img/sticks.png">
      </td>
      <td>
      <span class="glyphicon glyphicon-arrow-left"></span>
      </td>
      <td>
      <img class="img-rounded"   
         src="./assets/img/stick.gif"> 
      </td>
      <td>
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      </tr>
      </table>

  - Starvation 

      <table border="0">
      <tr>
      <td>    
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      <td> 
         <span class="glyphicon glyphicon-arrow-left"></span>
      </td>
      <td>
      <img class="img-rounded"   
         src="./assets/img/sticks.png"> 
      </td>
      <td>
      <span class="glyphicon glyphicon-arrow-right"></span>
      </td>
      <td>
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      </tr>
      <tr> 
      <td> 
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      </tr>
      </table>

* Cooperating 

    * Processes combine to solve a common task
  
    * Synchronisation 

      <table border="0">
      <tr>
      <td>    
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      <td> 
         <span class="glyphicon glyphicon-arrow-right"></span>
      </td>
      <td>
      <img class="img-thumbnail"   
         src="./assets/img/hatsnow.png" height="50%" width="50%"> 
      </td>
      <td>
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      <td> 
         <span class="glyphicon glyphicon-arrow-right"></span>
      </td>
      <td>
      <img class="img-thumbnail"  
         src="./assets/img/gloves.png"> 
      </td>
      </tr>
      <tr>
      <td>
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      <td> 
         <span class="glyphicon glyphicon-arrow-right"></span>
      </td>
      <td>
      <img class="img-thumbnail"  
         src="./assets/img/sweater.gif"> 
      </td>
      </tr>
      </table>
      
      <table border="0">
      <tr>
      <td width="20%">
      <img class="img-thumbnail"   
         src="./assets/img/hatsnow.png"> 
      </td>
      <td width="20%">
      <img class="img-thumbnail"  
         src="./assets/img/gloves.png"> 
      </td>
      <td width="20%">
      <img class="img-thumbnail"  
         src="./assets/img/sweater.gif" height="100%" width="100%"> 
      </td>
      <td> 
         <span class="glyphicon glyphicon-arrow-right"></span>
      </td>
      <td width="20%"> 
      <img class="img-thumbnail"  
         src="./assets/img/proclady.png"> 
      </td>
      <td> 
         <span class="glyphicon glyphicon-arrow-right"></span>
      </td>
      <td  width="20%"> 
      <img class="img-thumbnail"  
         src="./assets/img/box.png"> 
      </td>
      </tr>
      </table>


## Atomicity 

* An *atomic* action is something that is guaranteed to execute without
  interruption

* Since the execution of different threads is interleaved, what are the atomic
  actions?

  - Single instructions?

  - Basic code blocks? 

  - *Answer*: might not specified by the language design. We have to assume the
     worst! Context switch can occur anywhere, also in the middle of a
     statement.

* What if flash is not atomic for the Cremona display?

  <pre class="prettyprint lang-java"> 
  while (true) {
        window.flash("Buy @ Cremona!");
        try { Thread.sleep(buy_pause); }
        catch (InterruptedException e) {}
  }
  </pre>

  <pre class="prettyprint lang-java"> 
  while (true) {
        window.flash("Free beer!");
        try { Thread.sleep(beer_pause); }
        catch (InterruptedException e) {}
  }
  </pre>

## Example: The Liseberg counter

* How many people have entered Liseberg at any given time?

  - Each entrance has turnstiles which record when a person enters or leaves

    <img class="img-thumbnail" src="./assets/img/turnstyles.png" height="110%" width="110%">   

  <pre class="prettyprint lang-java linenums">
  public class Liseberg {
    private int counter = 0 ; 
    private Thread east ;
    private Thread west ; 

    public void done() {
      System.out.println("Counter: "+counter);
    }

    public void enter() {
      counter++;
    }

    public void people (String x) {
        for(int j = 0; j<100; j++) {
           try { Thread.sleep(500 + (int)(Math.random()*1000)) ;}
           catch (InterruptedException e) {} ;
           System.out.println("Process "+x+" enters "+j);
           enter();
        } ;
        done() ; 
    }

    public Liseberg () {
      east = new Thread() {
                public void run() { people("East") ; }
              } ;
      west = new Thread() {
                public void run() { people("West") ; }
              } ;
      east.start() ;
      west.start() ;   
    }

    public static void main(String[] args) {
      new Liseberg();
    }
  }
  </pre>
  
  - [Here, the code](./assets/java/Liseberg.java)

  - To appreciate better the lack of atomicity, change the lines 
    15-18 by the following ones 

    <pre class="prettyprint lang-java">
    for(int j = 0; j<1000000; j++) {
        //try { Thread.sleep(500 + (int)(Math.random()*1000)) ;}
        //catch (InterruptedException e) {} ;
        //System.out.println("Process "+x+" enters "+j);
    </pre>

## What is the answer? 

* We expect the answer 200

  - It depends on the `counter++` operation being *atomic*

  - Let us disassemble `Liseberg.class` 
    (we use the [decompiler Jad](http://varaneckas.com/jad/) for that)

    <pre class="prettyprint lang-java">
    public void enter() {
        counter++;
    }
    </pre>

    <pre class="prettyprint lang-jvm">
    0:aload_0         
    1:dup             
    2:getfield        #6   Field int counter
    5:iconst_1        
    6:iadd            
    7:putfield        #6   Field int counter
    10:return  
    </pre>

  - From, above line `2` gets the content of the field `counter` in the stack
    and line 7 stores `counter+1` back into that field

## Terminology: States and traces

* A program executes a sequence of atomic actions

* A *state* is the value of the program variables at any point in time

* A *trace* (or history) is a sequence of states that can be produced by the
  sequence of atomic actions of a program

## A bad trace 

* Suppose the first atomic actions of the Turnstile processes are interleaved as
  follows:


  <table border="0" width="100%">
  <tr> 
  <th> Turnstile East </th>
  <th> Turnstile West </th>
  </tr>  
  </table>
  <p class="state text-center"> counter = 0</p>  
  <table border="0" width="100%">
  <tr>
  <td width="50%"> 
    <pre class="prettyprint lang-jvm">
    0:aload_0         
    1:dup             
    2:getfield #counter
    5:iconst_1        
    6:iadd            
    </pre>
  </td>
  <td> </td>
  </tr>
  <tr>
  <td> </td>
  <td width="50%">
    <pre class="prettyprint lang-jvm">
    0:aload_0         
    1:dup             
    2:getfield #counter
    5:iconst_1        
    6:iadd            
    </pre>
  </td>
  </tr> 
  </table>
  <p class="state text-center"> counter = 0</p>  
  <table border="0" width="100%">
  <tr>
  <td width="50%"> 
    <pre class="prettyprint lang-jvm">
    7:putfield #counter
    10:return  
    </pre>
  </td>
  <td> </td>
  </tr>
  </table>
  <p class="state text-center"> counter = 1</p>  
  <table border="0" width="100%">
  <tr>
  <td> </td>
  <td width="50%"> 
    <pre class="prettyprint lang-jvm">
    7:putfield #counter 
    10:return  
    </pre>
  </td>
  </tr>
  </table>
  <p class="state text-center"> counter = 1</p>  

  - We lost one person on the way!

## Program properties 

* A `property` of a program is a logical statement that is true for every possible
   trace

* Two kinds of property are usual for stating correctness properties of
  concurrent programs

  - ** Safety property **: a trace never enters a *bad* state

  - ** Liveness property**: every trace eventually reaches a *good* state

* Examples of safety properties 

  - The program never produces a wrong answer

  - An invariant like `(x + y < 2)`

* Examples of liveness properties

  - The thread terminates
  
  - The thread eventually calls a certain procedure

## Synchronisation 

* Synchronisation is the restriction of the traces of a concurrent program in
  order to guarantee certain safety properties

* We will see at least two kinds of synchronisation: 

  - Mutual exclusion

   - Conditional synchronisation

## Critical sections 

* The **bad** traces in the Liseberg problem are caused by the code that
  implements `counter++`

* To fix the problem it must be executed atomically

  - Without any interleaving or parallel activity

* Critical section

  - A part of a program that must be *executed atomically*

## Mutual exclusion 

* It is the property that only one thread can execute in a given piece of 
  code at any given time 

* How can we achieve it?

  - Theory: possible with just shared variables

     - Very inefficient at the programming language level, but 
       sometimes necessary in very low-level of abstraction (Hardware)

  - Practice: programming language features (semaphores, monitors, etc.) 

## Summary 

* Today’s lecture

  - Introduction to concurrency
  - Threads in Java
  - The shared update problem (Liseberg): mutex

* Next time 

  - Solving the shared update problem Introduction to a first programming
  - language construct for synchronisation (semaphores)
