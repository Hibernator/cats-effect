package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.io.StdIn

object Effects:

  // pure functional programming
  /*
    pure functional program is just a giant expression computing a single value
    - composed of smaller expressions
    - does not produce any side effects
   */

  // substitution - an expression can be replaced with the value it evaluates to
  // - functional programming relies on this heavily
  // - it's called referential transparency

  def combine(a: Int, b: Int): Int = a + b
  val five: Int = combine(2, 3)
  val five_v2: Int = 2 + 3
  val five_v3 = 5
  // here we replace the function with its implementation and eventually with its value

  // In a pure functional program, we can replace the expression with its value via the substitution model
  // referential transparency
  // - can replace an expression with its value as many times as we want without changing the behaviour of the program

  // In impure functional program, referential transparency is broken, substitution does not work
  // example: print to the console
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = () // not the same
  // the behaviour of the program has changed

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = anInt += 1
  val changingVar_v2: Unit = () // not the same

  // side effects are inevitable for useful programs

  // Effect - bridges the necessity for side effects and desire to have pure functional program
  // - embodies the concept of side effect in purely functional code

  /*
    Effect type properties:
    - we want to know what kind of side effect is produced by looking at type signature of the effect
      - type signature describes the kind of calculation that will be performed
    - type signature describes the VALUE that will be calculated
    - construction of the effect is separated from execution - when side effects are needed
      - when side effects are needed, effect construction is separate from effect execution
   */

  /* Example: Option is an effect type
    - kind of calculation - describes a possible absent value
    - value that will be calculated - computes a value of type A if it exists
    - no side effects are needed, so it can be calculated immediately
   */
  val anOption: Option[Int] = Option(42)

  /*
    example: Future is NOT an effect type
    - kind of calculation - describes an asynchronous computation that will be performed at some point in the future
    - value that will be calculated - computes a value of type A if it's successful
    - side effect is required (allocating/scheduling a thread) - execution is NOT separated from construction
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /* example: IO is an effect type, most general possible
    - kind of calculation - describes any computation than might produce side effects
    - value that will be calculated - calculates a value of type A if it's successful
    - side effect is required if the zero lambda () => A requires them
      - the creation of MyIO does not produce side effects on construction
   */

  // Wraps a computation that might produce side effects if the unsefeRun method is called
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something")
    42
  })

  /*
    Exercises
    1. An IO which returns the current time of the system (System.currentTimeMillis)
    2. An IO which measures the duration of a computation (IO.unsafeRun). Hint: use exercise 1
    3. An IO which prints something to the console
    4. An IO which reads a line (a string) from the std input
   */

  // 1
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] =
    val intermediate = clock.flatMap: startTime =>
      computation.map(_ => startTime)
    intermediate.flatMap: startTime =>
      clock.map(_ - startTime)

  def measureFor[A](computation: MyIO[A]): MyIO[Long] =
    for
      startTime <- clock
      _ <- computation
      endTime <- clock
    yield endTime - startTime

  /*
    Let's deconstruct the for-comprehension:
    clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

    clock.map(finishTime => finishTime - startTime) = MyIO(() => clock.unsafeRun() - startTime)
    clock.map(finishTime => finishTime - startTime) = MyIO(() => System.currentTimeMillis() - startTime)

    clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime)))

    computation.flatMap(lambda) = MyIO(() => lambda(___COMP___).unsafeRun())
                                = MyIO(() => MyIO(() => System.currentTimeMillis() - startTime)).unsafeRun())
                                = MyIO(() => System.currentTimeMillis_after_computation() - startTime)

    clock.flatMap(startTime => MyIO(() => System.currentTimeMillis_after_computation() - startTime)
    MyIO(() => lambda(clock.unsafeRun()).unsafeRun())
    MyIO(() => lambda(System.currentTimeMillis()).unsafeRun())
    MyIO(() => MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis()).unsafeRun()))
    MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis_at_start())
   */

  // The deconstruction is really complicated and we don't need to think about it too much
  // Better to think in terms of the for-comprehension and data structures transformations

  def testTimeIO(): Unit =
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())

  // 3
  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  // 4
  val read: MyIO[String] = MyIO(() => StdIn.readLine())

  // let's combine putStrLn and read
  def testConsole(): Unit =
    val program: MyIO[Unit] = for
      line1 <- read
      line2 <- read
      _ <- putStrLn(s"$line1$line2")
    yield () // yielding Unit is a common pattern in cats-effect

    program.unsafeRun()

  /*
    The testConsole method look very similar to imperative programming like this:
    line1 = readFromConsole()
    line2 = readFromConsole()
    print(line1 + line2)

    But here we are describing an imperative program through MyIO data structure transformations with pure functional programming
   */

  def main(args: Array[String]): Unit = {
    anIO.unsafeRun() // only here the side effect is performed
    println(measure(MyIO(() => Thread.sleep(1000))).unsafeRun()) // measure the duration of a computation
    println(measureFor(MyIO(() => Thread.sleep(1000))).unsafeRun()) // measure the duration of a computation
    testTimeIO()
    testConsole()
  }
