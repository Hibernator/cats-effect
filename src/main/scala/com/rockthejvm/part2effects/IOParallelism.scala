package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple:

  // IOs are usually sequential
  val aniIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO: IO[String] = for
    ani <- aniIO
    kamran <- kamranIO
  yield s"$ani and $kamran love Rock the JVM"

  import com.rockthejvm.utils.*
  import cats.syntax.apply.*
  val meaningOfLife: IO[Int] = IO.delay(42)
  val favoriteLanugage: IO[String] = IO.delay("Scala")
  val goalInLife: IO[String] =
    (meaningOfLife.ioDebug, favoriteLanugage.ioDebug).mapN((num, str) => s"My goal in life is $num and $str")

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  // Parallel is a type class and here the compiler creates its instance for IO
  // It's a cats-core class that allows parallel composition of Applicatives or Monads
  val parallelIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.ioDebug)
  val parallelIO2: IO.Par[String] = Parallel[IO].parallel(favoriteLanugage.ioDebug)
  import cats.effect.implicits.*

  // this is evaluated on 2 or 3 different threads and synchronization between them happens automatically
  // It's a giant deal
  // This gathers results of first 2 threads into one, that performs the final computation
  val goalInLifeParallel: IO.Par[String] =
    (parallelIO1, parallelIO2).mapN((num, str) => s"My goal in life is $num and $str")
  // All the thread synchronization was done for us

  // turn the parallel version back to sequential
  // first, we obtain a type class instance of Parallel[IO] and then call sequential on it
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand for parallel computations - with help of Parallel type class and parMapN extension method
  import cats.syntax.parallel.*
  // parMapN does all the back-and-forth conversion
  val goalInLife_v3: IO[String] =
    (meaningOfLife.ioDebug, favoriteLanugage.ioDebug).parMapN((num, str) => s"My goal in life is $num and $str")

  // what happens if one of the parallel IOs fails?
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this"))

  // composing success and failure
  val parallelWithFailure: IO[String] = (meaningOfLife.ioDebug, aFailure.ioDebug).parMapN((num, str) => s"$num$str")

  // composing failure and failure
  // the first effect to fail gives the failure of the result
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] =
    (aFailure.ioDebug, IO(Thread.sleep(1000)) >> anotherFailure.ioDebug).parMapN((num, str) => s"$num$str")

  override def run: IO[Unit] =
//    composedIO.map(println)
//    goalInLife.map(println)
//    goalInLife_v3.ioDebug.void
//    parallelWithFailure.ioDebug.void
    twoFailures.ioDebug.void
