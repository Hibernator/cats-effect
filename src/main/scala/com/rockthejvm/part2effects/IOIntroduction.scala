package com.rockthejvm.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction:

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not have side effects
  val aDelayedIo: IO[Int] = IO.delay:
    println("I'm producing an integer")
    54

  val shouldNotDoThis: IO[Int] = IO.pure:
    println("I'm producing an integer")
    54

  val aDelayedIo_v2 = IO: // IO.apply is an alias for IO.delay
    println("I'm producing an integer")
    54

  // map, flatMap
  val improvedMeaningOfLife: IO[Int] = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife: IO[Unit] = ourFirstIO.flatMap: meaningOfLife =>
    IO.delay(println(meaningOfLife))

  def smallProgram(): IO[Unit] =
    for
      line1 <- IO(StdIn.readLine())
      line2 <- IO(StdIn.readLine())
      _ <- IO.delay(println(line1 + line2))
    yield ()

  /*
    Useful integrations with cats-core library

    mapN from Apply - combine IO effects as tuples
   */

  import cats.syntax.apply.*
  val combinedMeaningOfLife: IO[(Int)] =
    (ourFirstIO, improvedMeaningOfLife).mapN(_ + _) // maps a tuple to something else

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global // "platform" to run IOs, similar to ExecutionContext or thread pool

    // unsafeRunSync should only be called at "end of the world" (end of the program, main method)
    println(aDelayedIo.unsafeRunSync())
    println(smallProgram().unsafeRunSync())
    println(smallProgram_v2().unsafeRunSync())
