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
  val combinedMeaningOfLife: IO[Int] =
    (ourFirstIO, improvedMeaningOfLife).mapN(_ + _) // maps a tuple to something else

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /*
    Exercises
   */

  // 1 - sequence two IOs and take the result of the last one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa.flatMap(_ => iob)

  // eager andThen. IO is created eagerly but not evaluated
  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa *> iob
  // IO.flatMap is very similar and it is stack safe!!! This one is not

  // andThen "by-name" (iob passed by name), even the IO itself is not created immediately
  // This is stack-safe
  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa >> iob

  // 2 - sequence two IOs and take the result of the first one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirstFor[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    for
      a <- ioa
      _ <- iob
    yield a

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa <* iob // andThen and take first (ioa before iob)

  // 3 - repeat an IO effect forever
  // hint: use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] = io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] = io >> forever_v2(io) // lazy andThen is useful here to avoid stack overflow
  // if used eager andThen, it would cause stack overflow. We would have a lot of unevaluated ios

  def forever_v3[A](io: IO[A]): IO[A] = io *> forever_v3(io) // this will cause stack overflow

  def forever_v4[A](io: IO[A]): IO[A] = io.foreverM // uses tail recursion, so it does not cause stack overflow

  // 4 - convert an IO to a different type
  // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  // 5 - discard value inside IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = convert(ioa, ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] = ioa.as(()) // not very well readable, the one below is better

  def asUnit_v4[A](ioa: IO[A]): IO[Unit] = ioa.void

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if n <= 0 then 0 else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    def inner(ion: IO[(Int, Int)]): IO[Int] =
      ion.flatMap: (current, acc) =>
        if current <= 0 then IO.pure(acc)
        else inner(IO(current - 1, acc + current))
    inner(IO(n, 0))

  // Since IO.flatMap is stack safe, this recursion is stack-safe
  def sumIOFor(n: Int): IO[Int] =
    if n <= 0 then IO.pure(0)
    else
      for
        lastNumber <- IO(n)
        previousSum <- sumIOFor(n - 1)
      yield previousSum + lastNumber

  // 7 (hard) - write a Fibonacci function that does not crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatMap heavily
  def fibonacci(n: Int): IO[BigInt] =
    if n == 0 then IO.pure(BigInt(0))
    else if n == 1 then IO.pure(BigInt(1))
    else
      for
        // it's important to wrap the recursive calls in IO to prevert stack overflow
//        last <- IO(fibonacci(n - 1)).flatMap(x => x)
        last <- IO(fibonacci(n - 1)).flatten // same as flatMap(identity)
        previous <- IO.defer(fibonacci(n - 2)) // same as IO.delay(...).flatten
      yield last + previous

  def fibonacciNaive(n: Int): BigInt =
    if n == 0 then BigInt(0)
    else if n == 1 then BigInt(1)
    else fibonacciNaive(n - 1) + fibonacciNaive(n - 2)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global // "platform" to run IOs, similar to ExecutionContext or thread pool

    // unsafeRunSync should only be called at "end of the world" (end of the program, main method)
    println(aDelayedIo.unsafeRunSync())
//    println(smallProgram().unsafeRunSync())
//    println(smallProgram_v2().unsafeRunSync())

    println("takeLast")
    println(sequenceTakeLast(IO(1), IO(2)).unsafeRunSync())
    println("takeLFirst")
    println(sequenceTakeFirst(IO(1), IO(2)).unsafeRunSync())
//    forever(IO(println("running forever"))).unsafeRunSync()
    println("convert")
    println(convert(IO(println("hello")), "world").unsafeRunSync())
    println("sumIO")
    println(sumIO(100000).unsafeRunSync())
    println("fibonacci naive")
    println(fibonacci(60).unsafeRunSync())
