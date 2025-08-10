package com.rockthejvm.part2effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrorHandling:

  // What we know already: IO pure, delay, defer

  // How to create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("Failure!"))

  // Much more expressive and better readable than above statement
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))

  // Handling exceptions in IO
  val dealWithIt: IO[AnyVal] = aFailure.handleErrorWith:
    case _: RuntimeException => IO.delay(println("I'm still here"))

  // turn effect into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")

  // redeemWith: like a flatMap
  val resultAsEffect: IO[Unit] =
    aFailure.redeemWith(ex => IO(println(s"FAIL: $ex")), value => IO(println(s"SUCCESS: $value")))

  // Exercises

  // 1 - construct potentially failed IOs from standard data types: Option, Try, Either
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match
      case Some(value) => IO.pure(value)
      case None        => IO.raiseError(ifEmpty)

  def try2IO[A](aTry: Try[A]): IO[A] =
    aTry match
      case Success(value)     => IO.pure(value)
      case Failure(exception) => IO.raiseError(exception)

  def either2IO[A](either: Either[Throwable, A]): IO[A] =
    either match
      case Right(value) => IO.pure(value)
      case Left(error)  => IO.raiseError(error)

  // methods already exist in the cats-effect library: IO.fromTry, IO.fromOption, IO.fromEither

  // 2 - create two additional methods: handleError and handleErrorWith
  def handleIoError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.handleError(handler)
  def handleIoError_v2[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)
  def handleIoErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.handleErrorWith(handler)
  def handleIoErrorWith_v2[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    println(resultAsString.unsafeRunSync())
    println(handleIoError(IO("hello"))(_ => "default").unsafeRunSync())
