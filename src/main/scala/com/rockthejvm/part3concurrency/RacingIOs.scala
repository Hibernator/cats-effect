package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}
import com.rockthejvm.utils.ioDebug

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object RacingIOs extends IOApp.Simple:

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (IO(s"starting computation: $value").ioDebug >>
      IO.sleep(duration) >>
      IO(s"computation for $value done") >>
      IO(value))
      .onCancel(IO(s"computation CANCELED for $value").ioDebug.void)

  def testRace(): IO[String] =
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("scala", 2.seconds)

    // run both of them, race against each other and obtain the result of the computation that finished first
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be canceled
      - fibers and cancellation are managed automatically, no need for me to do it manually
     */

    first.flatMap:
      case Left(mol)   => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Favorite language won: $lang")

  // racePair method allows me to manage the fiber of the effect that lost (doesn't cancel it automatically)
  def testRacePair() =
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("scala", 2.seconds)

    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String]) // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)
    // The result of racePair is Either of Outcome of the winner and Fiber of the loser

    raceResult.flatMap:
      case Left(outMol, fibLang)  => fibLang.cancel >> IO("MOL won").ioDebug >> IO(outMol).ioDebug
      case Right(fibMol, outLang) => fibMol.cancel >> IO("Language won").ioDebug >> IO(outLang).ioDebug

  /*
    Exercises:

    1. - implement a timeout pattern with race
    2. - a method to return a LOSING effect from a race (hint: use racePair)
    3. - implement race in terms of racePair
   */

  // 1. Hint: race the given IO with another sleep IO
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(io, IO.sleep(duration))
      .flatMap:
        case Left(value) => IO(value)
        case Right(())   => IO.raiseError[A](new RuntimeException("timed out"))

  private val importantTask: IO[Int] = IO.sleep(2.seconds) >> IO(42).ioDebug
  val testTimeout = timeout(importantTask, 1.second)

  // There is a method for timeout in cats-effect already
  val testTimeout_v2 = importantTask.timeout(1.second)

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob)
      .flatMap:
        case Left(_, fiberB) =>
          fiberB.join.flatMap:
            case Succeeded(effectB) => effectB.map(Right.apply)
            case Errored(e)         => IO.raiseError(e)
            case Canceled()         => IO.raiseError(new RuntimeException("IO B lost but was canceled"))
        case Right(fiberA, _) =>
          fiberA.join.flatMap:
            case Succeeded(effectA) => effectA.map(Left.apply)
            case Errored(e)         => IO.raiseError(e)
            case Canceled()         => IO.raiseError(new RuntimeException("IO A lost but was canceled"))

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob)
      .flatMap:
        case Left(outcomeA, fiberB) =>
          outcomeA match
            case Succeeded(effectA) => fiberB.cancel >> effectA.map(Left.apply)
            case Errored(e)         => fiberB.cancel >> IO.raiseError(e)
            case Canceled()         =>
              fiberB.join.flatMap:
                case Succeeded(effectB) => effectB.map(Right.apply)
                case Errored(e)         => IO.raiseError(e)
                case Canceled()         => IO.raiseError(new RuntimeException("Both computation canceled"))
        case Right(fiberA, outcomeB) =>
          outcomeB match
            case Succeeded(effectB) => fiberA.cancel >> effectB.map(Right.apply)
            case Errored(e)         => fiberA.cancel >> IO.raiseError(e)
            case Canceled()         =>
              fiberA.join.flatMap:
                case Succeeded(effectA) => effectA.map(Left.apply)
                case Errored(e)         => IO.raiseError(e)
                case Canceled()         => IO.raiseError(new RuntimeException("Both computation canceled"))

  override def run: IO[Unit] =
//    testRace().ioDebug.void
//    testRacePair().void
//    timeout(IO(42), 1.second).ioDebug.void
//    timeout(IO(42) <* IO.sleep(2.seconds), 1.second).ioDebug.void
//    unrace(runWithSleep(42, 1.second), runWithSleep("scala", 2.seconds)).ioDebug.void
//    unrace(
//      runWithSleep(42, 1.second),
//      IO.sleep(2.seconds) >> IO.raiseError[String](new RuntimeException("Scala failed"))
//    ).ioDebug.void
    simpleRace(runWithSleep(42, 1.second), runWithSleep("scala", 2.seconds)).ioDebug.void
