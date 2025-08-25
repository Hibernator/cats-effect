package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.{Fiber, Outcome}
import cats.effect.{FiberIO, IO, IOApp}
import com.rockthejvm.utils.ioDebug

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Fibers extends IOApp.Simple:

  val meaningOfLife: IO[Int] = IO.pure(42)
  val favlang: IO[String] = IO.pure("Scala")

  // this will run sequentially
  def sameThreadIOs(): IO[Unit] = for
    _ <- meaningOfLife.ioDebug
    _ <- favlang.ioDebug
  yield ()

  // Fiber introduction
  // - similar to JVM threads but specific to cats-effect
  // - description of a computation that will run on a thread scheduled and managed by cats-effect runtime
  // - very lightweight, not like Java threads

  /*
    Fiber type parameters:
    - effect type - IO in our case
    - error type - doesn't have to be a Throwable
    - desirable value type
   */
  // it's almost impossible to craete fibers manually, cats-effect API has to be used
  def createFiber: Fiber[IO, Throwable, String] = ???

  // wraps a fiber in another IO because allocation of a fiber is an effectful operation (might produce a side effect)
  // type FiberIO[A] = Fiber[IO, Throwable, A]
  // the fiber is not actually started but the fiber allocation is wrapped in another effect
  val aFiber: IO[FiberIO[Int]] = meaningOfLife.ioDebug.start

  def differentThreadIOs(): IO[Unit] =
    for
      _ <- aFiber
      _ <- favlang.ioDebug
    yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] =
    for
      fib <- io.start // fib is an actual fiber here, will be performed on a different thread
      result <- fib.join // also an effect, which waits for the fiber to terminate
    yield result
    /*
      fib.join returns an outcome of the fiber computation (wrapped in effect)
      Outcome type parameters: effect type (IO), error type (Throwable), desirable type (A)
      IO[ResultType of fib.join]
      fib.join = Outcome[IO, Throwable, A]

      Possible outcomes:
      - success with an IO
      - failure with an exception
      - cancelled
     */

  // Fibers are low-level concurrency primitives, that allow cancellation at specific points in computation

  // handling the outcome of a fiber
  val someIoOnAnotherThread: IO[Outcome[IO, Throwable, Int]] = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread: IO[Int] = someIoOnAnotherThread.flatMap:
    case Succeeded(effect) => effect
    case Errored(e)        => IO(0)
    case Canceled()        => IO(0)

  def throwOnAnotherThread =
    for
      fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
      result <- fib.join
    yield result

  def testCancel() =
    val task = IO("starting").ioDebug >> IO.sleep(1.second) >> IO("done").ioDebug // sleep is non-blocking
    // handling the cancellation, useful for resource cleanup
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled!").ioDebug.void)

    for
//      fib <- task.start // on a separate thread
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("cancelling").ioDebug // running on the calling thread (main)
      _ <- fib.cancel // sends cancellation signal to the fiber from the calling thread
      result <- fib.join
    yield result

  /*
    Exercises

    1. Write a function that runs and IO on another thread, and, depending on the result of the fiber
      - returns the result in an IO
      - if errored or cancelled, returns a failed IO

    2. Write a function that takes 2 IOs, runs them on different fibers and returns an IO with a tuple containing
       both results
      - if both IOs complete successfully, tuple their results
      - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
      - if the first IO doesn't error but the second IO returns an error, raise that error
      - if one (or both) are canceled, raise a RuntimeException

    3. Write a function that adds a timeout to an IO:
      - IO runs on a fiber
      - if the timeout duration passes, then the fiber is canceled (I have to start a timeout IO at the same time)
      - the method returns an IO[A] which contains
        - the original value if the computation is successful before the timeout signal
        - the exception if the computation is failed before the timeout signal
        - a RuntimeException if ittimes out (i.e. canceled by the timeout)
   */

  // 1.
  def processResultsFromFiber[A](io: IO[A]): IO[A] = // do the pattern match
    io.start
      .flatMap(_.join)
      .flatMap:
        case Succeeded(effect) => effect
        case Errored(e)        => IO.raiseError(e)
        case Canceled()        => IO.raiseError(new RuntimeException("computation cancelled"))

  def processResultsFromFiberFor[A](io: IO[A]): IO[A] =
    for
      fiber <- io.start
      outcome <- fiber.join
      result <- outcome match
        case Succeeded(effect) => effect
        case Errored(e)        => IO.raiseError(e)
        case Canceled()        => IO.raiseError(new RuntimeException("computation cancelled"))
    yield result

  def testEx1() =
    val aComputation = IO("starting").ioDebug >> IO.sleep(1.second) >> IO("done").ioDebug >> IO(42).ioDebug
    processResultsFromFiber(aComputation).void

  // 2.
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] =
    for
      fibA <- ioa.start
      fibB <- iob.start
      outcomeA <- fibA.join
      outcomeB <- fibB.join
      result <- (outcomeA, outcomeB) match
        case (Succeeded(effectA), Succeeded(effectB)) =>
          for
            a <- effectA
            b <- effectB
          yield (a, b)
        case (Errored(_) | Canceled(), Errored(_) | Canceled()) =>
          IO.raiseError(new RuntimeException("computation failed completely"))
        case (Errored(e), _) => IO.raiseError(e)
        case (Canceled(), _) => IO.raiseError(new RuntimeException("first computation cancelled"))
        case (_, Errored(e)) => IO.raiseError(e)
        case (_, Canceled()) => IO.raiseError(new RuntimeException("second computation cancelled"))
    yield result

  def testEx2() =
    val firstIO = IO.sleep(2.seconds) >> IO(1).ioDebug
    val secondIO = IO.sleep(3.seconds) >> IO(2).ioDebug
    tupleIOs(firstIO, secondIO).ioDebug.void

  // 3.
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    for
      fib <- io.start
      // start the timeout on a different fiber so that we don't wait for this if the original task completes before
      // need to be careful - fibers can leak, or rather the resources they handle
      _ <- (IO.sleep(duration) >> fib.cancel).start
      outcome <- fib.join
      result <- outcome match
        case Succeeded(effect) => effect
        case Errored(e)        => IO.raiseError(e)
        case Canceled()        => IO.raiseError(new RuntimeException("Computation canceled"))
    yield result

  def testEx3() =
    val aComputation = IO("starting").ioDebug >> IO.sleep(1.second) >> IO("done").ioDebug >> IO(42).ioDebug
    timeout(aComputation, 100.millis).ioDebug.void

  override def run: IO[Unit] =
//    sameThreadIOs()
//    differentThreadIOs()
//    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .ioDebug.void
//    throwOnAnotherThread.ioDebug.void
//    testCancel().ioDebug.void // "done" won't be printed because the task was cancelled
//    processResultsFromFiberFor(favlang).ioDebug.void
//    tupleIOs(IO.raiseError(new RuntimeException("error1")), IO.raiseError(new RuntimeException("error2"))).ioDebug.void
//    timeout(meaningOfLife <* IO.sleep(2.seconds), 1.second).ioDebug.void
//    testEx1()
//    testEx2()
    testEx3()
