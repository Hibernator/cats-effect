package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.{Fiber, Outcome}
import cats.effect.{FiberIO, IO, IOApp}
import com.rockthejvm.utils.ioDebug

import scala.concurrent.duration.DurationInt

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

  override def run: IO[Unit] = {
//    sameThreadIOs()
//    differentThreadIOs()
//    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .ioDebug.void
//    throwOnAnotherThread.ioDebug.void
    testCancel().ioDebug.void // "done" won't be printed because the task was cancelled
  }
