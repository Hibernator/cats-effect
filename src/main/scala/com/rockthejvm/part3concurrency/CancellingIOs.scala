package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.ioDebug

import scala.concurrent.duration.DurationInt

object CancellingIOs extends IOApp.Simple:

  /*
    Canceling IOs
    - fib.cancel - this we know already
    - IO.race & other APIs - this we also know already
    - manual cancellation - this is new
   */

  val chainOfIOs: IO[Int] = IO("waiting").ioDebug >> IO.canceled >> IO(42).ioDebug // the last IO won't be evaluated

  // Uncancelable - a wrapper over IO that prevents it from being canceled by other thread or fiber

  // example: online store, payment processor
  // payment process must NOT be canceled
  val specialPaymentSystem: IO[String] =
    (
      IO("Payment running, don't cancel me").ioDebug >>
        IO.sleep(1.second) >>
        IO("Payment completed").ioDebug
    ).onCancel(IO("MEGA CANCEL OF DOOM").ioDebug.void)

  val cancelationOfDoom: IO[Unit] =
    for
      fib <- specialPaymentSystem.start
      _ <- IO.sleep(500.millis) >> IO("attempting cancelation").ioDebug >> fib.cancel
      _ <- fib.join
    yield ()

  // cats-effect runtime will never apply cancellation effect to this even if requested
  val atomicPayment: IO[String] = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2: IO[String] = specialPaymentSystem.uncancelable

  val noCancelationOfDoom: IO[Unit] =
    for
      fib <- atomicPayment.start
      _ <- IO.sleep(500.millis) >> IO("attempting cancelation").ioDebug >> fib.cancel
      _ <- fib.join
    yield ()

  /*
    The uncancelable API is more complex and general.
    It takes a function from Poll[IO] to IO. In the example above we aren't using that Poll instance.
    The Poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   */

  /*
    Example: authentication service. Has two parts:
    - input password, can be canceled, because otherwise we might block indefinitely on user input
    - verify password, CANNOT be canceled once it's started
   */
  val inputPassword: IO[String] =
    IO("Input password:").ioDebug >> IO("typing password").ioDebug >> IO.sleep(2.seconds) >> IO("RockTheJVM1!")
  val verifyPassword: String => IO[Boolean] = (pw: String) =>
    IO("verifying...").ioDebug >> IO.sleep(2.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlowUncancelable: IO[Unit] = IO.uncancelable: poll =>
    for
      pw <- inputPassword.onCancel(IO("authentication timed out. Try again later.").ioDebug.void)
      verified <- verifyPassword(pw)
      _ <- if verified then IO("Authentication successful").ioDebug else IO("Authentication failed").ioDebug
    yield ()

  val authFlowCancelable: IO[Unit] = IO.uncancelable: poll =>
    for
      // unmasking inputPassword. It's cancelable now
      pw <- poll(inputPassword).onCancel(IO("authentication timed out. Try again later.").ioDebug.void)
      verified <- verifyPassword(pw) // this is still not cancelable
      _ <- if verified then IO("Authentication successful").ioDebug else IO("Authentication failed").ioDebug
    yield ()

  // cancelation won't work on authFlowUncancelable but will partially work on authFlowCancelable
  val authProgram: IO[Unit] =
    for
      authFib <- authFlowCancelable.start
      _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").ioDebug >> authFib.cancel
      _ <- authFib.join
    yield ()

  /*
    Uncancelable calls are MASKS which suppress cancellation.
    Poll calls are "gaps opened" in the uncancelable region.
    The cancellation is only suppressed in the masked region. not completely eliminated.
    It will kick in as soon as the program leaves the masked region.
   */

  /*
    Exercises
   */

  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).ioDebug // this will do nothing

  // What will happen here?
  // 42 will be printed because it is in uncancelable region
  // All cancelation signals are suppressed even if they come from the same fiber
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).ioDebug)
  // Uncancelable eliminates ALL cancel calls, except the unmasked parts wrapped in poll

  // 2
  // What will happen if authFlow is canceled in different stages?
  // There are nested masked regions, each with their own poll. Both need to be applied to enable cancelation
  // IO.uncancelable will mask the entire region. The gaps can only be opened with poll of that IO.uncancelable
  // So in this case, the authFlowCancelable becomes uncancelable
  val invincibleAuthProgram: IO[Unit] =
    for
      authFib <- IO.uncancelable(_ => authFlowCancelable).start
      _ <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").ioDebug >> authFib.cancel
      _ <- authFib.join
    yield ()

  // 3
  // Here, we have a sequence of 3 effects inside masked region: cancelable, uncancelable, cancelable
  // The cancel signal is sent half-way through, in the middle of the uncancelable effect
  // Will the last effect run?
  // It will because the cancel signal is only suppressed until the cancelable regions is reached
  // The cancelation signal is applied to the whole chain, so it is acted upon by the first poll that encounters it
  def threeStepProgram(): IO[Unit] =
    val sequence = IO.uncancelable: poll =>
      poll(IO("cancelable").ioDebug >> IO.sleep(1.second)) >>
        IO("uncancelable").ioDebug >> IO.sleep(1.second) >>
        poll(IO("second cancelable").ioDebug >> IO.sleep(1.second))

    for
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("CANCELING").ioDebug >> fib.cancel
      _ <- fib.join
    yield ()

  override def run: IO[Unit] =
//    cancelationOfDoom
//    noCancelationOfDoom
//    authFlow
//    authProgram
//    cancelBeforeMol.void
//    uncancelableMol.void
//    invincibleAuthProgram.void
    threeStepProgram()
