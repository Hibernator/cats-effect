package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.ioDebug

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple:

  // use-case: manage a connection lifecycle (replaceable resource)
  class Connection(url: String):
    def open(): IO[String] = IO(s"opening connection to $url").ioDebug
    def close(): IO[String] = IO(s"closing connection to $url").ioDebug

  val asyncFetchUrl: IO[Unit] =
    for
      fib <- (Connection("rockthejvm.com").open() *> IO.sleep(Int.MaxValue.seconds)).start
      _ <- IO.sleep(1.second) *> fib.cancel
    yield ()
  // Problem: The connection was opened but never closed - leaking resources

  // This is the correct way but tedious and hard to read (if the resources are more complex)
  val correctAsyncFetchUrl: IO[Unit] =
    for
      connection <- IO(Connection("rockthejvm.com"))
      // here the cancellation handler was added
      fib <- (connection.open() *> IO.sleep(Int.MaxValue.seconds).onCancel(connection.close().void)).start
      _ <- IO.sleep(1.second) *> fib.cancel
    yield ()

  // bracket pattern - developers are compelled to think about using resource and handling cancellations and errors
  // bracket pattern: someIO.bracket(useResouceCallback)(releaseResourceCallback)
  // bracket is equivalent to try-catch but it's pure FP
  val bracketFetchUrl: IO[Unit] = IO(Connection("rockthejvm.com")) // acquire resource
    .bracket(connection => connection.open() *> IO.sleep(Int.MaxValue.seconds)) // use resource
    (connection => connection.close().void) // release resource - happens in any case (error, success)

  val bracketProgram: IO[Unit] = // same as correctFetchUrl
    for
      fib <- bracketFetchUrl.start
      _ <- IO.sleep(1.second) *> fib.cancel
    yield ()

  /*
    Exercise - use bracket paettern to open a file with text, print all lines (one every 100ms) and close the file
    - open a scanner - use openFileScanner method
    - read file line by line, every 100 millis (can use sleep)
    - close the scanner
    - if cancelled/throws error, close the scanner
   */

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[Unit] =
    def readAndPrintLine(scanner: Scanner): IO[Unit] =
      if scanner.hasNextLine then IO(println(scanner.nextLine())) *> IO.sleep(100.millis) >> readAndPrintLine(scanner)
      else IO.unit

    openFileScanner(path).bracket(scanner => readAndPrintLine(scanner))(scanner => IO(scanner.close()))

  // Daniel's solution
  def readLineByLine(scanner: Scanner): IO[Unit] =
    if scanner.hasNextLine then IO(scanner.nextLine()).ioDebug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFileDaniel(path: String): IO[Unit] =
    IO(s"opening file at $path").ioDebug *> openFileScanner(path).bracket(scanner => readLineByLine(scanner))(scanner =>
      IO(s"closing file at $path").ioDebug *> IO(scanner.close())
    )

  // Also scheduling and threads synchronization is taken care of by bracket, no need to spawn fibers manually

  override def run: IO[Unit] = {
//    bracketProgram.void
//    bracketReadFile("./src/main/resources/lines.txt")
    bracketReadFileDaniel("./src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
  }
