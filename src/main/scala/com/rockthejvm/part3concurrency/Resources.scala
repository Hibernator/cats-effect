package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
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
    Exercise - use bracket pattern to open a file with text, print all lines (one every 100ms) and close the file
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

  /*
    Resources - even better pattern than bracket
   */

  // Example: we want to read a file with a configuration and open a connection based on that - 2 resources

  def connectionFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        // acquire a connection based on the file
        IO(new Connection(scanner.nextLine())).bracket { connection =>
          connection.open().ioDebug >> IO.never // IO.never never terminates
        }(connection => connection.close().ioDebug.void)
      }(scanner => IO("closing file").ioDebug >> IO(scanner.close()))
  // here we have nested brackets and resources, which is not readable anymore

  // Resource concept - we acquire a resource and define a finalizer (release) but use it at a later place in code
  // Unlike bracket which has acqiusition, usage and release at the same place
  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(new Connection("rockthejvm.com")))(connection => connection.close().void)

  // at a later part the resource can be used, without having to care about acquisition and release
  val resourceFetchUrl: IO[Unit] =
    for
      fib <- connectionResource.use(connection => connection.open() >> IO.never).start
      _ <- IO.sleep(1.second) >> fib.cancel
    yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string").ioDebug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").ioDebug.void
  // now I can use it in both ways: bracket and resource
  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)

  /*
    Exercise: read a text file with one line every 100 millis, using Resource
    (refactor the bracket exercise to use Resource)
   */
  def resourceReadFile(path: String): IO[Unit] =
    val fileResource =
      Resource.make(IO(s"opening file at $path").ioDebug *> openFileScanner(path))(scanner =>
        IO("closing file").ioDebug >> IO(scanner.close())
      )
    fileResource.use(scanner => readLineByLine(scanner))

  // Daniel's solution
  def getResourceFromFile(path: String): Resource[IO, Scanner] =
    Resource.make(openFileScanner(path))(scanner => IO("closing file").ioDebug >> IO(scanner.close()))

  def danielResourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path").ioDebug >> getResourceFromFile(path).use(scanner => readLineByLine(scanner))

  // example of failing or cancellation and automatic resource release
  def cancelReadFile(path: String): IO[Unit] =
    for
      fib <- resourceReadFile(path).start
      _ <- IO.sleep(200.milli) >> fib.cancel
    yield ()

  /* benefits or resource:
   - decoupling logic of using the resource from the logic of acquiring and releasing it
   - code is more modular
   - easy to compose multiple resources
   */

  // Nested resources - map/flatMap/for-comprehension is available, resources can depend on each other

  // Refactoring the connectionFromConfig method from above that uses nested bracket
  def connectionFromConfigurationResource(path: String): Resource[IO, Connection] =
    Resource
      .make(IO("opening file").ioDebug >> openFileScanner(path))(scanner =>
        IO("closing file").ioDebug >> IO(scanner.close())
      )
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(connection => connection.close().void))

  // using flatMap is still not that readable, but it can be replaced with a for-comprehension
  def connectionFromConfigurationResourceClean(path: String): Resource[IO, Connection] =
    for
      scanner <- Resource
        .make(IO("opening file").ioDebug >> openFileScanner(path))(scanner =>
          IO("closing file").ioDebug >> IO(scanner.close())
        )
      connection <- Resource.make(IO(new Connection(scanner.nextLine())))(connection => connection.close().void)
    yield connection

  val openConnection = connectionFromConfigurationResourceClean("./src/main/resources/connection.txt")
    .use(connection => connection.open() >> IO.never)
  // connection and file will both close automatically

  // resources will also be closed when the usage IO is canceled from another thread
  val canceledConnection: IO[Unit] =
    for
      fib <- openConnection.start
      _ <- IO.sleep(1.second) >> IO("cancelling").ioDebug >> fib.cancel
    yield ()

  // finalizers - can be attached to regular IOs (effects)
  val ioWithFinalizer: IO[String] = IO("some resource").ioDebug.guarantee(IO("freeing resource").ioDebug.void)

  // can also have finalizers for different outcomes
  val ioWithFinalizer_v2: IO[String] = IO("some resource").ioDebug.guaranteeCase:
    case Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result").ioDebug).void
    case Errored(e)    => IO("nothing to release").ioDebug.void
    case Canceled()    => IO("resource got canceled, releasing what's left").ioDebug.void

  override def run: IO[Unit] =
//    bracketProgram.void
//    bracketReadFile("./src/main/resources/lines.txt")
//    bracketReadFileDaniel("./src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
//    resourceFetchUrl.void
//    resourceReadFile("./src/main/resources/lines.txt")
//    cancelReadFile("./src/main/resources/lines.txt")
//    openConnection.void
//    canceledConnection
    ioWithFinalizer.void
