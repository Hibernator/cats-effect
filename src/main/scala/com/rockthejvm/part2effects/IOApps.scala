package com.rockthejvm.part2effects

import cats.effect.{ExitCode, IO, IOApp}
import com.rockthejvm.part2effects.IOApps.program

import scala.io.StdIn

object IOApps:
  val program: IO[Unit] =
    for
      line <- IO(StdIn.readLine())
      _ <- IO(println(s"You entered: $line"))
    yield ()

object TestApp:

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    program.unsafeRunSync()

object FirstCatsEffectApp extends IOApp:

  // main method is implemented in terms of run
  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)

object MySimpleApp extends IOApp.Simple:
  override def run: IO[Unit] = program
