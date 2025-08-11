package com.rockthejvm.part2effects

import cats.Traverse
import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.ioDebug

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple:

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future:
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length

  val workload: List[String] = List(
    "I quite like CE",
    "Scala is great",
    "Looking forward to some awesome stuff"
  )

  def clunkyFutures(): Unit = // to make sure the futures only start when this method is called
    val futures: List[Future[Int]] = workload.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain. This is where the traverse concept becomes useful
    futures.foreach(_.foreach(println))

  import cats.instances.list.*
  val listTraverse: Traverse[List] = Traverse[List] // obtain the Traverse type clas instance for List

  // Traverse
  def traverseFutures(): Unit =
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(heavyComputation)
    singleFuture.foreach(println)

  // Traversing IOs

  // Same example but with IOs instead of Futures
  def computeAsIO(string: String): IO[Int] = IO { // same as Future but suspended in IO instead
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.ioDebug

  val ios: List[IO[Int]] = workload.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workload)(computeAsIO) // IOs executed sequentially

  // Parallel traversal
  import cats.syntax.parallel.* // Parallel extension methods from cats
  val parallelSingleIO: IO[List[Int]] = workload.parTraverse(computeAsIO) // IOs executed in parallel

  // Exercises

  // hint: Use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listTraverse.traverse(listOfIOs)(identity)

  // hard version
  import cats.syntax.traverse.*
  def sequenceGeneral[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] = wrapperOfIOs.traverse(identity)
  def sequenceGeneralDaniel[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)
    // instance of Traverse fetched manually instead of using extension method

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listOfIOs.parTraverse(identity)

  // hard parallel version
  def parSequenceGeneral[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] = wrapperOfIOs.parTraverse(identity)

  // Existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  // Existing parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = ios.parSequence // extension method from the Parallel syntax package

  override def run: IO[Unit] =
//    singleIO.map(_.sum).ioDebug.void
    parallelSingleIO.map(_.sum).ioDebug.void
