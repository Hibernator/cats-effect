package com.rockthejvm.part1recap

object CatsTypeClasses:

  /*
    - applicative
    - functor
    - flatMap
    - monad
    - apply
    - applicativeError/monadError
    - traverse
   */

  // functor - "mappable" data structures, has a map method
  trait MyFunctor[F[_]]:
    def map[A, B](initialValue: F[A])(f: A => B): F[B]

  import cats.Functor
  import cats.instances.list.*
  val listFunctor: Functor[List] = Functor[List]

  // generalizable "mapping" APIs
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.*
  def increment_v2[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)

  // applicative - ability to "wrap" types
  trait MyApplicative[F[_]] extends MyFunctor[F]:
    def pure[A](value: A): F[A]

  import cats.Applicative
  val applicativeList: Applicative[List] = Applicative[List]
  val aSimpleList: List[Int] = applicativeList.pure(43) // List(43)
  import cats.syntax.applicative.* // pure extension method
  val aSimpleList_v2: List[Int] = 43.pure[List] // List(43)

  // FlatMap - ability to chain multiple computations
  trait MyFlatMap[F[_]] extends MyFunctor[F]:
    def flatMap[A, B](initialValue: F[A])(f: A => F[B]): F[B]

  import cats.FlatMap
  val flatMapList: FlatMap[List] = FlatMap[List]
  import cats.syntax.flatMap.* // flatMap extension method
  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b))) // FlatMap extends Functor, therefore the map method is available

  // Monad
  // - bridges the gap between functional and imperative programming
  // - applicative + flatMap

  trait MyMonad[F[_]] extends MyApplicative[F], MyFlatMap[F]:
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] =
      flatMap(initialValue)(a => pure(f(a)))

  import cats.Monad
  val monadList: Monad[List] = Monad[List]
  // Since Monad is a combination of Applicative and FlatMap and doesn't add any new methods, no need to import anything

  def crossProduct_v2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for
      a <- fa
      b <- fb
    yield (a, b)

  /*
          Functor -> FlatMap ->
                  \            \
                   Applicative -> Monad
   */

  /*
  ApplicativeError
    - controls creation of F[A] instances through a raiseError method
    - data structure that holds errors of any type, doesn't have to be a Throwable. Very general
    - I can treat the data structure holding the error as F[A]
    - we can build "failed" instances of F[A] using raiseError
   */

  trait MyApplicativeError[F[_], E] extends MyApplicative[F]:
    def raiseError[A](error: E): F[A]

  import cats.ApplicativeError

  // a data structure that holds either a desirable value of type A or an undesirable value of type String
  // right-biased towards A
  type ErrorOr[A] = Either[String, A]

  // The ErrorOr type argument has to be Either with the correct undesirable type (String in this case)
  // Undesirable type of ApplicativeError has to be the same as undesirable type of Either (ErrorOr)
  val applicativeErrorEither: ApplicativeError[ErrorOr, String] = ApplicativeError[ErrorOr, String]

  val desirableValue: ErrorOr[Int] = applicativeErrorEither.pure(42) // Right(42)
  val faliedValue: ErrorOr[Int] = applicativeErrorEither.raiseError("Something failed") // Left("Something went wrong")

  import cats.syntax.applicativeError.* // imports raiseError extension method
  // Imported raiseError extension method is applicable to any type that has a given ApplicativeError instance
  // where the type is the error type
  val failedValue_v2: ErrorOr[Int] = "Something went wrong".raiseError[ErrorOr, Int]

  // MonadError

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E], MyMonad[F] // no new methods here (same as Monad)
  import cats.MonadError
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  // Traverse
  // F - list, G - option
  // The actual inner type can also be changed during mapping
  trait MyTraverse[F[_]] extends MyFunctor[F]:
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]

  // turns nested wrappers inside out

  // we want to turn a List[Option[Int]] into an Option[List[Int]]
  val listOfOptions: List[Option[Int]] = List(Some(1), Some(2), Some(43))
  import cats.Traverse
  val listTraverse: Traverse[List] = Traverse[List]
  // first argument is data structure with some values (List)
  // second argument is a function that turns every value into the second wrapper type
  // doesn't allow double nesting with the wrapper types, rather returns wrapper type inside out
  // for easier processing later
  // if we did List(1, 2, 3).map(x => Option(x)), we would get List(Option(1), Option(2), Option(3)),
  // which is inconvenient to process later
  val optionList: Option[List[Int]] = listTraverse.traverse(List(1, 2, 3))(x => Option(x))

  import cats.syntax.traverse.*
  val optionList_v2: Option[List[Int]] = List(1, 2, 3).traverse(x => Option(x))
  // List[Future[A]] -> Future[List[A]] would be more useful example

  def main(args: Array[String]): Unit = {}
