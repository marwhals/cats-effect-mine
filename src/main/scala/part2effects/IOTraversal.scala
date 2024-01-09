package part2effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I liek CE", "Scala is w00p", "looking forward to the scalasss")
  val futures: List[Future[Int]] = workLoad.map(heavyComputation)
  // Future[List[Int]] would be hard to obtain
//  futures.foreach(_.foreach(println)) -- uncomment for print

  //traverse imports

  import cats.Traverse
  import cats.instances.list._
  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    // ^^ this stores all the results
    singleFuture.foreach(println)
  }

  import utils._

  //traverse for IOs
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  //parallel traversal
  import cats.syntax.parallel._ // parTraverse extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /*
  - Exercises
   */

  //use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(identity)

  // hard version
  def sequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  // hard version
  def parSequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(identity)

  // existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  // parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence // extension method from the Parallel syntax package

  override def run = {
//    singleIO.map(_.sum).debug.void
    parallelSingleIO_v3.void
  }

}
