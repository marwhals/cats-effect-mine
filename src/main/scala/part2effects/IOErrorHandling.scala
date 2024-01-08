package part2effects

import cats.effect.IO

import scala.util._

object IOErrorHandling {

  // IO: pure, delay, defer - we want to  create Io of failed computations
  // create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail")) // both the same but raiseError is more readable

  // handling exceptions
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
    // add more cases depending on the exceptions you want to handle
  }

  // use an either to deal with both the failed and succesful effect
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
  // redeem: transform the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")
  // redeemWith -- similar to flat map. The returned values are not simple values they are wrapper values - in this case IO
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(
    ex => IO(println(s"FAIL: $ex")),
    value => IO(println(s"SUCCESS: $value"))
  )

  // Excercies - continue from 8:57
  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match {
      case Some(value) => IO.pure(value)
      case None => IO.raiseError(ifEmpty)
    }

  def try2IO[A](aTry: Try[A]): IO[A] =
    aTry match {
      case Success(value) => IO.pure(value)
      case Failure(exception) => IO.raiseError(exception)
    }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] =
    anEither match {
      case Right(value) => IO.pure(value)
      case Left(exception) => IO.raiseError(exception)
    }

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
  //    io.redeem(ex => handler(ex), value => value)
    io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
  //    io.redeemWith(ex => handler(ex), value => value)
    io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    aFailedCompute.unsafeRunSync()
    dealWithIt.unsafeRunSync()
    println(resultAsString.unsafeRunSync())

  }

}
