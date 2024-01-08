package part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {
  // An IO is a data type which describes anything that may produce side effects
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not have side effects
  val aDelayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    54
  }

  val shouldNotDoThis: IO[Int] = IO.pure { // this will be evaluated eagerly
    println("I'm producing an integer")
    53
  }

  val aDelayedIO_v2: IO[Int] = IO { // apply is equal to delay
    println("I'm producing an integer")
    54
  }

  // map, flatMap - transform IO data structures into other data structures
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples

  import cats.syntax.apply._

  val combinedMeaningOfLife: IO[Int] = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /*
  - Excercises
   */

  // 1 - sequence two IOs and take the result of the LAST one -- use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen operator

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // andThen with by-name call // lazy evaluation on the second one

  // 2 - sequence two IOs and take the result of the FIRST one -- use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob // andThen take first

  // 3 - repeat an IO forever -- use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io) // same

  def forever_v3[A](io: IO[A]): IO[A] =
    io *> forever_v3(io) // same

  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion -- method from library

  // 4 - convert and IO to a different type -- use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value) // same as maap

  // 5 - discard a value inside an IO, just return unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // discouraged - don't use this

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] =
    ioa.void // same - encouraged


  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n == 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      lastNumber <- IO(n)
      prevSum <- sumIO(n - 1)
    } yield prevSum + lastNumber

  // 7 - write a fibonacci IO that does NOT crash on recursion -- use recursion, ignore exponential complexity, use flatMap
  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO.defer(fibonacci(n - 1)) // by wrapping recursive calls in IOs we get stack safety
      prev <- IO.defer(fibonacci(n - 2)) // suspending effecting by wrapping it inside an effect defer is the same as delay....... .flatten
    } yield last + prev

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // EC for cats effect IO
    println(aDelayedIO.unsafeRunSync())
    println(smallProgram().unsafeRunSync())
  }


}
