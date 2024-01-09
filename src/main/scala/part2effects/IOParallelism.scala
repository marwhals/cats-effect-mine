package part2effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.{IO, IOApp}

object IOParallelism  extends IOApp.Simple {

  //IOs are usually sequential
  val timIO = IO(s"""[${Thread.currentThread().getName}] Tim""")
  val bobIO = IO(s"""[${Thread.currentThread().getName}] Bob""")

  val composedIO = for {
    tim <- timIO
    bob <- bobIO
  } yield s"$tim and $bob love Rock the JVM"

  //adding debug extension method / implicit method
  import utils._
  import cats.syntax.apply._

  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String] = IO.delay("Scala")
  val goalInLife = (meaningOfLife.debug, favLang.debug).mapN((num, string) => s"""my goal in life is $num and $string""")

  //paralellism on IOs
  // convert a sequential IO to a paralell IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  import cats.effect.implicits._ // behind the scenes stuff
  val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"""my goal in life is $num and $string""")
  // turn back to sequential
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  //short hand
  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] = (meaningOfLife.debug, favLang.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  //regarding failure
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  // compose success + failure
  val parallelWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)
  // compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
  // the first effect to fail give the failure of the result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = {
//    composedIO.map(println)
//    goalInLife.map(println)
//    goalInLife.debug.void
//    goalInLife_v2.debug.void
//    goalInLife_v3.debug.void
//    twoFailures.debug.void
    twoFailuresDelayed.debug.void
  }

}
