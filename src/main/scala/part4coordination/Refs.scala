package part4coordination

import cats.effect.{IO, IOApp, Ref}
import scala.concurrent.duration.DurationInt

/** Ref recap/summary
 * - Why: purely functional, thread-safe state management
 * - Purely functional atomic reference
 * - Interacting with a Ref is an effect
 * -- Setting a new value
 * -- Getting existing value
 * -- Getting + setting atomically
 * -- updating with a function
 * -- updating + getting (old value or new value)
 * -- modifying + surfacing an external value
 */

object Refs extends IOApp.Simple {

  // ref = purely functional atomic reference
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  // ref is a wrapper over a value which you can edit in a purely funtional way
  // modifying is an effect
  val increasedMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(43) // this will always be thread-safe
  }

  // obtain a value
  val mol = atomicMol.flatMap { ref =>
    ref.get // thread-safe
  }

  val gsMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.getAndSet(43)
  } // gets the old value, sets the new one

  // updating with a function - update value with old value * 10
  val fMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.update(value => value * 10)
  }

  val updatedMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.updateAndGet(value => value * 10) // get the new value
    // can also use getAndUpdate to get the OLD value
  }

  // modifying with a function returning a different type
  val modifiedMol: IO[String] = atomicMol.flatMap { ref =>
    ref.modify(value => (value * 10, s"my current value is $value"))
  }

  // why: concurrent + thread-safe reads/writes over shared values, in a purely functional way

  import cats.syntax.parallel._
  import utils._

  // Some non thread safe code
  def demoConcurrentWorkImpure(): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").debug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").debug
        _ <- IO(count += wordCount)
      } yield ()
    }

    List("I love Cats Effect", "This is to with refs", "Multi threading is very important")
      .map(task)
      .parSequence // Traverse type inside out
      .void
  }
  /*
    Drawbacks:
    - hard to read/debug
    - mix pure/impure code
    - NOT THREAD SAFE - programs correctness is not guaranteed
   */

  // The following code will always return the correct result in a thread safe way
  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").debug
        newCount <- total.updateAndGet(currentCount => currentCount + wordCount)
        _ <- IO(s"New total: $newCount").debug
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("I love Cats Effect", "This is to with refs", "Multi threading is very important")
        .map(string => task(string, initialCount))
        .parSequence
    } yield ()
  }

  /*
  Excercises - refactor impure program into a pure program
   */
  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1) // not thread safe -- shared state
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled // running on separate fibers
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_ + 1) // thread safe effect
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      t <- ticks.get
      _ <- IO(s"TICKS: $t").debug
      _ <- printTicks(ticks)
    } yield ()

    for {
      tickRef <- Ref[IO].of(0)
      _ <- (tickingClock(tickRef), printTicks(tickRef)).parTupled
    } yield ()
  }

  // This is a incorrect implementation
  def tickingClockWeird(): IO[Unit] = {
    val ticks = Ref[IO].of(0) // IO[ref] -- initialising atomic reference

    def tickingClock: IO[Unit] = for {
      t <- ticks // ticks will give you a NEW Ref -- since it is reinitialised
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- t.update(_ + 1) // thread safe effect
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks // ticks will give you a NEW Ref
      _ <- IO.sleep(5.seconds)
      currentTicks <- t.get
      _ <- IO(s"TICKS: $currentTicks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }



  override def run = {
//    demoConcurrentWorkImpure()
//    demoConcurrentWorkPure()
//    tickingClockImpure()
    tickingClockPure()
  }

}
