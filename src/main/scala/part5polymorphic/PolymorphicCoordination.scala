package part5polymorphic

import cats.effect._
import cats.effect.kernel.Deferred
import utils.general._

import scala.concurrent.duration.DurationInt

/**
 * Takeaways/ summary
 * --- Concurrent == the abilty to create concurrency primitives
 * -- ref an deferred are the basic primitives
 * -- can create everything else in terms of them
 */

object PolymorphicCoordination extends IOApp.Simple {

  // Concurrent - describe any concurrent primtive using - Ref + Deferred for ANY effect type
  trait MyConcurrent[F[_]] extends Spawn[F] { //describe the creation of any concurrency primitive
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO] // given instance of Concurrent[IO]
  val aDeferred = Deferred[IO, Int] // given/implicit Concurrent[IO] in scope
  val aDeferred_v2 = concurrentIO.deferred[Int]
  val aRef = concurrentIO.ref(42)

  // capabilities: pure, map/flatMap, raiseError, uncancelable, start (fibers), + ref/deferred
  // ----- concrete timer for IO -----
  def timer(): IO[Unit] = {
    def timerNotification(signal: Deferred[IO, Unit]) = for {
      _ <- IO("timer running on some other fiber, waiting...").debug
      _ <- signal.get
      _ <- IO("time is up").debug
    } yield ()

    def tickingClock(counter: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      count <- counter.updateAndGet(_ + 1)
      _ <- IO(count).debug
      _ <- if (count >= 10) signal.complete(())
      else tickingClock(counter, signal)
    } yield ()

    for {
      counter <- Ref[IO].of(0)
      signal <- Deferred[IO, Unit]
      notificationFib <- timerNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }

  // general timer for any effect type

  import cats.effect.syntax.spawn._
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // start extension/implicit method

  def polymorphicTimer[F[_]](implicit concurrent: Concurrent[F]): F[Unit] = {
    def timerNotification(signal: Deferred[F, Unit]) = for {
      _ <- concurrent.pure("timer running on some other fiber, waiting...").debug
      _ <- signal.get
      _ <- concurrent.pure("time is up").debug
    } yield ()

    def tickingClock(counter: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- unsafeSleep(1.second)
      count <- counter.updateAndGet(_ + 1)
      _ <- concurrent.pure(count).debug
      _ <- if (count >= 10) signal.complete(())
      else tickingClock(counter, signal)
    } yield ()

    for {
      counter <- concurrent.ref(0)
      signal <- concurrent.deferred[Unit]
      notificationFib <- timerNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }

  /**
   * Exercises:
   * - Generalize racePair - replace IO with a general effect type
   * - Generalize the Mutex concurrency primitive for any F -- see mutex code file
   */
  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]), // (winner result, loser fiber)
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B]) // (loser fiber, winner result)
  ]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]] // start extension method

  import cats.effect.syntax.monadCancel._ // start extension method

  def ourRacePair[F[_], A, B](fa: F[A], fb: F[B])(implicit concurrent: Concurrent[F]): F[RaceResult[F, A, B]] =
    concurrent.uncancelable { poll =>
      for {
        signal <- concurrent.deferred[EitherOutcome[F, A, B]]
        fiba <- fa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
        fibb <- fb.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
        result <- poll(signal.get).onCancel { // blocking call - should be cancelable
          for {
            cancelFibA <- fiba.cancel.start
            cancelFibB <- fibb.cancel.start
            _ <- cancelFibA.join
            _ <- cancelFibB.join
          } yield ()
        }
      } yield result match {
        case Left(outcomeA) => Left((outcomeA, fibb))
        case Right(outcomeB) => Right((fiba, outcomeB))
      }
    }

  override def run = {
    polymorphicTimer[IO] //only include the effect type at 'the end of the world' i.e when we want to use it
  }

}
