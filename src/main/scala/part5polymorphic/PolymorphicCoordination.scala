package part5polymorphic

import cats.effect.kernel.{Deferred, Spawn}
import cats.effect._
import utils.general._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

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
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.effect.syntax.spawn._ // start extension/implicit method

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

  override def run = {
    polymorphicTimer[IO] //only include the effect type at 'the end of the world' i.e when we want to use it
  }

}
