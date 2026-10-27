package part5polymorphic

import cats.effect.{Concurrent, IO, IOApp, Temporal}
import utils.general.customDebug

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object PolymorphicTemporalSuspension extends IOApp.Simple {

  // Temporal describes time blocking effects
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit] // semantically blocks this fiber for a specified time - this is the fundamental operation of the typeclass
  }

  // abilities: pure, map/flatMap, raiseError, uncancelable, start, ref/deferred, +sleep --- inherited from other Type classes
  val temporalIO = Temporal[IO] // given Temporal[IO] in scope
  val chainOfEffects = IO("Loading...") *> IO.sleep(1.second) *> IO("Game ready!")
  val chainOfEffects_v2 = temporalIO.pure("Loading...") *> temporalIO.sleep(1.second) *> temporalIO.pure("Game ready!") // same

  /**
   * Exercise: generalize the timeout functionality
   */

  import cats.syntax.flatMap._

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(implicit temporal: Temporal[F]): F[A] = {
    val timeoutEffect = temporal.sleep(duration)
    val result = temporal.race(fa, timeoutEffect)

    result.flatMap {
      case Left(v) => temporal.pure(v)
      case Right(_) => temporal.raiseError(new RuntimeException("Computation timed out."))
    }
  }

  override def run = ???

}
