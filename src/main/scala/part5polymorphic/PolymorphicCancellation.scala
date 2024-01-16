package part5polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}

object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]

    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  // MonadCancel describes the capability to cancel & prevent cancellation -- generalising for any Monad not just IOs
  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] { // MonadCancel is special occurence of MonadError
    def canceled: F[Unit]

    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // we can create values, because MonadCancel is a Monad
  val molIO: IO[Int] = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  // Can create computations with cancellable and uncancellable regions
  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure("once started, I can't go back...")
      res <- monadCancelIO.pure(56)
    } yield res
  }

  // goal: can generalize code - don't have to use just IO
  def mustComputeGeneral[F[_], E](implicit mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ =>
    for {
      _ <- mc.pure("once started, I can't go back...")
      res <- mc.pure(56)
    } yield res
  }

  val mustCompute_v2 = mustComputeGeneral[IO, Throwable]

  // allow cancellation listeners
  val mustComputeWithListener = mustCompute.onCancel(IO("I'm being cancelled!").void)
  val mustComputeWithListener_v2 = monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled!").void) // same
  // .onCancel as extension method

  // allow finalizers: guarantee, guaranteeCase
  val aComputationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"successful: $a").void)
    case Errored(e) => IO(s"failed: $e").void
    case Canceled() => IO("canceled").void
  }

  // bracket pattern is specific to MonadCancel
  val aComputationWithUsage = monadCancelIO.bracket(IO(42)) { value =>
    IO(s"Using the meaning of life: $value")
  } { value =>
    IO("releasing the meaning of life...").void
  }
  // therefore Resources can only be built in the presence of a MonadCancel instance

  override def run = IO().void

}
