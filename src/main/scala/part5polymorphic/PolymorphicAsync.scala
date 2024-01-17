package part5polymorphic

import cats.effect.{Async, Concurrent, IO, IOApp, Sync, Temporal}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import utils._


object PolymorphicAsync extends IOApp.Simple {

  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    // fundamental description of async computations - can be seen as a FFI
    // ****** generalises computations that take place outside of CE context
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]

    //----------- free methods
    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(kb => map(pure(cb(kb)))(_ => None))
    def never[A]: F[A] = async_(_ => ())
  }

  val asyncIO = Async[IO] // given/implicit Async[IO]

  // pure, map/flatMap, raiseError, uncancelable, start, ref/deferred, sleep, delay/defer/blocking, + ---- TC inheritance
  val ec = asyncIO.executionContext // exposes the execution context

  // power: async_ + async: FFI
  val threadPool = Executors.newFixedThreadPool(10)
  type Callback[A] = Either[Throwable, A] => Unit
  val asyncMeaningOfLife: IO[Int] = IO.async_ { (cb: Callback[Int]) =>
    // start computation on some other thread pool
    threadPool.execute { () =>
      // this code is not managed by CE - becomes managed by cats effect once it is wrapped inside an IO
      println(s"[${Thread.currentThread().getName}] Computing an async MOL")
      cb(Right(42))
    }
  }

  val asyncMeaningOfLifeComplex: IO[Int] = IO.async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] Computing an async MOL")
        cb(Right(42))
      }
    }.as(Some(IO("Cancelled!").debug.void)) // <-- finalizer in case the computation gets cancelled
  }

  val asyncMeaningOfLife_v2: IO[Int] = asyncIO.async_ { (cb: Callback[Int]) =>
    // start computation on some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] Computing an async MOL")
      cb(Right(42))
    }
  } // same

  val asyncMeaningOfLifeComplex_v2: IO[Int] = asyncIO.async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] Computing an async MOL")
        cb(Right(42))
      }
    }.as(Some(IO("Cancelled!").debug.void)) // <-- finalizer in case the computation gets cancelled
  } // same

  val myExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  val asyncMeaningOfLife_v3 = asyncIO.evalOn(IO(42).debug, myExecutionContext).guarantee(IO(threadPool.shutdown()))

  // never
  val neverIO = asyncIO.never

  /**
   * Exercises
   * - implement never and async_ in terms of the big async.
   * - tuple two effects with different requirements.
   */
  def firstEffect[F[_] : Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)
  def secondEffect[F[_] : Sync, A](a: A): F[A] = Sync[F].pure(a)

  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def tupledEffect[F[_] : Async, A](a: A): F[(A, A)] = for {
    first <- firstEffect(a)
    second <- secondEffect(a)
  } yield (first, second)

  override def run = ???

}
