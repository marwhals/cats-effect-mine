package part5polymorphic

import cats.Defer
import cats.effect.{IO, IOApp, MonadCancel, Sync}

import java.io.{BufferedReader, InputStreamReader}

/*
- Sync - the ability to suspend effects synchronously
-- delay = wrapping any computation in F
-- blocking = a semantically blocking computation, wrapped in F

Goal - generalise synchornous code for any effect type
----- FFI - suspending computations (including side effects) executed in a context outside of the CE runtime context

 */

object PolymorphicSync extends IOApp.Simple {

  val aDelayedIO = IO.delay { // "suspend" computations in IO
    println("I'm an effect!")
    42
  }

  val aBlockingIO = IO.blocking { // on some specific thread pool for blocking computations
    println("loading...")
    Thread.sleep(1000)
    42
  }

  // synchronous computation - bringing computations outside of CE context into the CE context
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](thunk: => A): F[A] // "suspension" of a computation - will run on the CE thread pool

    def blocking[A](thunk: => A): F[A] // runs on the blocking thread pool

    // defer comes for free ---- this is because of delay and Monads flatmap
    def defer[A](thunk: => F[A]): F[A] =
      flatMap(delay(thunk))(identity)
  }

  val syncIO = Sync[IO] // given/implicit Sync[IO] in scope

  // abilities: pure, map/flatMap, raiseError, uncancelable, + delay/blocking
  val aDelayedIO_v2 = syncIO.delay {
    println("I'm an effect!")
    42
  } // same as IO.delay

  val aBlockingIO_v2 = syncIO.blocking {
    println("loading...")
    Thread.sleep(1000)
    42
  } // same as IO.blocking

  val aDeferredIO = IO.defer(aDelayedIO)

  /**
   * Excercise - Write a polymorphic console
   *
   */
  trait Console[F[_]] {
    def println[A](a: A): F[Unit]

    def readLine(): F[String]
  }

  import cats.syntax.functor._

  object Console {
    def make[F[_]](implicit sync: Sync[F]): F[Console[F]] = sync.pure((System.in, System.out)).map {
      case (in, out) => new Console[F] {
        def println[A](a: A): F[Unit] =
          sync.blocking(out.println(a))

        def readLine(): F[String] = {
          val bufferedReader = new BufferedReader(new InputStreamReader(in)) //Java APi/code
          sync.blocking(bufferedReader.readLine())
        }
      }
    }
  }

  def consoleReaderDemo(): IO[Unit] = for {
    console <- Console.make[IO]
    _ <- console.println("Hi, what's your name?")
    name <- console.readLine()
    _ <- console.println(s"Hi $name, nice to meet you!")
  } yield ()

  override def run = consoleReaderDemo()

}
