package part4coordination

import cats.effect.IOApp

import cats.effect.kernel.{Deferred, Ref}
import cats.effect.{IO, IOApp}
import cats.effect.std.CyclicBarrier

import scala.util.Random
import scala.concurrent.duration._
import utils._
import cats.syntax.parallel._

object CyclicBarriers extends IOApp.Simple {

  /*
    A cyclic barrier is a coordination primitive that
    - is initialized with a count
    - has a single API: await

    A cyclic barrier will (semantically) block all fibers calling its await() method until we have exactly N fibers waiting,
      at which point the barrier will unblock all fibers and reset to its original state.
    Any further fiber will again block until we have exactly N fibers waiting.
    ...
    And so on.
   */

  // example: signing up for a social network just about to be launched
  def createUser(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] = for {
    _ <- IO.sleep((Random.nextDouble * 500).toInt.millis)
    _ <- IO(s"[user $id] Signing up for the waitlist for the new social network...").debug
    _ <- IO.sleep((Random.nextDouble * 1500).toInt.millis)
    _ <- IO(s"[user $id] On the waitlist now....").debug
    _ <- barrier.await // block the fiber when there are exactly N users waiting
    _ <- IO(s"[user $id] The social network is now available").debug
  } yield ()

  def openNetwork(): IO[Unit] = for {
    _ <- IO("[announcer] Social network is up for registration. Waiting for 10 users").debug
    barrier <- CyclicBarrier[IO](10)
    _ <- (1 to 7).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  override def run = {
    openNetwork()
  }

}

/*
 - Exercise - Implement a Cyclic barrier using Ref + Deferred. Ignore cancellation effects.
*/

abstract class CBarrier {
  def await: IO[Unit]
}

object CBarrier {
  case class State(nWaiting: Int, signal: Deferred[IO, Unit])

  def apply(count: Int): IO[CBarrier] = for {
    signal <- Deferred[IO, Unit]
    state <- Ref[IO].of(State(count, signal))
  } yield new CBarrier {
    override def await = Deferred[IO, Unit].flatMap { newSignal =>
      state.modify {
        case State(1, signal) => State(count, newSignal) -> signal.complete(()).void
        case State(n, signal) => State(n - 1, signal) -> signal.get
      }.flatten
    }
  }
}