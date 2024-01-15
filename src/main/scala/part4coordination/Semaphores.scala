package part4coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import cats.syntax.parallel._
import utils._

import scala.concurrent.duration.DurationInt
import scala.util.Random

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  // example: limiting the number of concurrent sessions on a server
  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquire
    // critical section
    _ <- IO(s"[session $id] logged in, working...").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    // end of critical section
    _ <- sem.release
  } yield res

  def demoSemaphore() = for {
    sem <- Semaphore[IO](2) // 2 permits , 3 fibers
    user1Fib <- login(1, sem).start
    user2Fib <- login(2, sem).start
    user3Fib <- login(3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquireN(requiredPermits)
    // critical section
    _ <- IO(s"[session $id] logged in, working...").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    // end of critical section
    _ <- sem.releaseN(requiredPermits)
  } yield res

  def demoWeightedSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- weightedLogin(1, 1, sem).start
    user2Fib <- weightedLogin(2, 2, sem).start
    user3Fib <- weightedLogin(3, 3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  /*
  - Exercise
  - Semaphore with 1 permit == a mutex
  - 1. find out if there's something wrong with this code
  - 2. why is it wrong?
  - 3. fix it
   */
  val mutex = Semaphore[IO](1)
  val users: IO[List[Int]] = (1 to 10).toList.parTraverse { id =>
    for {
      sem <- mutex // == Semaphore[IO](1)  Ans 1) recreating the semaphore every time 1 to 10
      _ <- IO(s"[session $id] waiting to log in...").debug
      _ <- sem.acquire
      // critical section
      _ <- IO(s"[session $id] logged in, working...").debug
      res <- doWorkWhileLoggedIn()
      _ <- IO(s"[session $id] done: $res, logging out...").debug
      // end of critical section
      _ <- sem.release
    } yield res
  }

  //3 move mutex to outside of the traverse
  val usersFixed: IO[List[Int]] = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session $id] waiting to log in...").debug
        _ <- sem.acquire
        // critical section
        _ <- IO(s"[session $id] logged in, working...").debug
        res <- doWorkWhileLoggedIn()
        _ <- IO(s"[session $id] done: $res, logging out...").debug
        // end of critical section
        _ <- sem.release
      } yield res
    }
  }

  override def run = {
//    demoSemaphore()
//    users.debug.void
    usersFixed.debug.void
  }

}
