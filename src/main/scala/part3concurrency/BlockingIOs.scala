package part3concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.DurationInt
import utils._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.second).debug // SEMANTIC BLOCKING - no threads are actually blocked, CE assigns this thread to some other fiber
    _ <- IO.sleep(1.second).debug // No sleeping is actually occuring the execution of the fiber is scheduled to run in one second
  } yield ()

  // really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  } // will evaluate on a thread from ANOTHER thread pool specific for blocking calls

  // yielding
  val iosOnManyThreads = for {
    _ <- IO("first").debug
    _ <- IO.cede // a signal to yield control over the thread
    _ <- IO("second").debug // the rest of this effect may run on another thread (not necessarily)
    _ <- IO.cede
    _ <- IO("third").debug
  } yield ()

  def testThousandEffectsSwitch() = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec) // the ec is not controlled by the CE runtime remove evalOn to use CE runtime
  }

  /*
  Takeaway - blocking calls & IO.sleep implement semantic blocking and yield control over the calling thread
   */

  override def run = {
//    someSleeps
//    aBlockingIO.void
//    iosOnManyThreads
    testThousandEffectsSwitch().void
  }

}
