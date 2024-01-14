package part4coordination

import cats.effect.kernel.Outcome
import cats.effect.{Deferred, Fiber, IO, IOApp, Ref}
import cats.syntax.traverse._
import utils._

import scala.concurrent.duration.DurationInt

/**
 * Deferred - recap/summary
 * A purely functional concurrency primitive with two methods
 * - get: blocks the fiber (semantically) until a value is present
 * - complete: inserts a value that can be read by the blocked fibers
 *
 * Why?
 * - allows inter-fiber communication
 * - avoids busy waiting
 * - maintains thread safety
 *
 * Use-cases
 * - producer-consumer like problems
 * - sending data between fibers
 * - notification mechanisms
 */

object Defers extends IOApp.Simple {


  // deferred is a primitive for waiting for an effect, while some other effect completes with a value
  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  // get blocks the calling fiber (semantically) until some other fiber completes the Deferred with a value
  val reader: IO[Int] = aDeferred.flatMap { signal => // Deferred instance is called signal
    signal.get // blocks the fiber
  }

  val writer = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result...").debug
      meaningOfLife <- signal.get // blocker
      _ <- IO(s"[consumer] got the result: $meaningOfLife").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[producer] crunching numbers...").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").debug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("File part 1  ", "file part 2", "file part 3", " file part 4", "the last part of the file<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts
        .map { part =>
          IO(s"[downloader] got '$part'").debug >> IO.sleep(1.second) >> contentRef.update(currentContent => currentContent + part) // update the file with the latest download chunk
        }
        .sequence // Turn the nested monad inside out
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get // get the current state of the file - check if it contains the last part <EOF>
      _ <- if (file.endsWith("<EOF>")) IO("[notifier] File download complete").debug
      else IO("[notifier] downloading...").debug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef) // busy wait! this is a problem but it is thread safe
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start // Run the two functions on different fibers
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()
  }


  // deferred is a solution to avoiding this busy waiting
  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").debug
      _ <- signal.get // blocks until the signal is completed
      _ <- IO("[notifier] File download complete").debug
    } yield ()

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] got '$part'").debug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentContent => currentContent + part) // adding the file part to the to the download file
      _ <- if (latestContent.contains("<EOF>")) signal.complete(latestContent) else IO.unit // look for the end else ignore and carry on
    } yield ()

    for {
      contentRef <- Ref[IO].of("") // mutable data structure for my the data
      signal <- Deferred[IO, String] // intialise the signal
      notifierFib <- notifyFileComplete(signal).start
      fileTasksFib <- fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileTasksFib.join
    } yield ()
  }

  /**
   * Exercises:
   *  - (medium) write a small alarm notification with two simultaneous IOs
   *    - one that increments a counter every second (a clock)
   *    - one that waits for the counter to become 10, then prints a message "time's up!"
   *
   *  - (mega hard) implement racePair with Deferred.
   *    - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
   *    - start two fibers, one for each IO
   *    - on completion (with any status), each IO needs to complete that Deferred
   *      (hint: use a finalizer from the Resources lesson)
   *      (hint2: use a guarantee call to make sure the fibers complete the Deferred)
   *    - what do you do in case of cancellation (the hardest part)?
   */

  // 1
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


  //2
  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, loser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B]) // (loser fiber, winner result)
  ]

  type EitherOutcome[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  /*
  -- Important to consider which parts of the IO chain should be cancellable and which parts shouldn't
   */
  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]]  // winner wil complete this signal
      fiba <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibb <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel { // blocking call - should be cancelable
        for {
          cancelFibA <- fiba.cancel.start // Important that both these fibers are cancelled at the same time
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
    //    demoDeferred()
    //    fileNotifierWithRef()
    //    fileNotifierWithDeferred()
    timer()
  }

}
