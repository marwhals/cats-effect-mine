package part4coordination

import cats.effect.std.CountDownLatch
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.parallel._
import utils._

import java.io.{File, FileWriter}
import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.Random
import cats.syntax.traverse._

object CountdownLatches extends IOApp.Simple {

  /*
    CDLatches are a coordination primitive initialized with a count.
    All fibers calling await() on the CDLatch are (semantically) blocked.
    When the internal count of the latch reaches 0 (via release() calls from other fibers), all waiting fibers are unblocked.
   */

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO("5...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("4...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("3...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("2...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("1...").debug >> IO.sleep(1.second)
    _ <- latch.release // gun firing --- fibers all start running
    _ <- IO("GO GO GO!").debug
  } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[runner $id] waiting for signal...").debug
    _ <- latch.await // block this fiber until the count reaches 0
    _ <- IO(s"[runner $id] RUNNING!").debug
  } yield ()

  def sprint(): IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    announcerFib <- announcer(latch).start // start the latch
    _ <- (1 to 10).toList.parTraverse(id => createRunner(id, latch)) // create the fibers
    _ <- announcerFib.join
  } yield ()

  /**
   * Exercise: simulate a file downloader on multiple threads
   */
  object FileServer {
    val fileChunksList = Array(
      "File chunk 1 ",
      "File chunk 2 ",
      "File chunk 3 - Never would I have thought I would do low-level concurrency WITH pure FP."
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)

    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] = {
    val fileResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fileResource.use { writer =>
      IO(writer.write(contents))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    compositeResource.use {
      case (reader, writer) => IO(reader.getLines().foreach(writer.write))
    }
  }

  /*
    - call file server API and get the number of chunks (n)
    - start a CDLatch
    - start n fibers which download a chunk of the file (use the file server's download chunk API)
    - block on the latch until each task has finished
    - after all chunks are done, stitch the files together under the same file on disk
   */
//  def downloadFile(filename: String, destFolder: String): IO[Unit] = for {
//    n <- FileServer.getNumChunks // just retrieves an IO of int
//    latch <- CDLatch(n)
//    _ <- IO(s"Download started on $n fibers.").debug
//    _ <- (0 until n).toList.parTraverse(id => createFileDownloaderTask(id, latch, filename, destFolder))
//    _ <- latch.await
//    _ <- (0 until n).toList.traverse(id => appendFileContents(s"$destFolder/$filename.part$id", s"$destFolder/$filename"))
//  } yield ()

  def downloadFile(filename: String, destFolder: String): IO[Unit] = for {
    n <- FileServer.getNumChunks
    latch <- CountDownLatch[IO](n)
    _ <- IO(s"Download started on $n fibers.").debug
    _ <- (0 until n).toList.parTraverse(id => createFileDownloaderTask(id, latch, filename, destFolder))
    _ <- latch.await // wait until countdown is 0
    _ <- (0 until n).toList.traverse(id => appendFileContents(s"$destFolder/$filename.part$id", s"$destFolder/$filename"))
  } yield ()

  def createFileDownloaderTask(id: Int, latch: CountDownLatch[IO], filename: String, destFolder: String): IO[Unit] = for {
    _ <- IO(s"[task $id] downloading chunk...").debug
    _ <- IO.sleep((Random.nextDouble * 1000).toInt.millis)
    chunk <- FileServer.getFileChunk(id)
    _ <- writeToFile(s"$destFolder/$filename.part$id", chunk)
    _ <- IO(s"[task $id] chunk download complete").debug
    _ <- latch.release // decrease the count by one once the download has completed
  } yield ()


  override def run = {
//    sprint()
    downloadFile("myScalafile.txt", "src/main/resources")
  }

}
