package part3concurrency

import cats.effect.{IO, IOApp}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {

  // use case: manage a connection lifecycle

  import utils._

  class Connection(url: String) {
    def open(): IO[String] = IO(s"""opening to connection to $url""").debug

    def close(): IO[String] = IO(s"""closing connection to $url""").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("theUrl.com")).open() *> IO.sleep(Int.MaxValue.seconds).start // spawn a fiber
    _ <- IO.sleep(1.second) *> fib.cancel // This will lead to the 'leaking' of resources
  } yield ()

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("theUrl.com"))
    fib <- (conn.open() *> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close().void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  // CE invented the bracket pattern to avoid the clunkiness
  /*
    bracket pattern: someIO.bracket(useResourceCB)(releaseResourceCB)
    bracket is equivalent to try-catches but is purely functional
   */

  //See below for the take away code
  val bracketFetchUrl = IO(new Connection("theUrl.com"))
    .bracket
    {conn => conn.open() *> IO.sleep(Int.MaxValue.seconds)} //Use the resource
    {conn => conn.close().void} // Will automatically release the resource

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /*
    Excercise read a file with the bracket pattern
     *  - open a scanner
     *  - read the file line by line, every 100 millis
     *  - close the scanner
     *  - if cancelled/throws error, close the scanner
   */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"""opening file at $path""") >>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner => // What to do in the unhappy path
        IO(s"closing file at $path").debug >> IO(scanner.close())
      }

  override def run = {
    //    asyncFetchUrl.void
    //    correctAsyncFetchUrl.void
//    bracketProgram.void
    bracketReadFile("src/main/scala/part3concurrency/AboutFibers.md")
  }

}
