package part3concurrency

import cats.effect.{IO, IOApp, Resource}
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {

  // use case: manage a connection lifecycle

  class Connection(url: String) {
    def open(): IO[String] = IO(s"""opening connection to $url""")

    def close(): IO[String] = IO(s"""closing connection to $url""")
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
    .bracket { conn => conn.open() *> IO.sleep(Int.MaxValue.seconds) } //Use the resource
    { conn => conn.close().void } // Will automatically release the resource

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
    if (scanner.hasNextLine) IO(scanner.nextLine()) >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"""opening file at $path""") >>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner => // What to do in the unhappy path
        IO(s"closing file at $path") >> IO(scanner.close())
      }

  /**
   * Resources
   *
   */
  /*
  - Nesting resources can become tedious. See below
   */
  def connFromConfig(path: String): IO[Unit] =
  openFileScanner(path)
    .bracket {
      scanner =>
        IO(new Connection(scanner.nextLine()))
          .bracket { conn =>
            conn.open() >> IO.never
          } { conn =>
            conn.close().void
          }
    } { scanner =>
      IO("closing file") >> IO(scanner.close())
    }


  val connectionResource = Resource.make(IO(new Connection("connection.com")))(conn => conn.close().void) // Compared to bracket we only have resource acquisition and release. Not usage
  // can use this resource later in the code

  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string")
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").void

  // see below for the gist of both
  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)

  /**
   * Exercise - read txt file with one line every 100 ms using resource
   * - refactor of the bracket version
   *
   */
  def getResourceFromFile(path: String) = Resource.make(openFileScanner(path)) { scanner =>
    IO(s"closing file at $path") >> IO(scanner.close())
  }

  def resourceReadFile(path: String) =
    IO(s"opening file at $path") >>
      getResourceFromFile(path).use { scanner =>
        readLineByLine(scanner)
      }

  def cancelReadFile(path: String) = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  // Resource decouple the logic of using a resource from acquiring and releasing the resource
  // nested resources - see below for for comprehension version
  def connFromConfResource(path: String) =
    Resource.make {
      IO("opening file") >> openFileScanner(path)
    } { scanner =>
      IO("closing file") >> IO(scanner.close())
    }.flatMap { scanner =>
      Resource.make {
        IO(new Connection(scanner.nextLine()))
      } {
        conn => conn.close().void
      }
    }

  val openConnection = connFromConfResource("src/main/resources/connection.txt").use(conn => conn.open() >> IO.never)
  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(1.second) >> IO("cancelling!") >> fib.cancel
  } yield ()
  // connection + file will close automatically
  // equivalent connection logic using for comprehensions
  def connFromConfResourceClean(path: String) = for {
    scanner <- Resource.make(IO("opening file") >> openFileScanner(path))(scanner => IO("closing file") >> IO(scanner.close()))
    conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void)
  } yield conn

  // finalizer - releasing of resources - these can be attatched to regular IOsval ioWithFinalizer = IO("some resource").guarantee(IO("freeing resource").void)
    val ioWithFinalizer_v2 = IO("some resource").guaranteeCase {
      case Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result")).void
      case Errored(e) => IO("nothing to release").void
      case Canceled() => IO("resource got canceled, releasing what's left").void
    }

  //Take away ----- reosurces can be composed using map/ flatMap and for-comprehensions



  override def run = {
    //    asyncFetchUrl.void
    //    correctAsyncFetchUrl.void
    //    bracketProgram.void
    //    bracketReadFile("src/main/scala/part3concurrency/AboutFibers.md")
    //    resourceFetchUrl.void
//    resourceReadFile("src/main/scala/part3concurrency/AboutFibers.md").void
//    openConnection
    canceledConnection
  }


}
