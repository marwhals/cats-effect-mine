package part3concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.DurationInt

object CancellingIOs extends IOApp.Simple {

  import utils._

  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation
   */
  val chainOfIOs: IO[Int] = IO("waiting").debug >> IO.canceled >> IO(42).debug // Whatever comes after IO.cancelled will no be evaluated

  //uncancellable - a wrapper over an IO which prevents it from being cancelled by another fiber
  // example - payment process must not be cancelled
  val specialPaymentSystem = (
    IO("Payment running, don't cancel me...").debug >>
      IO.sleep(1.second) >>
      IO("Payment completed.").debug
    ).onCancel(IO("MEGA CANCEL OF DOOM!").debug.void)


  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // wrapping the effect chain in an uncancellable is called "masking"
  // CE runtime will never cancel this effect even when requested by another fiber
  val atomicPayment_v2 = specialPaymentSystem.uncancelable // alternative

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation...").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
    The uncancelable API is more complex and more general.
    It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
    The Poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   */

  /*
  Example: Pretendy authentication service
  - input password, can be cancelled otherwise we might block indefinitely on user input
  - verify password, CANNOT be cancelled once it's started
   */

  val inputPassword = IO("Input password:").debug >> IO("(typing password)").debug >> IO.sleep(5.seconds) >> IO("password123")
  val verifyPassword = (pw: String) => IO("verifying...").debug >> IO.sleep(2.seconds) >> IO(pw == "password123")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later.").debug.void) // this is cancelable i.e poll is unmasking the effect/IO inputPassword
      verified <- verifyPassword(pw) // this is NOT cancelable
      _ <- if (verified) IO("Authentication successful.").debug // this is NOT cancelable
      else IO("Authentication failed.").debug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
    Uncancelable calls are MASKS which suppress cancellation.
    Poll calls are "gaps opened" in the uncancelable region.
   */

  /*
    Exercises/ illustration of cancellable and poll
   */
  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debug
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug)
  // uncancelable will eliminate ALL cancel points - use poll to introduce them


  // 2
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debug >> IO.sleep(1.second) >> IO("cancelable end").debug) >>
        IO("uncancelable").debug >> IO.sleep(1.second) >> IO("uncancelable end").debug >>
        poll(IO("second cancelable").debug >> IO.sleep(1.second) >> IO("second cancelable end").debug)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("CANCELING").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }
  // Takeaway - the cancel will be handled by the first cancellable region i.e wrapped in a poll.

  /* Cancellation Regions summary
  - Make effects ignore cancellation signals with uncancellable

  Define pinpoint cancellation regions by using the poll
  - everything wrapped in the poll call is cancellable
  - everything else is not cancellable

  The poll is the 'local' opposite of uncancellable
  - this can be used as many times as we like
   */

  override def run = {
    //    cancellationOfDoom
    //    noCancellationOfDoom
//    authProgram
//    invincibleAuthProgram
    threeStepProgram()
  }

}
