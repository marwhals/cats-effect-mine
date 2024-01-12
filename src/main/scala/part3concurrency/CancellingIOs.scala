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
      pw       <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later.").debug.void) // this is cancelable i.e poll is unmasking the effect/IO inputPassword
      verified <- verifyPassword(pw) // this is NOT cancelable
      _        <- if (verified) IO("Authentication successful.").debug // this is NOT cancelable
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

  override def run = {
//    cancellationOfDoom
//    noCancellationOfDoom
    authProgram
  }

}
