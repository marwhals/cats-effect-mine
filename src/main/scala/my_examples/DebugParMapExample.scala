package my_examples

import cats.effect.{IO, IOApp}
import cats.syntax.all.*

object DebugParMapExample extends IOApp.Simple {
  val iOOne = IO.delay("Response one")
  val iOTwo = IO.delay("Response two")
  val iOThree = IO.delay("Response three")

  val program = iOOne.debug("I am program one")
  val program2 = iOTwo.debug("I am program two")
  val program3 = iOThree.debug("I am program three")

  val combinedProgram = (program, program2, program3).parMapN { (one, two, three) =>
    s"$one + $two + $three"
  }

  def run: cats.effect.IO[Unit] = {
    combinedProgram *> IO.println(s"Finished")
  }
}