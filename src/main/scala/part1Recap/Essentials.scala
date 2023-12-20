package part1Recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util._

object Essentials {

  //values
  val aBoolean: Boolean = false


  // expressions are EVALUATED to a value
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instruction vs expressions
  val theUnit = println("abcdef") //unit = void

  // OOP
  class Animal

  class Cat extends Animal

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  //scala.deprecatedInheritance model: extend <= 1 class, but inherit from >= 0 traits
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("yum")
  }

  // singleton
  object MySingleton // singleton pattern in one line

  // companions
  object Carnivore // companion object of the class carnivore

  // generics
  class MyList[A]

  // Method notation
  val three = 1 + 2
  val anotherThree = 1.+(2)

  //functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(123)

  // HOFs map/flatmap/filter
  val processedList = List(1, 2, 3).map(incrementer)
  val aLongerList = List(1, 2, 3).flatMap(x => List(x, x + 1))

  // options and try
  val anOption: Option[Int] = Option(/* something that might be null */ 3) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2)
  val anAttempt = Try(/* something that might throw */ 42) // Success(42)
  val anAttempt2 = Try(/* something that might throw */
    {
      throw new Exception()
      42
    }
  ) //
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10)

  // pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value) => s"the option is not empty: $value"
    case None => "the option is empty"
  }

  // Futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future {
    // a bit of code
    42
  }

  // wait for completion (async)
  aFuture.onComplete {
    case Success(value) => println(s"The async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of value failed: $exception")
  }

  // map a Future
  val anotherFuture = aFuture.map(_ + 1) // Future(43) when it completes

  // for-comprehensions
  val checkerboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))
  val anotherCheckerboard = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c) // equivalent expression

  // partial functions
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  //Higher kinded type stuff
  trait HigherKindedType[F[_]]

  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential = true
  }

  def main(args: Array[String]): Unit = {

  }

}
