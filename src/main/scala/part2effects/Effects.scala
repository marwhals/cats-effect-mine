package part2effects

import scala.concurrent.Future
import scala.io.StdIn

object Effects {


  // pure functional programming - a big expression computing a single value
  // relies on substitution

  def combine(a: Int, b: Int): Int = a + b

  val five = combine(2, 3)
  val five_v1 = 2 + 3
  val five_v2 = 5

  // referential transparency

  // example of breking this
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = () // not the same

  // another example of breaking ref transparency
  var anInt = 0
  val changingVar: Unit = anInt += 1
  val changingVar_v2: Unit = () // not the same

  // an effect ---- bridges the gap between needing side effects with writing purely functional code
  // an effect is the data type which embodies a side effect

  /*
  What we want from an effect type
  Properties / effect types
  - type signature describes the kind of calculation that will be performed
  - type signature describes the VALUE that will be calculated
  - when side effects are needed, effect construction is separate from effect execution
   */

  /*
    Example : Option technically an effect type
  - describes possibly absent values
  - computes a value fo type A, if it exists
  - side effects are no needed
   */

  val anOption: Option[Int] = Option(42)

  /*
    Example : Option technically an effect type
  - describes possibly absent values
  - computes a value fo type A, if it exists
  - side effects are no needed
   */

  /*
    example: Future is NOT an effect type
    - descibes an asynchronous computation
    - computes a value of Type A, if it is successful
    - side effects are needed (allocating or scheduling a thread) -> this is NOT separate from construction
   */

  import scala.concurrent.ExecutionContext.Implicits.global

  val aFuture: Future[Int] = Future(42)

  /* example: MyIO data type from the Monads lesson -> it is an effect type
    - described any computation that may produce side effects
    - calculates a value of type A if its successful
    - side effect are required for the evaluation of () => A
      - YES, the creation of MyIO does NOT produce the side effects on construction
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...........")
    42
  })

  /*
  - Excercises create
  - An IO which returnes the current time of the system
  - An IO which measures the duration of a computation
  - An IO which prints to the console
  - An IO which reads a line (a string) from the std input
   */

  //1
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  //2
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    startTime <- clock
    _ <- computation
    //    result <- computation // result is not actually used but it is invoked
    finishTime <- clock
  } yield finishTime - startTime

  /*  Breaking down the for comprehension

      clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

      clock.map(finishTime => finishTime - startTime) = MyIO(() => System.currentTimeMillis() - startTime)
      => clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime)))

      computation.flatMap(lambda) = MyIO(() => lambda(___COMP___).unsafeRun())
                                  = MyIO(() => MyIO(() => System.currentTimeMillis() - startTime)).unsafeRun())
                                  = MyIO(() => System.currentTimeMillis_after_computation() - startTime)

      clock.flatMap(startTime => MyIO(() => System.currentTimeMillis_after_computation() - startTime))
      = MyIO(() => MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis()).unsafeRun())
      = MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis_at_start())

    * think in terms of for comprhensions*

*/


  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => {
      println("Sleeping started........")
      Thread.sleep(1000)
      println("Sleeping done........")
    }))
    println(test.unsafeRun())
  }


  //3
  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  //4
  val read: MyIO[String] = MyIO(() => StdIn.readLine())

  def testConsole(): Unit = {
    val program: MyIO[Unit] = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + line2)
    } yield () //common pattern

    program.unsafeRun()
  }

  def main(args: Array[String]): Unit = {
    anIO.unsafeRun()
    testTimeIO()
    testConsole()
  }

}
