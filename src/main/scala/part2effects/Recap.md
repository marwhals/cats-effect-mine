Effects
-
Descriptions of computations to be performed at our discretion

A bridge beteeen
- pure functional programming and referential transparancy
- impure FP/imperative programming & side effects

Effect properties
- it describes what kind of computation wil be performed
- the type signature describes the value will be calculated
- it separates effect description from effect execution
- --- when externally visible side effects are produced

Cats Effect IO
-

The ultimate effect type
- any computation that might perform side effects
- produces aa value of type A if it's successful
- the effect construction is separate form effect execution

Expressions and methods returning IOs are called effectful

Perform the effects at the 'end of the world' using the unsafeRunSync() method

```scala
val outFirstIO: IO[Int] = IO.pure(42) <---- Should not have side effects
val aDelayedIO: IO[Int] = IO.delay {
  println("I'm producing an integer") <---- Computation is not performed
  54
}

val aDelayedIO_v2: IO[Int] = IO { ... } // apply == delay

//the end of the world
import cats.effect.unsafe.implicits.global
myBigEffect.unsafeRunSync()
```

IO transformations: map, flatMap
```scala
import scala.io.StdIn
val improvedMeaningOfLife = outFirstIO.map(_ * 2)
val printedMeaningOfLife = outFirstIO.flatMap(mol => IO.delay(println(mol)))

----- Imperative program example -----
def smallProgram(): IO[Unit] = for {
  line1 <- IO(StdIn.readLine())  
  line2 <- IO(StdIn.readLine())
  _ <- IO.delay(println(line1 + line2))
} yield ()
```
IO compositions real like an imperative program
- pure FP is preserved

IO is a monad

Other transformations
- ">>, *>, <*"
- as, void

Can create failed effects
```scala
val aFailedComputation: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))
```
Can handle errors/exceptions
```scala
val dealWithIt = aFailure.handleErrorWith {
  case _: RuntimeException => IO.delay(println("I'm still here"))
}
```
Can transform IO to also hold failure to process later
```scala
val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
```

IO Parallelism
- effects are evaluated on different threads
- synchronisation and coordination are automatic
```scala
import cats.syntax.parallel.catsSyntaxParallelSequence._
val goalInLifeParallel: IO[String] = (meaningOfLife.debug, favLang.debug).parMapN(_ + _)
```

IO Traversal
- useful when we want to wrap double nested containers
- can be done in parallel
```scala
val workload: List[String]
val computeAsIO: String => IO[Int] = ...

import cats.instances.list._ // Travers TC instance for list
import cats.syntax.parallel._ // parTraverse extension method

val parallelSingleIO: IO[List[Int]] = workLoad.parTravers(computeAsIO)

```
