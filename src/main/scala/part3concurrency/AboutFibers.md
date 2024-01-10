Fibers are a description of an effect being executed on some other thread
```scala
def createFiber: Fiber[IO, Throwable, String] = ???
```
---- they have three parts 1) effect type 2) failure type 3) result type

Creating a fiber is an effectful operation
-- a thus a fiber will be wrapped in an IO
```scala
val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.start
```
Managing a fiber is an effectful operation
- the result of the operation is wrapped in another IO
 ```scala
def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
  fib <- io.start
  result <- fib.join
} yield result
```

How fibers work
--- Cats effect has a thread pool that manages the execution of effects
------ Thread - active - can run code
------ fiber - passive - just a data structure - numbers of fibers is measure per GB of heap

Motivation for Fibers

Why we need fibers
- No more need for threads and locks
- Delgate thread management to Cats Effect
- Avoid asynchronous code with callbacks
- Maintain pure functional programming
- Keep low-level primitives (e.g. blocking, waiting, joining, interrupting, cancelling)

Fiber scheduling concepts and implementation details
- blocking effects in a fiber lead to descheduling
- semantic blocking
- cooperative scheduling
- the same fiber can run on multiple JVM threads
- work stealing thread pool


