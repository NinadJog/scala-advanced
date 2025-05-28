package part2afp

import scala.annotation.targetName

object Monads {

  def listStory(): Unit = {
    val aList = List(1, 2, 3)
    val listMultiply =
      for
        x <- List(1, 2, 3)
        y <- List(4, 5, 6)
      yield x * y

    val listMultiply_v2 = // with flatMap and map
      List(1, 2, 3) flatMap (x =>
      List(4, 5, 6) map     (y => x * y))

    // Contrived example with functions while building up to monads
    val f = (x: Int) => List(x, x + 1)  // f and g can be used in a flatMap
    val g = (x: Int) => List(x, 2 * x)
    val pure = (x: Int) => List(x) // Same as the List "constructor"; it's also the pure from Applicative

    // Given f, g, and pure, all lists will have to satisfy 3 properties.

    // Property 1. Left identity
    val leftIdentity = (pure(42) flatMap f) == f(42) // List(42).flatMap(f) == f(42) for every x, every f

    // Property 2. Right identity
    val rightIdentity = (aList flatMap pure) == aList // for every list

    // Property 3. Associativity
    /* Let's calculate aList.flatMap(f).flatMap(g):
      [1, 2, 3]       flatMap [x, x+1]    == [1,2, 2,3, 3,4]    // aList.flatMap(f)
      [1,2, 2,3, 3,4] flatMap [x, 2 * x]  == [1,2,2,4,  2,4,3,6,  3,6,4,8]
      The 3 chunks are as follows.
      [1,2,2,4] == f(1) flatMap g
      [2,4,3,6] == f(2) flatMap g
      [3,6,4,8] == f(3) flatMap g
      Concatenating the 3 pieces together, we get
      [1,2,2,4,  2,4,3,6,  3,6,4,8] == f(1).flatMap(g) ++ f(2).flatMap(g) ++ f(3).flatMap(g)
      which means we are applying the lambda (x => f(x).flatMap(g)) to every element of aList.
      That's the same as [1,2,3].flatMap(x => f(x).flatMap(g))
     */
    val associativity = aList.flatMap(f).flatMap(g) == aList.flatMap(x => f(x).flatMap(g))

    // A list obeys all these 3 properties.
  }

  //---------------------------------------------------------------------------
  def optionStory(): Unit = {

    val anOption = Option(42)
    val optionString: Option[String] =
      for
        lang <- Option("Scala")
        ver <- Option(3)
      yield s"$lang-$ver"

    // Same using flatMap and map
    val optionString_v2 =
      Option("Scala") flatMap (lang =>
      Option(3)       map     (ver =>
        s"$lang-$ver"))

    val f = (x: Int) => Option(x + 1)
    val g = (x: Int) => Option(2 * x)
    val pure = (x: Int) => Option(x)  // Same as the Option "constructor"

    // Property 1. Left identity
    val leftIdentity = (pure(42) flatMap f) == f(42) // Option(42).flatMap(f) == f(42) for every x, every f

    // Property 2. Right identity
    val rightIdentity = (anOption flatMap pure) == anOption // for every Option

    // Property 3. Associativity
    /*
      LHS = (anOption flatMap f) flatMap g
      == (Option(42) flatMap (x => Option(x+1))) flatMap (y => Option(2 * y))
      == Option(43) flatMap (y => Option(2 * y))
      == Option(86)

      RHS = anOption flatMap (f(_) flatMap g)
      == Option(42) flatMap (x => Option(x + 1) flatMap (y => 2 * y))
      == Option(42) flatMap (x => Option(2 * x + 2))
      == Option(86) == LHS
     */
    val associativity = ((anOption flatMap f) flatMap g) == (anOption flatMap (f(_) flatMap g))
  }

  // Monads are data structures that obey these 3 properties of flatMaps.
  // Monads can chain dependent computations.

  //---------------------------------------------------------------------------
  // Exercises

  // unsafeRun is a "zero lambda", one that doesn't take any parameters
  // In the map method, return a new PossiblyMonad of type B constructed with a
  // zero lambda whose implementation is going to be f applied to the VALUE
  // obtained after invoking the unsafeRun.
  // Is this a monad? Yes, it is.
  // Interpretation: it separates the description of a computation from its execution
  // Since PossiblyMonad is a case class, the unsafeRun parameter gets
  // automatically promoted to a field. But unsafeRun is a computation.
  case class IO[A](unsafeRun: () => A) {

    def map[B](f: A => B): IO[B] =
      IO(() => f(unsafeRun())) // apply f to the value obtained after invoking unsafe run

    // f(unsafeRun()) :: PossiblyMonad[B] (where :: stands for 'has type', same as in Haskell)
    // f(unsafeRun()).unsafeRun() :: B
    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(unsafeRun()).unsafeRun())
  }

  // Add a companion object so we are able to construct a PossiblyMonad instance
  // on an actual value rather than a lambda
  object IO {

    // Pass the value A by name, so that we don't break the contract of PossiblyMonad.
    // We don't need to invoke unsafeRun unless necessary. The execution of
    // value is deferred
    @targetName("pure") // see detailed comment below on why it's needed
    def apply[A](value: => A): IO[A] =
      new IO(() => value) // The zero-lambda evaluates the value later

    // 1. The targetName annotation gives another name to the apply method in the
    // JVM bytecode. We need it because PossiblyMonad already has a companion
    // object with an apply method since it's a case class. The targetName
    // annotation prevents a name collision in the compiled code.

    // 2. We use 'new PossiblyMonad(...)' in the body of the apply method because
    // in the absence of new, the compiler and/or IntelliJ marks the apply
    // method as tail recursive, as it thinks the call PossiblyMonad(() => value)
    // is a recursive call. Using a 'new' removes the confusion.
  }

  //---------------------------------------------------------------------------
  // Exercise: Given this data structure and its companion object, determine
  // whether it's a monad or not.
  def possiblyMonadStory(): Unit = {

    val possiblyMonad: IO[Int] = IO(42)

    val f = (x: Int) => IO(x + 1)
    val g = (x: Int) => IO(2 * x)
    val pure = (x: Int) => IO(x) // Same as the PossiblyMonad "constructor"

    // Property 1. Left identity
    val leftIdentity = (pure(42) flatMap f) == f(42) // PossiblyMonad(42).flatMap(f) == f(42) for every x, every f

    // Property 2. Right identity
    val rightIdentity = (possiblyMonad flatMap pure) == possiblyMonad

    // Property 3. Associativity
    val associativity = ((possiblyMonad flatMap f) flatMap g) == (possiblyMonad flatMap (f(_) flatMap g))

    println(leftIdentity)   // false
    println(rightIdentity)  // false
    println(associativity)  // false
    // ^^ All these are false negatives, as should be comparing the values rather than the functions

    // Also false because the apply method simply constructs a new lambda
    // '() => value' every time and the runtime has no way of comparing two
    // lambdas for equality even if they are functionally equal
    println(IO(3) == IO(3)) // false
    // This is also a false negative, as should be comparing the values rather than the functions

    // Modified versions of the above properties. These are the REAL tests!
    // Now we compare the VALUES generated from executing unsafeRun()
    val leftIdentity_v2 = (pure(42) flatMap f).unsafeRun() == f(42).unsafeRun() //
    val rightIdentity_v2 = (possiblyMonad flatMap pure).unsafeRun() == possiblyMonad.unsafeRun() //
    val associativity_v2 = ((possiblyMonad flatMap f) flatMap g).unsafeRun() ==
                            (possiblyMonad flatMap (f(_) flatMap g)).unsafeRun()

    println(leftIdentity_v2) // true
    println(rightIdentity_v2) // true
    println(associativity_v2) // true

    //---------------------------------------------------------------------------
    // So PossiblyMonad (IO) is indeed a monad.
    // What's a zero lambda? A zero lambda is a computation that might produce
    // side-effects (print to a console, send something over the wire, etc.)
    // AND ultimately produce a value.
    // So PossiblyMonad wraps any sort of computation that might produce
    // side-effects in addition to producing values.

    // Test the ORDER OF PRODUCTION of the effects, associativity in particular.
    // Test that f and g are applied in the exact sequence that we specified.
    val fs = (x: Int) => IO { // function with a side-effect
      println("incrementing")
      x + 1
    }

    val gs = (x: Int) => IO { // function with a side-effect
      println("doubling")
      2 * x
    }

    // Prints "incrementing" and "doubling" in that order
    val associativity_v3 = ((possiblyMonad flatMap fs) flatMap gs).unsafeRun() ==
      (possiblyMonad flatMap (fs(_) flatMap gs)).unsafeRun()
  }

  //---------------------------------------------------------------------------
  def possiblyMonadExample(): Unit = {

    // PossiblyMonad is a data structure that describes any sort of computation
    // without actually performing it at the time of construction.
    // It SEPARATES the description of a computation from the actual execution
    // of that computation.
    val aPossiblyMonad: IO[Int] = IO {
      println("printing my first PossiblyMonad")
      42
    }

    val anotherPM: IO[String] = IO {
      println("my second PossiblyMonad")
      "Scala"
    }

    // If we want to perform the computation that's described in the structure
    // of the monad, we have to call unsafeRun
    val aResult = aPossiblyMonad.unsafeRun()
    println(aResult)

    // We can combine the values returned by these computations without
    // first performing those side-effectful computations. Here's how we combine
    // the results of both the above PossiblyMonads.

    // The following for comprehension (i.e. flatMaps) will NOT perform any
    // computation that's associated with the two PossiblyMonads.
    // Computations are DESCRIBED, not EXECUTED.
    val result =
      for
        num <- aPossiblyMonad
        lang <- anotherPM
      yield s"$num-$lang"

    /*
      In very general terms, PossiblyMonad will allow us to do pure functional
      programming on any sort of computation that might perform side-effects.
      So we can chain or compose computations that perform side-effects and at
      the end of our application we can simply call unsafeRun on the final
      PossiblyMonad instance that we obtain at the end of our program.
      The PossiblyMonad is known as IO in Cats-Effect and Zio
    */
  }

  //---------------------------------------------------------------------------
  // Monad Summary
  /*
  trait MonadTemplate[A]:
    def flatMap[B](f: A => MonadTemplate[B]): MonadTemplate[B]  // flatMap is also known as bind

  object MonadTemplate:
    def apply[A](value: => A): MonadTemplate[A] // also called 'pure' or 'unit

  Monad laws
  left identity: unit(x).flatMap(f) == f(x)
  right identity: aMonadInstance.flatMap(unit) == aMonadInstance
  associativity: (m flatMap f) flatMap g == m flatMap (x => f(x).flatMap(g))
  */

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    possiblyMonadStory()
    // possiblyMonadExample()  // Does NOT print the printlns from aPossiblyMonad and anotherPM
  }
}
