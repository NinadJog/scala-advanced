package part4context

import scala.annotation.tailrec

object ExtensionMethods {

  /**
    1. Extension methods enhance a type AFTER it has been defined.
    2. Adds new capabilities to existing types, including standard library types.
    3. Make APIs more expressive
    4. Add new behaviors to certain types SELECTIVELY based on context,
       with given/using clauses
   */
  case class Person(name: String):  // Enhances the String type
    def greet: String = s"Hi, my name is $name. Nice to meet you!"

  extension (string: String)
    def greetAsPerson:String = Person(string).greet

  val danielGreeting:String = "Daniel".greetAsPerson

  //---------------------------------------------------------------------------
  // generic extension methods
  extension [A](list: List[A])  // Enhances the one-holed type List
    def ends: (A, A) = (list.head, list.last)

  val aList = List(1, 2, 3, 4)
  val firstLast = aList.ends

  //---------------------------------------------------------------------------
  // Advantages
  // 1. Generic extension methods make APIs very expressive, especially when combined
  // the contextual abstractions of given and using.
  // 2. Can enhance CERTAIN types with new capabilities
  // 3. Extension methods are completely separate (orthogonal) from the OO
  //    way of adding capabilities to existing types.

  trait Semigroup[A]:  // typeclass originally named Combinator
    def combine(x: A, y:A): A

  // extension is also a contextual abstraction because it doesn't work for
  // all types A, but only for those types for whom there's a given
  // Combinator[A] in scope.
  extension [A](list: List[A])
    def combineAll(using combinator: Semigroup[A]): A =
      list.reduce(combinator.combine)

  // Let's define an Int combinator so we can combine List[Int]
  // Implementation of the Combinator typeclass for the type Int
  given intCombinator: Semigroup[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  val firstSum = aList.combineAll

  // Following code does not compile because the Combinator type class has not
  // been implemented for String i.e. there's no given Combinator[String] in scope.
  // val stringConcat = List("I", "love", "Scala").combineAll

  //---------------------------------------------------------------------------
  // grouping extensions

  object GroupedExtensions:
    extension [A](list: List[A])
      def ends: (A, A) = (list.head, list.last)
      def combineAll(using combinator: Semigroup[A]): A = list.reduce(combinator.combine)

  // calling extension methods directly
  val firstLast_v2 = ends(aList)   // same as aList.ends

  //---------------------------------------------------------------------------
  // Exercise 1. Add an isPrime method to the Int type

  extension (number: Int)
    def isPrime: Boolean = {
      // Returns true if n is not divisible by each of the successive
      // integers t, t-1, t-2, etc. all the way down to 1.
      // For example, if n = 11, check whether it's divisible by 5, 4, 3, 2, 1
      @tailrec
      def isPrimeUntil(t: Int): Boolean =
        if (t <= 1) true // If we have checked all the way down to 1, that means it's prime
        else if (number % t == 0) false
        else isPrimeUntil(t - 1)

      isPrimeUntil(number / 2)
    }

    // Instructor's solution. Increments potential divisors starting from 2
    def isPrime_v2: Boolean = {
      @tailrec
      def isPrimeAux(potentialDivisor: Int): Boolean = {
        if (potentialDivisor > number / 2) true
        else if (number % potentialDivisor == 0) false
        else isPrimeAux(potentialDivisor + 1)
      }
      assert (number >= 0)
      if (number == 0 || number == 1) false
      else isPrimeAux(number / 2)
    }

  val is7prime: Boolean   = 7.isPrime   // true
  val is36prime: Boolean  = 36.isPrime  // false

  val is2003prime: Boolean = 2003.isPrime_v2 // true

  //---------------------------------------------------------------------------
  /** Exercise 2. Assuming that the following Tree structure can't be modified,
    add the following extensions to Tree.
      - map(f: A => B): Tree[B]
      - forall(predicate: A => Boolean): Boolean
      - sum: sum of all elements of the tree for Int
      - combine all elements of the tree using a combinator
  */

  // The instructor created the Tree using a sealed trait and case classes, but
  // I chose to model it with an enum. In either case, Tree is a sum algebraic data type (ADT)
  enum Tree[A]:
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])

  import Tree.* // If we don't import this, then Leaf and Branch have to be qualified
  // as Tree.Leaf and Tree.Branch in the following pattern match code.

  extension [A](tree: Tree[A])
    def map[B](f: A => B): Tree[B] = tree match
      case Leaf(value)          => Leaf(f(value))
      case Branch(left, right)  => Branch(left map f, right map f)

    def forall(predicate: A => Boolean): Boolean = tree match
      case Leaf(value)          => predicate(value)
      case Branch(left, right)  => (left forall predicate) && (right forall predicate)

    // Makes the context available at the METHOD level (using Combinator[A])
    // This was also the instructor's solution
    def combineAll(using combinator: Semigroup[A]): A = tree match
      case Leaf(value)          => value
      case Branch(left, right)  => combinator.combine(left.combineAll, right.combineAll)

    // same as sum, but calls summon[Combinator[A]]
    // Makes the context available at the METHOD level (using Combinator[A])
    def combineAll_v2(using Semigroup[A]): A = tree match
      case Leaf(value)          => value
      case Branch(left, right)  => summon[Semigroup[A]]
                                    .combine(left.combineAll_v2, right.combineAll_v2)

  // uses a context bound [A : Combinator] and the summon method.
  // Makes the context available at the EXTENSION level
  extension [A: Semigroup](tree: Tree[A])
    def combineAll_v3: A = tree match
      case Leaf (value)         => value
      case Branch (left, right) => summon[Semigroup[A]]
                                    .combine (left.combineAll_v3, right.combineAll_v3)

  // Sum all elements (leaves) of a Tree[Int]
  extension (tree: Tree[Int])
    def sum: Int = tree match
      case Leaf(value)          => value
      case Branch(left, right)  => left.sum + right.sum

  val aTree: Tree[Int] = Branch(Leaf(10), Branch(Leaf(1), Leaf(3)))
  val sumTree     = aTree.sum                 // 14
  val incTree     = aTree map (_+1)
  val isEvenTree  = aTree forall (_%2 == 0)   // false
  val combineTree = aTree.combineAll          // 14. Uses the Int combinator from earlier

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(danielGreeting) // Hi, my name is Daniel. Nice to meet you!
    println(ends)           // (1, 4)
    println(firstSum)       // 10
    println(firstLast_v2)   // 10

    // Exercise 1
    println(is7prime)       // true
    println(is36prime)      // false
    println(is2003prime)    // true

    // Exercise 2
    println(sumTree)        // 14
    println(incTree)        // Branch(Leaf(11), Branch(Leaf(2), Leaf(4)))
    println(isEvenTree)     // false
    println(combineTree)    // 14
  }
}
