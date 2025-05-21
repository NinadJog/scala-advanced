package part4context

object Givens {

  /**
   * Givens are a way for the compiler to inject arguments automatically
   *
   * Uses of givens:
   * 1. Changing program behavior depending upon context i.e. contextual abstractions.
   * 2. Type classes
   * 3. Automatic dependency injection
   * 4. Type-level programming
   */
  // list sorting
  val aList = List(4, 2, 3, 1)
  val anOrderedList = aList.sorted//(descendingOrdering) injected by the compiler

  given descendingOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  val inverseOrderedList = aList.sorted(descendingOrdering)

  //---------------------------------------------------------------------------
  // Custom sorting
  case class Person(name: String, age: Int)
  val people = List(Person("Alice", 29), Person("Sarah", 34), Person("Jim", 23))

  // An ordering for the Person class. Sort by name in ascending order
  // Instance of the Ordering typeclass
  given personOrdering: Ordering[Person] with
    override def compare(x: Person, y: Person): Int =
      x.name compareTo y.name

  val sortedPeople = people.sorted

  //---------------------------------------------------------------------------
  // using clauses: writing methods that require a given instance

  trait Combinator[A]:
    def combine(x: A, y: A): A

  // Combine elements from a list. Uses a using clause
  def combineAll[A](list: List[A])(using combinator: Combinator[A]) =
    list.reduce(combinator.combine)

  /* We should be able to do things such as
   * combineAll(List(1, 2, 3)
   * combineAll(people)
   */

  // Instance of the Combinator typeclass for Int
  given intCombinator: Combinator[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  val firstSum = combineAll(List(1, 2, 3))//(intCombinator) <-- automatically passed by the compiler

  //---------------------------------------------------------------------------
  // context bounds
  // Version 1. The combinator is not used explicitly in the body of the following function.
  def combineInGroupsOf3[A](list: List[A])(using combinator: Combinator[A]): List[A] =
    list
      .grouped(3)           // Group into lists of 3
      .map(combineAll(_)/*(combinator) passed by compiler*/)   // Combine each list of 3 elements
      .toList

  // Version 2. Since the argument named 'combinator' is not being used explicitly, we can omit it:
  def combineInGroupsOf2[A](list: List[A])(using Combinator[A]): List[A] =
    list
      .grouped(3) // Group into lists of 3
      .map(combineAll(_)/* given Combinator[A] is passed by the compiler*/) // Combine each list of 3 elements
      .toList

  // Version 3. The [A : Combinator] tells the compiler that a given combinator of type A
  // exists in scope. [A : Combinator] is called a context bound
  def combineInGroupsOf3_v3[A : Combinator](list: List[A]): List[A] =
    list
      .grouped(3)         // Group into lists of 3
      .map(combineAll(_)/* given Combinator[A] is passed by compiler*/) // Combine each list of 3 elements
      .toList

  //---------------------------------------------------------------------------
  // Synthesise new given instances based on existing ones.

  // Given an Int ordering, we can synthesize (create) an ordering of a List[Int]
  given listOrdering(using intOrdering: Ordering[Int]): Ordering[List[Int]] with
    override def compare(x: List[Int], y: List[Int]): Int = x.sum - y.sum

  // Now we can use the sorted method on a list of lists!
  val listOfLists = List(List(1, 2), List(1, 1), List(3, 4, 5))

  // When we sort it, it will be sorted in increasing order of their sums:
  // List(List(1, 1), List(1, 2), List(3, 4, 5))
  val nestedListsOrdered = listOfLists.sorted

  // Synthesizing can also be taken to the next level with type arguments,
  // with generics.
  // Example: Now that we have an Ordering for a list of Int, we want to generate
  // an ordering for a list of anything. The following method is powerful because
  // it's available for any type A for which we have an Ordering in scope and a
  // Combinator in scope
  given listOrderingBasedOnCombinator[A](using ord: Ordering[A])
                                        (using combinator: Combinator[A]): Ordering[List[A]] with
    override def compare(x: List[A], y: List[A]): Int =
      ord.compare(combineAll(x), combineAll(y))

  //---------------------------------------------------------------------------
  // Pass a regular value instead of a given

  // Since myCombinator is not a given, we can't use the 'with' syntax
  val myCombinator = new Combinator[Int] {
    override def combine(x: Int, y: Int): Int = x * y // product instead of sum
  }

  // In this case the 'using' keyword has a different meaning: it means use
  // whatever value (function) is being passed after the using keyword
  // instead of passing the 'given' value that the compiler tries to inject
  val listProduct = combineAll(List(1, 2, 3, 4))(using myCombinator)

  //---------------------------------------------------------------------------
  // Exercise 1. Create a given for ordering Option[A] if you can order A.

  // Signature is similar to the that of List[Ordering[A]] from line 73 above.
  given optionOrdering[A](using ord: Ordering[A]): Ordering[Option[A]] with
    override def compare(x: Option[A], y: Option[A]): Int =
      (x, y) match
        case (None, None)       => 0
        case (_, None)          => 1  // Assume Some > None
        case (None, _)          => -1 // None < Some
        case (Some(a), Some(b)) => ord.compare(a, b)

  // Test it
  val optionList = List(Option(1), Option.empty[Int], Option(3), Option(-100))
  val optionListSorted = optionList.sorted // List(None, Some(3), Some(1), Some(-100))
  // because we have the given descendingOrdering for Int in scope.

  //---------------------------------------------------------------------------
  // Exercise 2. Create a summoning method that fetches the given value of your
  // particular type.

  // Motivation: In Exercise 1, we would like to eliminate the method argument
  // (using ord: Ordering[A]) and instead use the context bound [A : Ordering].
  // If we try to do that, the ord.compare(a, b) in the last line of the method
  // won't compile, as ord has been removed. The solution is to fetch or summon
  // the appropriate compare method for type A.

  // Solution:
  def fetchGivenValue[A](using theValue: A): A = theValue

  // We can now use this to simplify the solution to Exercise 1:
  // (Placing it inside a local object because otherwise there are 2 givens
  // of the same type, creating an ambiguity.)
  object OptionOrdering:
    given optionOrdering_v2[A : Ordering]: Ordering[Option[A]] with
      override def compare(x: Option[A], y: Option[A]): Int =
        (x, y) match
          case (None, None)       => 0
          case (_, None)          => 1 // Assume Some > None
          case (None, _)          => -1 // None < Some
          case (Some(a), Some(b)) => fetchGivenValue[Ordering[A]].compare(a, b)

    // Since this functionality is so useful, Scala provides a method named
    // summon in the standard library for this functionality. Here's the
    // implementation with a call to summon:
    given optionOrdering_v3[A : Ordering]: Ordering[Option[A]] with
      override def compare(x: Option[A], y: Option[A]): Int =
        (x, y) match
          case (None, None)       => 0
          case (_, None)          => 1 // Assume Some > None
          case (None, _)          => -1 // None < Some
          case (Some(a), Some(b)) => summon[Ordering[A]].compare(a, b)

  end OptionOrdering

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(anOrderedList)      // [1, 2, 3, 4] in the absence of given descendingOrdering
    println(inverseOrderedList) // [4, 3, 2, 1]

    println(firstSum)// 6
    println(nestedListsOrdered) // List(List(1, 1), List(1, 2), List(3, 4, 5))

    println(optionListSorted) // List(None, Some(3), Some(1), Some(-100))
  }
}
