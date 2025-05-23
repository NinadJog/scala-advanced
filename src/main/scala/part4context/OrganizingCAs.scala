package part4context

/**
 * Best practices for where to place givens. The same tips also apply to
 * extension methods.
 *
 * 1. When you have a DEFAULT given (one that makes sense 99% of the time),
 *    place it in the companion object of the type. It will be automatically
 *    picked up by the compiler.
 *
 * 2. When you have MANY givens but ONE that's dominant (used most often; say
 *    one that applies 70% of the time), place the dominant given in the
 *    companion object and the other given(s) in separate
 *    objects. Import the non-dominant givens explicitly as needed.
 *
 * 3. When you have MANY givens but NONE of them is dominant, place ALL of them
 *    in separate objects and import them explicitly as needed for the business logic.
 */
object OrganizingCAs {

  val aList = List(4, 3, 1, 2)
  val anOrderedList = aList.sorted

  // Where does the compiler fetch givens & extension methods?
  // 1 - local scope
  given intOrdering: Ordering[Int] with
    override def compare(x: Int, y: Int): Int = y - x // reverse ordering

  //---------------------------------------------------------------------------
  // 2 - imported scope
  // All the explicitly imported given values from other packages or places
  case class Person(name: String, age: Int)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 67)
  )

  // Define a givens to sort the Person list by name; define the givens in an object
  object PersonGivens {

    // Sort in descending order by age
    given ageOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person): Int = x.age - y.age

    extension (p: Person)
      def greet: String = s"Heya, my name is ${p.name}. Nice to meet you!"
  }

  //-------------------
  // Different ways of importing. Use one of these ways to import.
  // a. explicit import
  // import PersonGivens.ageOrdering // works

  // b. import a given for a particular type
  // This is handy when we don't really know the name of our given instance
  // import PersonGivens.given Ordering[Person]  // We can also use {} after PersonGivens.

  // c. import ALL givens
  // import PersonGivens.given    // works but commented out to test other givens strategies
  val sortedPersons: List[Person] = persons.sorted

  // Warning: Using import PersonGivens.* does NOT import any of the givens;
  // it imports everything EXCEPT the givens!

  //---------------------------------------------------------------------------
  // 3. The compiler looks in the companion objects of ALL types involved in the method signature
  /**
   * Example: The sorted function has the following simplified type signature:
    def sorted[B >: A](using ord: Ordering[B]): List[B]

   The compiler looks for givens in the companion objects of
    - Ordering
    - List
    - Person (== B in the above signature of sorted)
  */
  object Person {   // companion object
    given byNameOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person): Int = x.name compareTo y.name

    extension (p: Person)
      def greet: String = s"Hi, my name is ${p.name}"
  }

  //===========================================================================
  // Exercises
  /**
   * Create given instances for Ordering[Purchase]
   * - ordering by total price, descending = 50% of code base
   * - ordering by unit count, descending  = 25% of code base
   * - ordering by unit price, ascending   = 25% of code base
   */
  case class Purchase(nUnits: Int, unitPrice: Double):
    lazy val totalPrice: Double = nUnits * unitPrice

  object Purchase: // companion object
    given totalPriceOrd: Ordering[Purchase] with // Order by total price descending
      override def compare(x: Purchase, y: Purchase): Int =
        val priceDiff = x.totalPrice - y.totalPrice
        if (priceDiff == 0)      0
        else if (priceDiff < 0) -1
        else                     1

  // Alternative compact implementation of the total price ordering
  object TotalPriceOrdering:
    given totalPriceOrd_v2: Ordering[Purchase] = Ordering.fromLessThan((x, y) => x.totalPrice < y.totalPrice)

  // order by unit count descending
  object UnitCountOrdering:
    given unitCountOrd: Ordering[Purchase] = Ordering.fromLessThan((x, y) => x.nUnits < y.nUnits)

  // order by unit price ascending
  object UnitPriceOrdering:
    given unitPriceOrd: Ordering[Purchase] = Ordering.fromLessThan((x, y) => y.unitPrice < x.unitPrice)

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    println(anOrderedList)
    println(sortedPersons) // List(Person(Amy,22), Person(John,67), Person(Steve,30)) when byNameOrdering is used

    import PersonGivens.* // imports everything INCLUDING the extension method greet (but doesn't import givens)
    println(Person("Daniel", 22).greet)
  }
}
