package part4context

/* Type classes are a pattern of organizing code that
 * - Decouples implementations from the definition
 * - Allows multiple implementations for the same type
 * - Brings back (or improves) expressiveness
 *
 * Construction Summary
 * 1. Type class definition e.g. MyTypeClass[T]
 * 2. Type class instances (givens) e.g. MyTypeClass[Int]
 * 3. Front-facing API (in companion object of MyTypeClass) with methods that take contextual parameters via using clauses.
 * 4. Expressiveness with Extension Methods (e.g. in object named MyTypeClassSyntax)
 */

object TypeClasses {

  // Small library that serializes data to a standard format (HTML)

  // Version 1. The Object-Oriented Way
  trait HTMLWritable:
    def toHtml: String

  // Every data structure will have to inherit from (or implement) this trait.
  case class User(name: String, age: Int, email: String) extends HTMLWritable:
    override def toHtml: String = s"<div>$name ($age years old) <a href=$email/></div>"

  val bob = User("Bob", 43, "bob@rockthejvm.com")
  val bobToHtml: String = bob.toHtml
  // Do the same for all the other data structures that we want to serialize to HTML.

  /*
    Drawbacks:
    1. Available only for the types we write in our little library. (If a data
       structure is not part of the library, it's a problem because it won't be
       serializable)
    2. Only one implementation. We are stuck with a single implementation of
       the toHtml method in the interface. If we are doing two UI's and we want
       to represent User in two different ways, we will have to enhance the
       HTMLWriter interface (by adding a method/API to it) and force the entire
       data structure hierarchy to implement it.
   */

  //---------------------------------------------------------------------------
  // Version 2. Pattern Matching

  object HTMLSerializerPM:
    def serializeToHtml(value: Any): String = value match
      case User(name, age, email) => s"<div>$name ($age years old) <a href=$email/></div>"
      case _ => throw new IllegalArgumentException("data structure not supported")

  /*
    - This is the Scala-specific approach. Simplest to implement but it has the
      following drawbacks.
      1. We have lost type safety. When we pass a data structure to this API, we
         have no way of knowing whether it is supported, because it takes a value
         of type Any and does a pattern match on it. We have to wait until the
         runtime throws an exception to know whether our data structure is
         supported or not.
      2. Need to modify a SINGLE piece of code (the serializeToHtml method) every time.
   */

  //---------------------------------------------------------------------------
  // V3. Type class

  // Part 1 - Type class definition (Note: the companion object appears later in this file.)
  trait HTMLSerializer[T]:
    def serialize(value: T): String

  // Part 2 -Type class instances for the types we want to support.
  // The interesting thing about these instances is that we pass them implicitly
  // i.e. as given values.
  given userSerializer: HTMLSerializer[User] with
    override def serialize(value: User): String = {
      val User(name, age, email) = value
      s"<div>$name ($age years old) <a href=$email/></div>"
    }

  val bobToHtml_v2 = userSerializer.serialize(bob)

  /*
    Benefits:
    1. Can define serializers for other types OUTSIDE the "library" since the
       implementation is separate from the interface. Users of the library
       can define implementations for their own types.
    2. Multiple serializers for the same type; can pick whichever one you want
       by organizing the given instances appropriately.
   */

  // Example: implement a serializer for Date
  import java.util.Date
  given dateSerializer: HTMLSerializer[Date] with
    override def serialize(date: Date): String = s"<div>${date.toString}</div>"

  // Example: A different implementation for the User serializer.
  // (Shield it inside an object so we can turn it into a given without causing
  // duplicate instances in scope.)
  object SomeOtherSerializerFunctionality:  // organize givens properly
    given partialUserSerializer: HTMLSerializer[User] with
      override def serialize(user: User): String = {
        val User(name, _, _) = user
        s"<div>$name</div>"
      }

  //---------------------------------------------------------------------------
  // Part 3 - Using the type class (Create a user-facing API in the form of the serialize method)

  // Place the front-facing API in the companion object of the User type
  object HTMLSerializer {
    def serialize[T](value: T)(using serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    // alternative implementation using context bounds and summon
    // (I wrote this on my own; needs to be tested.)
    def serialize_v2[T : HTMLSerializer](value: T): String =
      summon[HTMLSerializer[T]].serialize(value)

    // alternative implementation using context bounds and apply method
    // (I wrote this on my own; needs to be tested.)
    def serialize_v3[T : HTMLSerializer](value: T): String =
      HTMLSerializer[T].serialize(value)

    // We add an apply method that simply surfaces out whichever 'given'
    // instance of serializer we have in scope for this type class.
    // Note that this apply method takes a type parameter in []
    def apply[T](using serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }

  /*
   * Even though the following function calls look just as complicated as the
   * one for bobToHtml_v2, the advantage is that we don't have to rely on the
   * name of the given instance, so we don't need to know where the given
   * instance is or how it was imported.
   */
  val bobToHtml_v3 = HTMLSerializer.serialize(bob)
  val bobToHtml_v4 = HTMLSerializer.serialize_v2(bob)
  val bobToHtml_v5 = HTMLSerializer.serialize_v3(bob)

  // Use the apply method to surface out the serializer instance
  // HTMLSerializer[User] and then call serialize on that instance.
  val bobToHtml_v6 = HTMLSerializer[User].serialize(bob)


  /*
   * At this point we have 3 structures that are decoupled from each other:
   * 1) The trait (co-data) that defines the HtmlSerializer
   * 2) The type class instances for certain data types (User, Date)
   * 3) The user-facing API that uses the type class instances (methods that
   *    have contextual parameters in the form of 'using' clauses or context
   *    bounds.
   */

  //---------------------------------------------------------------------------
  // Part 4 - Extension methods

  // Used to bring back the simplicity we had in the OO version, where we just
  // called bob.toHtml.
  // The user-facing API is placed in an object whose name is suffixed with
  // 'Syntax' by convention:

  object HtmlSyntax:
    extension [T](value: T)
      def toHTML(using serializer: HTMLSerializer[T]): String =
        serializer.serialize(value)

  // Use the extension method in the main application.
  // We are back to the same expressiveness that we had at the beginning with
  // the OO version without having to care where the HTMLSerializer is
  // defined, where the given instance is defined and imported, etc.
  import HtmlSyntax.*
  val bobToHtml_v7 = bob.toHTML

  /*
   * Bottom Line:
   * - Extend functionality to new types that we want to support
   * - Flexibility to add TC instances in a different place than the TC definition
   * - Choose implementations (by importing the appropriate givens)
   * - super expressive (via extension methods)
   */

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    println(bobToHtml)    // The OO way
    println(bobToHtml_v2) // Calling a type class instance's 'given' explicitly

    println(bobToHtml_v3) // Using the type class (Calling the user-facing API)
    println(bobToHtml_v4) // Alternative implementation using context bounds
    println(bobToHtml_v5) // Alternative implementation using apply method.

    println(bobToHtml_v6) // Calls serialize on the serializer surfaced via the apply method
    println(bobToHtml_v7) // Call the extension method
  }
}
