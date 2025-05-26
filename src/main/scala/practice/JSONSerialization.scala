package practice

import java.util.Date

object JSONSerialization {

  /* Use case for building library:
   * Users, blog posts, feeds (which are collections of blog posts)
   * Serialize to JSON using type classes
   */

  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

  /* Objective: Serialize all of these to JSON.
   * Steps:
   * 1. Define intermediate data types that the JSON Spec supports: numbers,
   *    strings, lists, objects
   * 2. Define a type class that can convert any of our types (User, Post, Feed)
   *    into this intermediate representation.
   * 3. Combine these together to serialize to JSON.
   */

  //---------------------------------------------------------------------------
  // Step 1. Define the intermediate types that the JSON spec supports
  enum JSONValue:
    case JSONString(value: String)
    case JSONNumber(value: Int)
    case JSONArray(values: List[JSONValue])
    case JSONObject(values: Map[String, JSONValue])

    def stringify: String = this match
      case JSONString(value)  => s""""$value""""
      case JSONNumber(value)  => value.toString
      case JSONArray(values)  => values.map(_.stringify).mkString("[", ",", "]")
      case JSONObject(values) => values.map {
        case (key, value) => "\"" + key + "\":" + value.stringify
      }.mkString("{", ",", "}")

  /* Example of a JSONObject:
   {
      "name": "John",
      "age": 22,
      "friends": [...]
      "latestPost": {...}
    }
   */

  /* Instructor's version used a sealed trait and case classes; I used the more concise enum.
  sealed trait JSONValue:
    def stringify: String

  final case class JSONString(value: String) extends JSONValue:
    override def stringify: String = s"\"$value\""

  final case class JSONNumber(value: Int) extends JSONValue:
    override def stringify: String = value.toString

  final case class JSONArray(values: List[JSONValue]) extends JSONValue:
    override def stringify: String =
      (values map (_.stringify)).mkString("[", ",", "]") // ["string", 4, ...]

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue:
    override def stringify: String = values.map {
      case (key, value) => "\" + key + \":" + value.stringify
    }.mkString("{", ",", "}")
  */

  import JSONValue.* // need to do this because we defined JSONValue as an enum
  val data = JSONObject(Map(
    "user" -> JSONString("Daniel"),
    "posts" -> JSONArray(List(
      JSONString("Scala is awesome!"),
      JSONNumber(42)
    ))
  ))

  //---------------------------------------------------------------------------
  // Step 2. Write a type class to convert our custom data types to this intermediate
  // representation. This has 4 steps

  // 1) Type class definition
  //    We call it Converter rather than Serializer because it's converting from
  //    one data type to another.
  trait JSONConverter[T]:
    def convert(value: T): JSONValue

  //---------------------------------------------------------------------------
  // 2) Instances of type class for User, Post, Feed and the types they depend
  //    upon: String, Int, Date

  given intConverter: JSONConverter[Int] with
    override def convert(value: Int): JSONValue = JSONNumber(value)

  given stringConverter: JSONConverter[String] with
    override def convert(value: String): JSONValue = JSONString(value)

  given dateConverter: JSONConverter[Date] with
    override def convert(value: Date): JSONValue = JSONString(value.toString)

  // For the converters of the custom objects, use the int, string & date converters
  // by calling them explicitly
  given userConverter: JSONConverter[User] with
    override def convert(user: User): JSONValue = {
      val User(name, age, email) = user
      JSONObject(Map(
        "name"  -> JSONConverter[String].convert(name), // or use stringConverter.convert(name) or JSONString(name)
        "age"   -> JSONConverter[Int].convert(age),     // or use intConverter.convert(age) or JSONNumber(age),
        "email" -> JSONConverter[String].convert(email)
      ))
    }

  given postConverter: JSONConverter[Post] with
    override def convert(post: Post): JSONValue = {
      val Post(content, createdAt) = post
      JSONObject(Map(
        "content"   -> JSONConverter[String].convert(content),
        "createdAt" -> JSONConverter[Date].convert(createdAt) // or use dateConverter.convert(createdAt)
      ))
    }

  given feedConverter: JSONConverter[Feed] with
    override def convert(feed: Feed): JSONValue =
      JSONObject(Map(
        "user" -> JSONConverter[User].convert(feed.user), // or use userConverter.convert
        "posts" -> JSONArray(feed.posts map JSONConverter[Post].convert) // convert List[Post] to List[JSONValue]
      ))

  //---------------------------------------------------------------------------
  // 3) User-facing API
  //    This is usually done through the companion object of the type class (JSONConverter)
  object JSONConverter {

    // My solution. Uses the apply method to get the appropriate convert method based on
    // the type JSONConverter[T]
    def convert[T : JSONConverter](value: T): JSONValue =
      JSONConverter[T].convert(value)

    /* Instructor's solution
    def convert[T](value: T)(using converter: JSONConverter[T]): JSONValue =
      converter.convert(value)
    */

    // Also define an apply method that surfaces out the JSONConverter.
    // It's because of this apply method that we were able to use
    // JSONConverter[String].convert, JSONConverter[User].convert, etc.
    // in the givens above. If we comment it out, none of them will compile
    def apply[T](using instance: JSONConverter[T]): JSONConverter[T] = instance
  }

  // Example for testing
  val now = new Date(System.currentTimeMillis())
  val john = User("John", 34, "john@rockthejvm.com")
  val feed = Feed(john, List(
    Post("Hello, I'm learning type classes", now),
    Post("Look at this cute puppy!", now)
  ))

  // The following works, but it looks clunky. So we will use extension methods
  val feedJsonString: String = JSONConverter.convert(feed).stringify

  //---------------------------------------------------------------------------
  // 4) Extension methods

  object JSONSyntax:
    extension [T](value: T)
      def toIntermediate(using converter: JSONConverter[T]): JSONValue =
        converter.convert(value)

      def toJson(using converter: JSONConverter[T]): String =
        toIntermediate.stringify

  // Test the extension method
  import JSONSyntax.*
  val feedJsonString_v2: String = feed.toJson // This is our ultimate API

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(data.stringify) // {"user":"Daniel","posts":["Scala is awesome!",42]}
    println(feedJsonString)
    println(feedJsonString_v2)
  }
}
