package part1Recap

// Implicits

object ContextualAbstractionsScala2 {

  case class Person(name: String) {
    def greet(): String = s"""One two three hello im $name"""
  }

  //need the implicit keyword otherwise greet wont compile
  implicit class ImpersonableString(name: String) {
    def greet(): String =
      Person(name).greet()
  }

  val greeting = "Peter".greet()
  val werid = "Does this make sense?".greet()

  // example:

  import scala.concurrent.duration._

  val oneSecond = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount: Int = 10
  val twelve = increment(2) // implicit arugment 10 passed by the compiler
  // This is done purely by the type

  def multiply(x: Int)(implicit factor: Int) = x * factor

  val aHundred = multiply(10) // same implicit argument passed by the compiler

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def convert2Json[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person) = "{\"name\" : \"" + person.name + "\"}"
  }

  val davidsJson = convert2Json(Person("David")) // implicit serializer passed here.... implicitly

  // implicits defs
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]) = s"[${list.map(serializer.toJson).mkString(",")}]"
    }

  val personsJson = convert2Json(List(Person("Alice"), Person("Bob")))

  // implicit conversions (not recommended)
  case class Cat(name: String) {
    def meow(): String = s"$name is meowing"
  }

  implicit def string2Cat(name: String): Cat = Cat(name) // Can use a string whereever a cat is needed

  val aCat: Cat = "Garfield" // string2Cat("Garfield")
  val garfieldMeowing = "Garfield".meow() //  string2Cat("Garfield").meow()

  def needsCat(cat: Cat): Int = {
    1234
  }

  needsCat("Is this a cat though?")


  def main(args: Array[String]): Unit = {

  }
}

object TypeClassesScala2 {
  case class Person(name: String, age: Int)

  // part 1 - Type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - type class instances
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String) = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int) = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person) =
      s"""
         |{"name" : "${value.name}", "age" : ${value.age}}
         |""".stripMargin.trim
  }

  // part 3 - offer some API
  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  // part 4 - add extension methods --- in the prescesnse of an implicit serialiser of type T i.e for float it wouldnt work
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  //  convertListToJson(List(1.2,3.4)) ///List contains doubles -- wont compile
  convertListToJson(List(1, 4))

  def main(args: Array[String]): Unit = {
    println(convertListToJson(List(Person("Alice", 23), Person("Xavier", 45))))
    val bob = Person("Bob", 68)

    import JSONSyntax._
    println(bob.toJson)
  }
}
