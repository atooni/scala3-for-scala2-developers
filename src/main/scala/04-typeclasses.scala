/**
 * TYPECLASSES
 * 
 * Scala 3 introduces direct support for typeclasses using contextual features of the language.
 * Typeclasses provide a way to abstract over similar data types, without having to change the 
 * inheritance hierarchy of those data types, providing the power of "mixin" interfaces, but 
 * with additional flexibility that plays well with third-party data types.
 */
object typeclass_basics:
 
  final case class Person2(name: String, age: Int)

  trait Comparable[A]:
    extension (self: A) def compare(that : A): Int

  object Person2:
    given Comparable[Person2] = 
      new Comparable[Person2]:
        extension (self: Person2) def compare(that: Person2): Int = 
          if (self.name < that.name) -1
          else if (self.name > that.name) 1
          else if (self.age < that.age) -1 
          else if (self.age > that.age) 1
          else 0

  def sort[A](list : List[A])(using Comparable[A]): List[A] = list match
    case Nil => Nil
    case x :: xs => 
      val (lt, gt) = list.partition(_.compare(x) < 0)
      sort(lt) ++ sort(gt)

  val l = List(Person2("Andreas", 52), Person2("Karin", 54))    
  val l2 = sort(l)

  given Comparable[String] = 
    new Comparable[String]:
      extension (self:String) def compare(that: String): Int = 
        if (self < that) -1 
        else if (self > that) 1
        else 0

  sort(List("foo", "bar"))

  trait PrettyPrint[-A]:
    extension (a: A) def prettyPrint: String

  given PrettyPrint[String]:
    extension (a: String) def prettyPrint: String = a

  "foo".prettyPrint

  final case class Person(name: String, age: Int)

  /**
   * EXERCISE 1
   * 
   * With the help of the `given` keyword, create an instance of the `PrettyPrint` typeclass for the 
   * data type `Person` that renders the person in a pretty way.
   */
  given PrettyPrint[Person]:
    extension (p: Person) def prettyPrint: String = s"Person(${p.name}, ${p.age})"

  val s = Person("Andreas", 52).prettyPrint  

  /**
   * EXERCISE 2
   * 
   * With the help of the `given` keyword, create a **named* instance of the `PrettyPrint` typeclass 
   * for the data type `Int` that renders the integer in a pretty way.
   */
  given intPrettyPrint as PrettyPrint[Int]:
    extension (i : Int) def prettyPrint: String = i.toString

  /**
   * EXERCISE 3
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `String`.
   */
  val stringPrettyPrint: PrettyPrint[String] = summon[PrettyPrint[String]]

  /**
   * EXERCISE 4
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `Int`.
   */
  val intPrettyPrint2: PrettyPrint[Int] = summon[PrettyPrint[Int]]

  /**
   * EXERCISE 5
   * 
   * With the help of the `using` keyword, create a method called `prettyPrintIt` that, for any type 
   * `A` for which a `PrettyPrint` instance exists, can both generate a pretty-print string, and 
   * print it out to the console using `println`.
   */
  def prettyPrintIt[A](self: A)(using PrettyPrint[A]) = println(self.prettyPrint)

  /**
   * EXERCISE 6
   * 
   * With the help of both `given` and `using`, create an instance of the `PrettyPrint` type class
   * for a generic `List[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given [A] (using PrettyPrint [A]) as PrettyPrint[List[A]] =
    l => l.map(_.prettyPrint).mkString(",")
//    extension (a: List[A]) def prettyPrint: String = a.map(_.prettyPrint).mkString(",")

  /**
   * EXERCISE 7
   * 
   * With the help of both `given` and `using`, create a **named** instance of the `PrettyPrint` 
   * type class for a generic `Vector[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given vectorPrettyPrint[A](using PrettyPrint[A]) as PrettyPrint[Vector[A]]:
    extension (v : Vector[A]) def prettyPrint: String = v.map(_.prettyPrint).mkString(",")

object given_scopes:
  trait Hash[-A]:
    extension (a: A) def hash: Int

  object HashGivens:
    given Hash[Int] = _.hashCode
    given Hash[Long] = _.hashCode
    given Hash[Float] = _.hashCode
    given Hash[Double] = _.hashCode

  /**
   * EXERCISE 1
   * 
   * Import the right given into the scope (but ONLY this given) so the following code will compile.
   */
    import HashGivens.given_Hash_Int
    12.hash 

  /**
   * EXERCISE 2
   * 
   * Import the right given into the scope (but ONLY this given) so the following code will compile.
   */
    import HashGivens.{ given Hash[Double] }
    12.123.hash   

  
object typeclass_derives:

  import scala.CanEqual

  /**
   * EXERCISE 1
   * 
   * Using the `derives` clause, derive an instance of the type class `Eql` for 
   * `Color`.
   */
  enum Color derives CanEqual:
    case Red 
    case Green 
    case Blue

/**
 * IMPLICIT CONVERSIONS
 * 
 * Scala 3 introduces a new type class called `Conversion` to perform "implicit 
 * conversions"--the act of automatically converting one type to another.
 */
object conversions:
  import scala.language.implicitConversions
  final case class Rational(n: Int, d: Int)

  /**
   * EXERCISE 1
   * 
   * Create an instance of the type class `Conversion` for the combination of types
   * `Rational` (from) and `Double` (to).
   */
  // given ...
  given Conversion[Rational, Double] = r => r.n.toDouble / r.d.toDouble

  /**
   * EXERCISE 2
   * 
   * Multiply a rational number by 2.0 (a double) to verify your automatic
   * conversion works as intended.
   */
  import scala.language.implicitConversions
  Rational(1, 2) * 2.0

object typeclass_graduation:
  /**
   * EXERCISE 1
   * 
   * Add cases to this enum for every primitive type in Scala.
   */
  enum PrimType[A]:
    case Int extends PrimType[Int]
    case Byte extends PrimType[Byte]
    case Char extends PrimType[Char]
    case Double extends PrimType[Double]
    case Float extends PrimType[Float]
    case Long extends PrimType[Long]
    case Short extends PrimType[Short]
    case Unit extends PrimType[Unit]
    case String extends PrimType[String]
  
  /**
   * EXERCISE 2
   * 
   * Add another case to `Data` to model enumerations, like `Either`.
   */
  enum Data:
    case Record(fields: Map[String, Data])
    case Primitive[A](primitive: A, primType: PrimType[A])
    case Collection(elements: Vector[Data])
    case Enumeration(tag: String, data: Data)

  /**
   * EXERCISE 3
   * 
   * Develop a type class called `EncodeData[A]`, that can encode an `A` into `Data`.
   */
  trait EncodeData[A]:
    extension (self: A) def encode: Data

  /**
   * EXERCISE 4
   * 
   * In the companion object of `Data`, write encoders for different primitive types in Scala,
   * including lists and collections.
   */
  object EncodeData:
    given EncodeData[Int] = Data.Primitive(_, PrimType.Int)
    given EncodeData[Byte] = Data.Primitive(_, PrimType.Byte)
    given EncodeData[Char] = Data.Primitive(_, PrimType.Char)
    given EncodeData[Double] = Data.Primitive(_, PrimType.Double)
    given EncodeData[Float] = Data.Primitive(_, PrimType.Float)
    given EncodeData[Long] = Data.Primitive(_, PrimType.Long)
    given EncodeData[Short] = Data.Primitive(_, PrimType.Short)
    given EncodeData[Unit] = Data.Primitive(_, PrimType.Unit)
    given EncodeData[String] = Data.Primitive(_, PrimType.String)

    given [A](using EncodeData[A]) as EncodeData[Vector[A]] =
      v => Data.Collection(v.map(_.encode))

    given [A](using EncodeData[A]) as EncodeData[List[A]] = 
      v => Data.Collection(v.toVector.map(_.encode))


  /**
   * EXERCISE 5
   * 
   * Create an instance of `EncodeData` for `Person`.
   */
  final case class Person(name: String, age: Int)
  object Person:
    import EncodeData.{ given EncodeData[String] }
    import EncodeData.{ given EncodeData[Int] }

    given EncodeData[Person] = p => Data.Record(Map("name" -> p.name.encode, "age" -> p.age.encode))

