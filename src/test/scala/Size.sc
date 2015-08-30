import shapeless._
import shapeless.PolyDefns.~>


object choose extends (Set ~> Option) {
  def default[T](s: Set[T]) = s.headOption
  def apply[T](s : Set[T]) = s.headOption
}

val lo = List(Set(1, 3, 5), Set(2, 4, 6)).map(choose)


object Size extends Poly1{
  implicit def caseString  = at[String](x => x.length)
  implicit def caseInt = at[Int](x => 1)
  implicit def caseList[T] = at[List[T]](x=> x.size)
  implicit def caseTuple[T, U](implicit st : Case.Aux[T, Int], su : Case.Aux[U, Int]) =at[(T, U)](t => Size(t._1)+Size(t._2))
}

case class Person(name : String, password : String)

object ToJson extends  Poly1 {
  implicit def casePerson = at[Person](p => s"{ 'name' : '${p.name}', 'password' : '${p.password}'}")
}

val pString = ToJson(Person("Damien", "admin"))

val listString = List("Damien", "Mika", "Manue").map(Size)
val listInt = List(1, 2, 3).map(Size)
val listList = List(List(1,2,3,4)).map(Size)

val sizeOfString = Size("1234")
val sizeOfInt = Size(1234)
val sizeOfList = Size(1 :: 2 :: 3 :: 4 :: 5 :: Nil)
val sizeOfTuple = Size(("123456",1234))



