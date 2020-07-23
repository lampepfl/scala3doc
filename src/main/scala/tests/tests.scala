package tests

/** * AN IMPORTANT TEST CLASS
  *
  * This is an *important* test class.
  *
  * @author Gal Anonim
  * @version 1.0.0
  * @result A class doesn't actually have a result.
  */
class A
class B extends A
class C 
class D[T]
class E[T] extends D[T]



class Constructors(a: String):
    def this() = this("Ala")
    def this(a: A)(b: A) = this("Ala")

/** Some methods to tests */
class Methods:
 def nobraces: A = ???
 def simple(): B = ???
 def oneParam(a: A): B = ???
 def multipleParams(a: A, b: B): C = ???
 def vararg(a: A*): C = ???
 def multipleList(a: A)(b: B): C = ???

 def generic[T](a: D[T]): D[T] = ???
 def generic2[T, V](a: D[T], b: E[V]): D[T] = ???

 def primitives(a: Int, b: Double, c: Short): Byte = 0
 def strings(a: String): String = ""
 def arrays(a: Array[String], b: Array[Int]): Array[Double] = ???
