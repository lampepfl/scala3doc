package tests

package implicitClasslikes

implicit object ImplicitObject
{
  implicit class ImplicitClass(val i: Int)
}

object A
{
  class B
  implicit object B
}