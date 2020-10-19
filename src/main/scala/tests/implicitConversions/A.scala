package tests.implicitConversions

implicit val convDtoC: Conversion[ClazzD, ClazzC]
    = ???

class ClazzA
{
    /* Methods inside this comment block should be added by implicit conversion
    val prop1: String

    def met1: String
    */
}

object ClazzA
{
    implicit def convToB1(a: ClazzA): ClazzB
        = ???
}

class ClazzAB extends ClazzA
{
    /* Methods inside this comment block should be added by implicit conversion
    val prop1: String

    def met1: String
    */

}

object ClazzAB
{

}

class ClazzD
{
    /* Methods inside this comment block should be added by implicit conversion
    val prop2: String

    def met2: String
    */
}

class ClazzF[T]
{
    /* Methods inside this comment block should be added by implicit conversion
    val prop3: String

    def met3: String
    */
}

object ClazzF
{
    implicit def convFtoE: Conversion[ClazzF[String], ClazzE]
        = ???
}