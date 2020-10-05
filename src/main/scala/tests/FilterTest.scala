package tests


class FilterTestBase:
    sealed abstract class BInherited
    abstract case class CInherited(s: String)

    sealed case class DInherited(c: String)

    final case class EInherited(c: String)

    private class PrivateInherited
    protected class ProtectedInherited

    protected def protectetDefInherited(a: Int): String = ???
    private def privateDefInherited(a: Int): String = ???
    def publicDefInherited(a: Int): String = ???
    

    protected val protectetValInherited = 123
    private val privateValInherited = 344
    val publicValInherited = 567
    
    protected type protectedTypeInherited = 123
    private type privateTypeInherited = 344
    type publicTypeInherited = 567
  
    protected given Set[String | Int] = Set(1, "ala")
    given Map[String, Double] = Map.empty

    protected given namedSet as Set[String | Int] = Set(1, "ala")
    given namedMap as Map[String, Double] = Map.empty

class FilterTest extends FilterTestBase:

    sealed abstract class B
    abstract case class C(s: String)

    sealed case class D(c: String)

    final case class E(c: String)

    private class Private
    protected class Protected

    protected def protectetDef(a: Int): String = ???
    private def privateDef(a: Int): String = ???
    def publicDef(a: Int): String = ???
    

    protected val protectetVal = 123
    private val privateVal= 344
    val publicVal = 567
    
    protected type protectedType = 123
    private type privateType= 344
    type publicType = 567
    
    protected given Seq[String | Int | Double] = List(1)
    given List[String] = "ula" :: Nil
    
    given namedList as List[String] = "ula" :: Nil
    protected given namedSeq as Seq[String | Int | Double] = List(1)
