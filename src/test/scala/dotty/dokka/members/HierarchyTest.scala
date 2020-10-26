package dotty.dokka.members 

import dotty.dokka.ScaladocTest
import dotty.dokka.Assertion.AfterDocumentablesTransformation
import dotty.dokka.kUnit
import dotty.dokka.model.api._
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}

class HierarchyTest extends ScaladocTest("example/hierarchy"):
    override def assertions = Seq(
        AfterDocumentablesTransformation { m =>
            m.visitMembers { x =>
                if (x.getName == "C1") {
                    assertEquals(List("A1", "A2[Int]", "A3[Int, String]", "Any", "B1", "B2", "B3", "Object"), x.getParentsAsStrings)
                    assertEquals(List("B1", "B2", "B3"), x.getDirectParentsAsStrings)
                    assertEquals(List("E1", "E2"), x.getKnownChildrenAsStrings)
                }
                if (x.getName == "E2") {
                    assertEquals(List("A1", "A2[Int]", "A3[Int, String]", "Any", "B1", "B2", "B3", "C1[Int, Boolean, Any]", "D2[Int, Boolean]", "D3", "Object"), x.getParentsAsStrings)
                    assertEquals(List("C1[Int, Boolean, Any]", "D2[Int, Boolean]", "D3"), x.getDirectParentsAsStrings)
                    assertEquals(List.empty, x.getKnownChildrenAsStrings)
                }
                if (x.getName == "A2") {
                    assertEquals(List("Any", "Object"), x.getParentsAsStrings)
                    assertEquals(List.empty, x.getDirectParentsAsStrings)
                    assertEquals(List("B2", "B3", "C1[A, B, C]", "E1", "E2"), x.getKnownChildrenAsStrings)
                }
            }
        }
    )