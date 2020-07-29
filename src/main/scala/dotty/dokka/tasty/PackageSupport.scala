package dotty.dokka.tasty

import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.model.doc.DocumentationNode

import collection.JavaConverters._

trait PackageSupport:
    self: TastyParser =>
    import reflect._
    import self.SymbolOps.{documentation}
    def parsePackage(pck: PackageClause): DPackage = {
        val name = extractPackageName(pck.pid.show)
        val documentation = pck.symbol.documentation
        DPackage(
          new DRI(name, null, null, PointingToDeclaration.INSTANCE, null),
          Nil.asJava,
          Nil.asJava,
          Nil.asJava, // TODO add support for other things like type or package object entries
          Nil.asJava,
          documentation, // TODO find docs for package and search for package object to extract doc
          null,
          sourceSet.toSet,
          PropertyContainer.Companion.empty()
        )
    }

    def parsePackageObject(pckObj: ClassDef): DPackage = 
        DPackage(
          new DRI(pckObj.symbol.dri.getPackageName, null, null, PointingToDeclaration.INSTANCE, null),
          Nil.asJava,
          Nil.asJava,
          Nil.asJava, // TODO add support for other things like type or package object entries
          Nil.asJava,
          pckObj.symbol.documentation, // TODO find docs for package and search for package object to extract doc
          null,
          sourceSet.toSet,
          PropertyContainer.Companion.empty()
        )

    private def extractPackageName(pidShowNoColor: String): String = {
        val pidSplit = pidShowNoColor.split("\\.")
        pidSplit.mkString("",".","")
    }