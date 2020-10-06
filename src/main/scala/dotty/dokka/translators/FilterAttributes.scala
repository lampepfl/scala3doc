package dotty.dokka.translators

import org.jetbrains.dokka.base.translators.documentables.{DefaultPageCreator, PageContentBuilder, PageContentBuilder$DocumentableContentBuilder}
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.utilities.DokkaLogger
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.doc._

import dotty.dokka._

object FilterAttributes:
  def attributesFor(documentable: Documentable): Map[String, String] = 
    (visibity(documentable) ++ isAbstract(documentable) ++ origin(documentable)).filter(_._2.nonEmpty)

  private def visibity(documentable: Documentable): Map[String, String] = documentable match
    case v: WithVisibility => 
      val name = v.getVisibility.defaultValue match
        case v: ScalaVisibility => v.name
        case _ => "unknown"
      Map("visibility" -> name)
    case _ => 
      Map.empty
  
  private def isAbstract(documentable: Documentable): Map[String, String] = documentable match 
    case m: WithAbstraction =>
      val name = m.getModifier.defaultValue match 
        case v: ScalaModifier => v.name
        case _ => "unknown"
      
      Map("overridability" -> name)
    case _ => 
      Map.empty  


  private def origin(documentable: Documentable): Map[String, String] = 
    OriginInfo.getFrom(documentable).fold(Map.empty){
      case OriginInfo.InheritedFrom(name, _) => Map("inherited" -> name)
      case OriginInfo.ImplicitlyAddedBy(name: String, _) => Map("implicitly" -> s"by $name")
      case OriginInfo.ExtensionFrom(name: String, _) => Map("extension" -> s"from $name")
    }
