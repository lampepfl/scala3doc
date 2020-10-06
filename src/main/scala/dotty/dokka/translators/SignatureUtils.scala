package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.base.translators.documentables.PageContentBuilder
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.properties.WithExtraProperties
import org.jetbrains.dokka.pages._
import collection.JavaConverters._

object SignatureUtils:
    private val ignoreRules: List[(AnnotationsInfo.Annotation) => Boolean] = List(
        a => a.dri.getPackageName.startsWith("scala.annotation.internal")
    )

    extension (tokens: Seq[String]) def toSignatureString(): String =
        tokens.filter(_.trim.nonEmpty).mkString(""," "," ")

    extension [T <: Documentable] (d: T) def annotations() = (d match {
        case e: WithExtraProperties[T] => e.get(AnnotationsInfo).annotations
        case _ => List.empty
    }).filterNot(annotation => ignoreRules.exists(ignoreFun => ignoreFun(annotation)))

    // TODO
    // def annotationsBlock(d: Documentable): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = builder
    //     .group(styles = Set(TextStyle.Block), kind = ContentKind.Annotations){ bdr => 
    //         d.annotations().foldLeft(bdr){ (bdr, annotation) => bdr
    //             .buildAnnotation(annotation)
    //         }
    //     }
    
    // TODO
    // def annotationsInline(d: Documentable): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = builder
    //     .group(styles = Set(TextStyle.Span), kind = ContentKind.Annotations){ bdr => 
    //         d.annotations().foldLeft(bdr){ (bdr, annotation) => bdr
    //             .buildAnnotation(annotation)
    //         }
    //     }

    // TODO
    // private def buildAnnotation(a: AnnotationsInfo.Annotation): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = builder
    //     .group(){ bdr => bdr
    //         .text("@")
    //         .driLink(a.dri.getClassNames, a.dri)
    //         .buildAnnotationParams(a)
    //         .text(" ")
    //     }


    // private def buildAnnotationParams(a: AnnotationsInfo.Annotation): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = 
    //     if !a.params.isEmpty then builder
    //         .group(styles = Set(TextStyle.BreakableAfter)){ bdr => bdr
    //             .list(a.params, "(", ")", ", "){ (bdr, param) => bdr
    //                 .buildAnnotationParameter(param)
    //             }
    //         }
    //     else builder

    // private def addParameterName(txt: Option[String]): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = txt match {
    //         case Some(name) => builder.text(s"$txt = ")
    //         case _ => builder
    //     }

    // private def buildAnnotationParameter(a: AnnotationsInfo.AnnotationParameter): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = a match {
    //     case AnnotationsInfo.PrimitiveParameter(name, value) => builder
    //         .addParameterName(name)
    //         .text(value)
    //     case AnnotationsInfo.LinkParameter(name, dri, text) => builder
    //         .addParameterName(name)
    //         .driLink(text, dri)
    //     case AnnotationsInfo.UnresolvedParameter(name, value) => builder
    //         .addParameterName(name)
    //         .text(value)
    // }

    def modifiersAndVisibility[T](t: WithAbstraction with WithVisibility with WithExtraProperties[T]): String =
        import org.jetbrains.dokka.model.properties._
        val extras = t.getExtra.getMap()
        val additionalModifiers =
          Option(extras.get(AdditionalModifiers.Companion).asInstanceOf[AdditionalModifiers])
            .map(_.getContent)
            .map(content => content.defaultValue.asScala.collect{case s: ScalaOnlyModifiers => s})
            
        val prefixes = additionalModifiers
            .map(_.filter(_.prefix).map(_.getName))
            .map(modifiers => modifiers.toSeq.toSignatureString())
            .getOrElse("")

        val visibilityModifier =
            val visibility = t.getVisibility.defaultValue.asInstanceOf[ScalaVisibility]
            visibilityToString(visibility)
        
        val suffixes = additionalModifiers
            .map(_.filter(!_.prefix).map(_.getName))
            .map(modifiers => modifiers.toSeq.toSignatureString())
            .getOrElse("")

        Seq(
            prefixes.trim,
            visibilityModifier, 
            t.getModifier.defaultValue.getName,
            suffixes.trim
        ).toSignatureString()
            
    private def visibilityToString(visibility: ScalaVisibility) = visibility match
      case ScalaVisibility.Unrestricted => ""
      case ScalaVisibility.Protected(scope) => s"protected${visibilityScopeToString(scope)}"
      case ScalaVisibility.Private(scope) => s"private${visibilityScopeToString(scope)}"

    private def visibilityScopeToString(scope: VisibilityScope) = 
      import VisibilityScope._
      scope match
            case ImplicitTypeScope | ImplicitModuleScope => ""
            case ExplicitTypeScope(name) => s"[$name]"
            case ExplicitModuleScope(name) => s"[$name]"
            case ThisScope => "[this]"


  //   type TypeStr = String | (String, DRI)

  //   def typeSignature(b: Projection): Seq[TypeStr] = b match {
  //       case tc: TypeConstructor =>
  //           tc.getProjections.asScala.toSeq.map {
  //               case text: UnresolvedBound => text.getName
  //               case link: TypeParameter => 
  //                   (link.getName, link.getDri)
  //               case other =>
  //                   s"TODO($other)"
  //           }
  //       case other =>
  //           Seq(s"TODO: $other")
  //   }

  //   private def list[T](
  //           elements: Seq[T],
  //           prefix: String = "",
  //           suffix: String = "",
  //           separator: String = ", "
  //   )(op: T => Seq[TypeStr]): Seq[TypeStr] =
  //     elements match 
  //       case Nil => Nil
  //       case head +: tail => 
  //         Seq(prefix) ++ op(head) ++ tail.flatMap(g => Seq(separator) ++ op(g)) ++ Seq(suffix)

  //   def generics(on: WithGenerics): Seq[TypeStr] =
  //     list(on.getGenerics.asScala.toSeq, "[", "]")(_.getBounds.asScala.flatMap(typeSignature))

    
  //   def functionParameters(method: DFunction) = 
  //       val methodExtension = method.get(MethodExtension)
  //       val receiverPos = if method.isRightAssociative() then method.get(MethodExtension).parametersListSizes(0) else 0
  //       val paramLists = methodExtension.parametersListSizes
  //       val (bldr, index) = paramLists.foldLeft((Seq[TypeStr](), 0)){
  //           case ((builder, from), size) =>
  //               val toIndex = from + size
  //               if from == toIndex then (builder ++ Seq("()"), toIndex)
  //               else if !methodExtension.extensionInfo.isDefined || from != receiverPos then
  //               val b = list(method.getParameters.subList(from, toIndex).asScala.toList, "(", ")"){ param =>
  //                       // RODO.annotationsInline(param)
  //                       param.getName :: ": " :: typeSignature(param.getType)
  //                   }
  //                   (builder ++ b, toIndex)
  //               else (builder, toIndex)
  //       }
  //       bldr

          



    
    