package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.base.translators.documentables.PageContentBuilder
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.properties.WithExtraProperties
import org.jetbrains.dokka.pages._
import collection.JavaConverters._

type InlineContent = String | (String, DRI)

case class InlineSignatureBuilder(names: Seq[InlineContent] = Nil, preName: Seq[InlineContent] = Nil) extends SignatureBuilder:
    override def text(str: String): SignatureBuilder = copy(names = str +: names)
    override def name(str: String, dri: DRI): SignatureBuilder = copy(names = Nil, preName = names)
    override def driLink(text: String, dri: DRI): SignatureBuilder = copy(names = (text, dri) +: names)
    override def group(styles: Any = "", kind: Any = "")(op: SignatureBuilder => SignatureBuilder): SignatureBuilder = op(this)

trait SignatureBuilder extends ScalaSignatureUtils {
    def text(str: String): SignatureBuilder
    def name(str: String, dri: DRI) = driLink(str, dri)
    def driLink(text: String, dri: DRI): SignatureBuilder
    def group(styles: Any = "", kind: Any = "")(op: SignatureBuilder => SignatureBuilder): SignatureBuilder

     def list[E](
            elements: List[E],
            prefix: String = "",
            suffix: String = "",
            separator: String = ", ",
        )(
            elemOp: (SignatureBuilder, E) => SignatureBuilder
        ): SignatureBuilder = elements match {
            case Nil => this
            case head +: tail =>
                tail.foldLeft(elemOp(text(prefix), head))((b, e) => elemOp(b.text(separator), e)).text(suffix)
        }

    def annotationsBlock(d: Documentable): SignatureBuilder = 
            group(styles = Set(TextStyle.Block), kind = ContentKind.Annotations){ bdr => 
                d.annotations().foldLeft(bdr){ (bdr, annotation) => bdr
                    .buildAnnotation(annotation)
                }
            }
        
        def annotationsInline(d: Documentable): SignatureBuilder =
            group(styles = Set(TextStyle.Span), kind = ContentKind.Annotations){ bdr => 
                d.annotations().foldLeft(bdr){ (bdr, annotation) => bdr
                    .buildAnnotation(annotation)
                }
            }

        private def buildAnnotation(a: AnnotationsInfo.Annotation): SignatureBuilder = 
            group(){ bdr => bdr
                .text("@")
                .driLink(a.dri.getClassNames, a.dri)
                .buildAnnotationParams(a)
                .text(" ")
            }


        private def buildAnnotationParams(a: AnnotationsInfo.Annotation): SignatureBuilder = 
            if !a.params.isEmpty then 
                group(styles = Set(TextStyle.BreakableAfter)){ bdr => bdr
                    .list(a.params, "(", ")", ", "){ (bdr, param) => bdr
                        .buildAnnotationParameter(param)
                    }
                }
            else this

        private def addParameterName(txt: Option[String]): SignatureBuilder = txt match {
                case Some(name) => this.text(s"$txt = ")
                case _ => this
            }

        private def buildAnnotationParameter(a: AnnotationsInfo.AnnotationParameter): SignatureBuilder = a match {
            case AnnotationsInfo.PrimitiveParameter(name, value) => 
                addParameterName(name).text(value)
            case AnnotationsInfo.LinkParameter(name, dri, text) => 
                addParameterName(name).driLink(text, dri)
            case AnnotationsInfo.UnresolvedParameter(name, value) => 
                addParameterName(name).text(value)
        }

        def modifiersAndVisibility(t: WithAbstraction with WithVisibility with WithExtraProperties[_], kind: String) =
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


            text(
                    Seq(
                        prefixes.trim,
                        visibilityModifier, 
                        t.getModifier.defaultValue.getName,
                        suffixes.trim,
                        kind
                    ).toSignatureString()
                )

        def typeSignature(b: Projection): SignatureBuilder = b match {
            case tc: TypeConstructor =>
                tc.getProjections.asScala.foldLeft(this) { (bdr, elem) => elem match {
                    case text: UnresolvedBound => bdr.text(text.getName)
                    case link: TypeParameter => 
                        bdr.driLink(link.getName, link.getDri)
                    case other =>
                        bdr.text(s"TODO($other)")
                }
            }
            case other =>
                text(s"TODO: $other")
        }

        def generics(on: WithGenerics) = list(on.getGenerics.asScala.toList, "[", "]"){ (bdr, e) => 
            val bldr = bdr.text(e.getName)
            e.getBounds.asScala.foldLeft(bldr)( (b, bound) => b.typeSignature(bound))
        }
        
        def functionParameters(method: DFunction) = 
            val methodExtension = method.get(MethodExtension)
            val receiverPos = if method.isRightAssociative() then method.get(MethodExtension).parametersListSizes(0) else 0
            val paramLists = methodExtension.parametersListSizes
            val (bldr, index) = paramLists.foldLeft(this, 0){
                case ((builder, from), size) =>
                    val toIndex = from + size
                    if from == toIndex then (builder.text("()"), toIndex)
                    else if !methodExtension.extensionInfo.isDefined || from != receiverPos then
                        val b = builder.list(method.getParameters.subList(from, toIndex).asScala.toList, "(", ")"){ (bdr, param) => bdr
                            .annotationsInline(param)
                            .text(param.getName)
                            .text(": ")
                            .typeSignature(param.getType)
                        }
                        (b, toIndex)
                    else (builder, toIndex)
            }
            bldr
}

trait ScalaSignatureUtils:
    val ignoreRules: List[(AnnotationsInfo.Annotation) => Boolean] = List(
        a => a.dri.getPackageName.startsWith("scala.annotation.internal")
    )

    extension (tokens: Seq[String]) def toSignatureString(): String =
        tokens.filter(_.trim.nonEmpty).mkString(""," "," ")

    extension [T <: Documentable] (d: T) def annotations() = (d match {
        case e: WithExtraProperties[T] => e.get(AnnotationsInfo).annotations
        case _ => List.empty
    }).filterNot(annotation => ignoreRules.exists(ignoreFun => ignoreFun(annotation)))
        

            
    def visibilityToString(visibility: ScalaVisibility) = visibility match {
        case ScalaVisibility.Unrestricted => ""
        case ScalaVisibility.Protected(scope) => s"protected${visibilityScopeToString(scope)}"
        case ScalaVisibility.Private(scope) => s"private${visibilityScopeToString(scope)}"
    }

    def visibilityScopeToString(scope: VisibilityScope) = 
        import VisibilityScope._

        scope match
            case ImplicitTypeScope | ImplicitModuleScope => ""
            case ExplicitTypeScope(name) => s"[$name]"
            case ExplicitModuleScope(name) => s"[$name]"
            case ThisScope => "[this]"

    