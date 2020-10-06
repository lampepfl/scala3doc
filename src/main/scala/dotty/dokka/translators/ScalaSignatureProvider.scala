package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.properties.{WithExtraProperties}
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.base.signatures.KotlinSignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.utilities.DokkaLogger
import collection.JavaConverters._
import org.jetbrains.dokka.base.translators.documentables._
import org.jetbrains.dokka.model.properties.PropertyContainer
import dokka.java.api._
import java.util.function.Consumer
import kotlin.jvm.functions.Function2
import org.jetbrains.dokka.links.DRI

class ScalaSignatureProvider(contentConverter: CommentsToContentConverter, logger: DokkaLogger) extends SignatureProvider with ScalaSignatureUtils:
    private val default = new KotlinSignatureProvider(contentConverter, logger)
    private val styles = Set(TextStyle.Monospace).asInstanceOf[Set[Style]]
    private val contentBuilder = new ScalaPageContentBuilder(contentConverter, this, logger)

    private def signatureContent(d: Documentable)(
        func: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = contentBuilder.contentForDocumentable(d, kind = ContentKind.Symbol, styles = styles, buildBlock = func)


    case class ContentNodeBuilder(builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) extends SignatureBuilder{
        def text(str: String): SignatureBuilder = ContentNodeBuilder(builder.text(str))
        def driLink(text: String, dri: DRI): SignatureBuilder = ContentNodeBuilder(builder.driLink(text, dri))
        def group(styles: Any = "", kind: Any = "")(op: SignatureBuilder => SignatureBuilder): SignatureBuilder =
            this // TODO
    }

    override def signature(documentable: Documentable) = 
        JList(signatureContent(documentable){ builder => 
        val res = ScalaSignatureProvider.rawSignature(documentable, ContentNodeBuilder(builder))
        res.asInstanceOf[ContentNodeBuilder].builder 
    })

object ScalaSignatureProvider:     
    def rawSignature(documentable: Documentable, builder: SignatureBuilder): SignatureBuilder = 
        documentable match
            case extension: DFunction if extension.get(MethodExtension).extensionInfo.isDefined =>
                extensionSignature(extension, builder)
            case method: DFunction if method.get(IsGiven) != null =>
                givenMethodSignature(method, builder)
            case method: DFunction =>
                methodSignature(method, builder)
            case enumEntry: DClass if enumEntry.get(IsEnumEntry) != null => 
                enumEntrySignature(enumEntry, builder)
            case clazz: DClass =>
                classSignature(clazz, builder)
            case enumProperty: DProperty if enumProperty.get(IsEnumEntry) != null => 
                enumPropertySignature(enumProperty, builder)
            case property: DProperty =>
                propertySignature(property, builder)
            case parameter: DParameter =>
                parameterSignature(parameter, builder)
            case _ => 
                ???


    private def enumEntrySignature(entry: DClass, bdr: SignatureBuilder): SignatureBuilder =
        val ext = entry.get(ClasslikeExtension)
        val temp: SignatureBuilder = bdr
            .text("case ")
            .name(entry.getName, entry.getDri)
            .generics(entry)

        val temp2 = ext.constructor.toSeq.foldLeft(temp){ (bdr, elem) =>
            bdr.functionParameters(elem)
        }
        ext.parentTypes match{
            case Nil => temp2
            case extendType :: withTypes =>
                val temp3 = temp2
                    .text(" extends ")
                    .typeSignature(extendType)
                withTypes.foldLeft(temp3){ (bdr, tpe) =>
                    bdr.text(" with ").typeSignature(tpe)

                }
        }

    private def enumPropertySignature(entry: DProperty, builder: SignatureBuilder): SignatureBuilder = 
        val modifiedType = entry.getType match
            case t: TypeConstructor => TypeConstructor(
                t.getDri,
                t.getProjections.asScala.map{ 
                    case t: UnresolvedBound if t.getName == " & " => UnresolvedBound(" with "); 
                    case other => other
                }.asJava,
                t.getModifier
            )
            case other => other
        
        builder
            .text("case ")
            .name(entry.getName, entry.getDri)
            .text(" extends ")
            .typeSignature(modifiedType)


    private def classSignature(clazz: DClass, builder: SignatureBuilder): SignatureBuilder =
        val ext = clazz.get(ClasslikeExtension)
        val temp = builder
            .annotationsBlock(clazz)
            .modifiersAndVisibility(clazz, ext.kind.name)
            .name(clazz.getName, clazz.getDri)
            .generics(clazz)

        val temp2 = ext.constructor.toSeq.foldLeft(temp){ (bdr, elem) =>
            bdr.functionParameters(elem)
        }
        ext.parentTypes match
            case Nil => temp2
            case extendType :: withTypes =>
                val temp3 = temp2
                    .text(" extends ")
                    .typeSignature(extendType)
                withTypes.foldLeft(temp3){ (bdr, tpe) =>
                    bdr.text(" with ").typeSignature(tpe)
                }

    private def extensionSignature(extension: DFunction, builder: SignatureBuilder): SignatureBuilder =
        val grouped = extension.get(MethodExtension).extensionInfo.map(_.isGrouped).getOrElse(false)
        val extendedSymbol = if (extension.isRightAssociative()) {
            extension.getParameters.asScala(extension.get(MethodExtension).parametersListSizes(0))
        } else {
            extension.getParameters.asScala(0)
        }
        val bldr = builder.annotationsBlock(extension)
        val bdr = (
            if(!grouped){
            bldr
                .text("extension ")
                .text(s" (${extendedSymbol.getName}: ")
                .typeSignature(extendedSymbol.getType)
                .text(") ")
            } else bldr
        )
            .modifiersAndVisibility(extension, "def")
            .name(extension.getName, extension.getDri)
            .generics(extension)  
            .functionParameters(extension)
        if !extension.isConstructor then
            bdr
                .text(":")
                .text(" ")
                .typeSignature(extension.getType)
        else bdr

    private def givenMethodSignature(method: DFunction, builder: SignatureBuilder): SignatureBuilder =
        val bdr = builder.text("given ")
        method.get(IsGiven).givenInstance match {
            case Some(instance) => bdr
                .name(method.getName, method.getDri)
                .text(" as ")
                .typeSignature(instance)
            case None => bdr.typeSignature(method.getType)
        }
        

    private def methodSignature(method: DFunction, builder: SignatureBuilder): SignatureBuilder =
        val bdr = builder
        .annotationsBlock(method)
        .modifiersAndVisibility(method, "def")
        .name(method.getName, method.getDri)
        .generics(method)  
        .functionParameters(method)
        if !method.isConstructor then
            bdr
                .text(":")
                .text(" ")
                .typeSignature(method.getType)
        else bdr
        

    private def propertySignature(property: DProperty, builder: SignatureBuilder): SignatureBuilder =
        property.get(PropertyExtension).kind match
            case kind if property.get(IsGiven) != null => givenPropertySignature(property, builder)
            case "type" => typeSignature(property, builder)
            case other => fieldSignature(property, other, builder)

    private def typeSignature(typeDef: DProperty, builder: SignatureBuilder): SignatureBuilder =
        val modifiers = typeDef.getExtra.getMap.get(AdditionalModifiers.Companion).asInstanceOf[AdditionalModifiers]
        val isOpaque = modifiers != null && modifiers.getContent.defaultValue.asScala.contains(ScalaOnlyModifiers.Opaque)
        val bdr = builder
            .annotationsBlock(typeDef)
            .modifiersAndVisibility(typeDef, "type")
            .name(typeDef.getName, typeDef.getDri)
            .generics(typeDef)
        if(!isOpaque){
            (if !typeDef.get(PropertyExtension).isAbstract then bdr.text(" = ") else bdr)
                .typeSignature(typeDef.getType)
        } else bdr
        

    private def givenPropertySignature(property: DProperty, builder: SignatureBuilder): SignatureBuilder =
        val bdr = builder
            .text("given ")
            .name(property.getName, property.getDri)

        property.get(IsGiven).givenInstance match {
            case Some(instance) => bdr
                .text(" as ")
                .typeSignature(instance)
            case None => bdr
        }

    private def fieldSignature(property: DProperty, kind: String, builder: SignatureBuilder): SignatureBuilder =
        builder
            .annotationsBlock(property)
            .modifiersAndVisibility(property, kind)
            .name(property.getName, property.getDri)
            .text(":")
            .text(" ")
            .typeSignature(property.getType)

    private def parameterSignature(parameter: DParameter, builder: SignatureBuilder): SignatureBuilder =
        val ext = parameter.get(ParameterExtension)
        builder
            .text(if ext.isGrouped then "extension (" else "(")
            .text(parameter.getName)
            .text(": ")
            .typeSignature(parameter.getType)
            .text(")")
