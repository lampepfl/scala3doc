package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka._
import scalatags.Text.all._
import scalatags.Text.TypedTag
import collection.JavaConverters._
import com.virtuslab.dokka.site.SiteRenderer
import java.net.URI
import kotlinx.html.FlowContent
import kotlinx.html.stream.StreamKt
import kotlinx.html.Gen_consumer_tagsKt


class ScalaHtmlRenderer(ctx: DokkaContext) extends SiteRenderer(ctx):
    override def buildTable(f: FlowContent, node: ContentTable, pageContext: ContentPage, sourceSetRestriciton: java.util.Set[DisplaySourceSet]) = {
        if(node.getStyle.asScala.toSet.contains(TableStyle.DescriptionList)) withHtml(f, buildDescriptionList(node, pageContext, sourceSetRestriciton))
        else super.buildTable(f, node, pageContext, sourceSetRestriciton)
    }

    override def wrapGroup(f: FlowContent, node: ContentGroup, pageContext: ContentPage, childrenCallback: kotlin.jvm.functions.Function1[? >: kotlinx.html.FlowContent, kotlin.Unit]) = {
        val additionalClasses = node.getStyle.asScala.map(_.toString.toLowerCase).mkString("", ",", "")
        def buildSymbol: String = div(cls := s"symbol $additionalClasses")(
            raw(
                buildWithKotlinx(childrenCallback).toString
            )
        ).toString
        if node.getDci.getKind == ContentKind.Symbol && node.getStyle.asScala.toSet.contains(TextStyle.Monospace) then withHtml(f, buildSymbol) else super.wrapGroup(f, node, pageContext, childrenCallback)
    }

    override def buildContentNode(f: FlowContent, node: ContentNode, pageContext: ContentPage, sourceSetRestriciton: java.util.Set[DisplaySourceSet]) = {
        node match {
            case n: HtmlContentNode => withHtml(f, raw(n.body).toString)
            case other => super.buildContentNode(f, node, pageContext, sourceSetRestriciton)
        }
    }

    def buildDescriptionList(node: ContentTable, pageContext: ContentPage, sourceSetRestriciton: java.util.Set[DisplaySourceSet]) = {
        val children = node.getChildren.asScala.toList.zipWithIndex
        dl(cls := "attributes")(
            children.map((e, i) => 
                if(i % 2 == 0) 
                    dt(
                        raw(
                            buildWithKotlinx(e, pageContext, sourceSetRestriciton)
                        )
                    )
                else 
                    dd(
                        raw(
                            buildWithKotlinx(e, pageContext, sourceSetRestriciton)
                        )
                    )
            )
        ).toString
    }

    private def buildWithKotlinx(node: ContentNode, pageContext: ContentPage, sourceSetRestriciton: java.util.Set[DisplaySourceSet]): String = {
        val res = Gen_consumer_tagsKt.div(
            StreamKt.createHTML(true, false),
            null, 
            (div) => {build(node, div, pageContext, sourceSetRestriciton); kotlin.Unit.INSTANCE}
        ).toString.stripPrefix("<div>").stripSuffix("</div>\n")
        res
    }

    private def buildWithKotlinx(func: kotlin.jvm.functions.Function1[? >: kotlinx.html.FlowContent, kotlin.Unit]) = {
        val res = Gen_consumer_tagsKt.div(
            StreamKt.createHTML(true, false),
            null, 
            func
        ).toString.stripPrefix("<div>").stripSuffix("</div>\n")
        res
    }
    

    //open fun buildHtml(page: PageNode, resources: List<String>, content: FlowContent.() -> Unit) =
    type KF[A, N] = kotlin.jvm.functions.Function1[A, N]
    override def buildHtml(page: PageNode, resources: java.util.List[String], content: KF[_ >: FlowContent,kotlin.Unit]) =
        super.buildHtml(page, resources, { (c: FlowContent) =>
            val res = content.invoke(c)
            withHtml(c, """
            <!-- Embedding Script -->
            <script src="https://scastie.scala-lang.org/embedded.js"></script>

            <script>
            window.addEventListener('load', function() {
                scastie.Embedded('.language-scala', { targetType: 'dotty', isWorksheetMode: false });
            });
            </script>
            """)
            res
        })