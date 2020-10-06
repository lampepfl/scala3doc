package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka._
import scalatags.Text.all._
import scalatags.Text.tags2.{title, main, nav}
import scalatags.Text.TypedTag
import collection.JavaConverters._
import com.virtuslab.dokka.site.SiteRenderer
import com.virtuslab.dokka.site.BaseStaticSiteProcessor
import java.net.URI
import java.util.{List => JList, Set => JSet}
import kotlinx.html.FlowContent
import kotlinx.html.stream.StreamKt
import kotlinx.html.Gen_consumer_tagsKt

class ScalaHtmlRenderer(ctx: DokkaContext) extends SiteRenderer(ctx) {

    type FlowContentConsumer = kotlin.jvm.functions.Function1[? >: kotlinx.html.FlowContent, kotlin.Unit]

    override def buildTable(f: FlowContent, node: ContentTable, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) = {
        if(node.getStyle.asScala.toSet.contains(TableStyle.DescriptionList)) withHtml(f, buildDescriptionList(node, pageContext, sourceSetRestriciton))
        else super.buildTable(f, node, pageContext, sourceSetRestriciton)
    }

    override def wrapGroup(f: FlowContent, node: ContentGroup, pageContext: ContentPage, childrenCallback: FlowContentConsumer) = {
        val additionalClasses = node.getStyle.asScala.map(_.toString.toLowerCase).mkString("", ",", "")
        def buildSymbol: String = div(cls := s"symbol $additionalClasses")(
            raw(
                buildWithKotlinx(childrenCallback).toString
            )
        ).toString
        if node.getDci.getKind == ContentKind.Symbol && node.getStyle.asScala.toSet.contains(TextStyle.Monospace) then withHtml(f, buildSymbol) else super.wrapGroup(f, node, pageContext, childrenCallback)
    }

    override def buildContentNode(f: FlowContent, node: ContentNode, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) = {
        node match {
            case n: HtmlContentNode => withHtml(f, raw(n.body).toString)
            case n: DocumentableList => withHtml(f, buildDocumentableList(n).toString())
            case n: DocumentableFilter => withHtml(f, div(`class` := "documentableFilter")("filter instance here").toString)
            case other => super.buildContentNode(f, node, pageContext, sourceSetRestriciton)
        }
    }

    private val clazz = `class`

    private val anchor = raw("""
        <svg width="20" height="20" viewBox="0 0 20 20" fill="darkgray" xmlns="http://www.w3.org/2000/svg">
            <path d="M21.2496 5.3C20.3496 4.5 19.2496 4 18.0496 4C16.8496 4 15.6496 4.5 14.8496 5.3L10.3496 9.8L11.7496 11.2L16.2496 6.7C17.2496 5.7 18.8496 5.7 19.8496 6.7C20.8496 7.7 20.8496 9.3 19.8496 10.3L15.3496 14.8L16.7496 16.2L21.2496 11.7C22.1496 10.8 22.5496 9.7 22.5496 8.5C22.5496 7.3 22.1496 6.2 21.2496 5.3Z"></path>
            <path d="M8.35 16.7998C7.35 17.7998 5.75 17.7998 4.75 16.7998C3.75 15.7998 3.75 14.1998 4.75 13.1998L9.25 8.6998L7.85 7.2998L3.35 11.7998C1.55 13.5998 1.55 16.3998 3.35 18.1998C4.25 19.0998 5.35 19.4998 6.55 19.4998C7.75 19.4998 8.85 19.0998 9.75 18.1998L14.25 13.6998L12.85 12.2998L8.35 16.7998Z"></path>
        </svg>
    """)

    private def buildDocumentableList(n: DocumentableList) = div(clazz:="documentableList")(
        n.name.fold(raw(""))(h3(clazz := "documentableHeader")(_)),
        n.elements.map { element =>
            def topLevelAttr = Seq(clazz := "documentableElement") ++ Seq(attr("data-ala") := "ola")
            val pageAddress = "TODO"
            div(topLevelAttr)(
                div(clazz := "symbol monospace")(  
                    a(href:=pageAddress, clazz := "documentableAnchor")(anchor),
                    span(clazz := "all_modifiers")(
                        span(clazz := "modifiers")(element.modifiers),
                        span(clazz := "kind")(" ", element.kind, " ")
                    ),
                    a(clazz := "symbol", href := pageAddress)(element.name),
                    span(clazz := "signature")(element.signature)
                ),
                div(clazz := "brief")(raw("TODO use dokka"))
            )
        }
    )

    def buildDescriptionList(node: ContentTable, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) = {
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

    override def buildCodeBlock(
        f: FlowContent,
        code: ContentCodeBlock,
        pageContext: ContentPage,
    ): Unit = {
        // we cannot use Scalatags, because we need to call buildContentNode
        import kotlinx.html.{Gen_consumer_tagsKt => dsl}
        val c = f.getConsumer
        val U = kotlin.Unit.INSTANCE

        dsl.div(c, "sample-container", { e =>
            dsl.pre(c, null, { e =>
                val codeClass = code.getStyle.asScala.iterator.map(_.toString.toLowerCase).mkString("", " ", " language-scala")
                dsl.code(c, codeClass, { e =>
                    e.getAttributes.put("theme", "idea")
                    code.getChildren.asScala.foreach(buildContentNode(f, _, pageContext, /*sourceSetRestriction*/ null))
                    U
                })
                U
            })
            U
        })
    }

    override def buildHtml(page: PageNode, resources: JList[String], kotlinxContent: FlowContentConsumer): String =
        val (pageTitle, pageResources, fromTemplate) = page match
            case static: BaseStaticSiteProcessor.StaticPageNode =>
                val res = if static.hasFrame then resources else static.resources
                val title = static.getLoadedTemplate.getTemplateFile.title
                (title, res, !static.hasFrame)
            case _ =>
                (page.getName, resources, false)
        html(
            head(
                meta(charset := "utf-8"),
                meta(name := "viewport", content := "width=device-width, initial-scale=1"),
                title(pageTitle),
                linkResources(page, pageResources.asScala).toSeq,
                script(raw(s"""var pathToRoot = "${getLocationProvider.pathToRoot(page)}";"""))
            ),
            body(
                if fromTemplate then
                    raw(buildWithKotlinx(kotlinxContent))
                else
                    div(
                        id := "container",
                        div(
                            id := "leftColumn",
                            div(id := "logo"),
                            div(id := "paneSearch"),
                            nav(id := "sideMenu"),
                        ),
                        div(
                            id := "main",
                            div (
                                id := "leftToggler",
                                span(cls := "icon-toggler")
                            ),
                            div(id := "searchBar"),
                            main(
                                raw(buildWithKotlinx(kotlinxContent))
                            ),
                            footer(
                                span(cls := "go-to-top-icon")(
                                    a(href := "#container")(
                                        span(cls:="icon-vertical_align_top"),
                                        raw("&nbsp;Back to top")
                                    )
                                ),
                                span(cls := "pull-right")(
                                    raw("Generated by&nbsp;"),
                                    a(href := "https://github.com/lampepfl/scala3doc")("Scala3doc")
                                )
                            )
                        )
                    ),
                    script(`type` := "text/javascript", src := resolveRoot(page, "scripts/pages.js")),
                    script(`type` := "text/javascript", src := resolveRoot(page, "scripts/main.js"))
            )
        ).toString

    private def resolveRoot(page: PageNode, path: String) =
        getLocationProvider.pathToRoot(page) + path

    private def linkResources(page: PageNode, resources: Iterable[String]): Iterable[Frag] =
        def fileExtension(url: String): String =
            val param = url.indexOf('?')
            val end = if param < 0 then url.length else param
            val point = url.lastIndexOf('.', end)
            url.substring(point+1, end)

        def resolveLink(url: String): String =
            if URI(url).isAbsolute then url else resolveRoot(page, url)

        for res <- resources yield
            fileExtension(res) match
                case "css" => link(rel := "stylesheet", href := resolveLink(res))
                case "js" => script(`type` := "text/javascript", src := resolveLink(res), defer)
                case _ => raw(res)

    private def buildWithKotlinx(node: ContentNode, pageContext: ContentPage, sourceSetRestriction: JSet[DisplaySourceSet]): String =
        Gen_consumer_tagsKt.div(
            StreamKt.createHTML(true, false),
            null,
            (div) => {build(node, div, pageContext, sourceSetRestriction); kotlin.Unit.INSTANCE}
        ).toString.stripPrefix("<div>").stripSuffix("</div>\n")

    private def buildWithKotlinx(func: FlowContentConsumer): String =
        Gen_consumer_tagsKt.div(
            StreamKt.createHTML(true, false),
            null,
            func
        ).toString.stripPrefix("<div>").stripSuffix("</div>\n")

}
