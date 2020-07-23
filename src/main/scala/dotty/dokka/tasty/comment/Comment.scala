package dotty.dokka.tasty.comment

import com.vladsch.flexmark.util.ast.{ Node => MarkdownNode }
import com.vladsch.flexmark.util.ast.NodeIterator
// import HtmlParsers._
// import util.MemberLookup
// import representations._

import com.vladsch.flexmark.formatter.Formatter;
import com.vladsch.flexmark.util.options.MutableDataSet;

type Packages = Any /* Map[String, EmulatedPackageRepresentation] */
type Repr = Any

case class Comment (
  body:                    String,
  short:                   String,
  authors:                 List[String],
  see:                     List[String],
  result:                  Option[String],
  throws:                  Map[String, String],
  valueParams:             Map[String, String],
  typeParams:              Map[String, String],
  version:                 Option[String],
  since:                   Option[String],
  todo:                    List[String],
  deprecated:              Option[String],
  note:                    List[String],
  example:                 List[String],
  constructor:             Option[String],
  group:                   Option[String],
  groupDesc:               Map[String, String],
  groupNames:              Map[String, String],
  groupPrio:               Map[String, String],
  /** List of conversions to hide - containing e.g: `scala.Predef.FloatArrayOps` */
  hideImplicitConversions: List[String]
)

private[comment] case class ParsedComment (
  body:                    String,
  authors:                 List[String],
  see:                     List[String],
  result:                  List[String],
  throws:                  Map[String, String],
  valueParams:             Map[String, String],
  typeParams:              Map[String, String],
  version:                 List[String],
  since:                   List[String],
  todo:                    List[String],
  deprecated:              List[String],
  note:                    List[String],
  example:                 List[String],
  constructor:             List[String],
  group:                   List[String],
  groupDesc:               Map[String, String],
  groupNames:              Map[String, String],
  groupPrio:               Map[String, String],
  hideImplicitConversions: List[String],
  shortDescription:        List[String]
)

trait MarkupConversion[T](
  packages: Packages
) extends MemberLookup {
  def ent: Repr
  def parsed: ParsedComment

  protected def linkedExceptions(m: Map[String, String]): Map[String, String]
  protected def stringToMarkup(str: String): T
  protected def markupToMarkdown(t: T): String
  protected def stringToShortMarkdown(str: String): String
  protected def filterEmpty(xs: List[String]): List[T]
  protected def filterEmpty(xs: Map[String, String]): Map[String, T]

  private def single(annot: String, xs: List[String], filter: Boolean = true): Option[T] =
    (if (filter) filterEmpty(xs) else xs.map(stringToMarkup)) match {
      case x :: xs =>
        Some(x)
      case _ => None
    }

  final def comment: Comment = Comment(
    body                    = markupToMarkdown(stringToMarkup(parsed.body)),
    short                   = stringToShortMarkdown(parsed.body),
    authors                 = filterEmpty(parsed.authors).map(markupToMarkdown),
    see                     = filterEmpty(parsed.see).map(markupToMarkdown),
    result                  = single("@result", parsed.result).map(markupToMarkdown),
    throws                  = linkedExceptions(parsed.throws),
    valueParams             = filterEmpty(parsed.valueParams).view.mapValues(markupToMarkdown).toMap,
    typeParams              = filterEmpty(parsed.typeParams).view.mapValues(markupToMarkdown).toMap,
    version                 = single("@version", parsed.version).map(markupToMarkdown),
    since                   = single("@since", parsed.since).map(markupToMarkdown),
    todo                    = filterEmpty(parsed.todo).map(markupToMarkdown),
    deprecated              = single("@deprecated", parsed.deprecated, filter = false).map(markupToMarkdown),
    note                    = filterEmpty(parsed.note).map(markupToMarkdown),
    example                 = filterEmpty(parsed.example).map(markupToMarkdown),
    constructor             = single("@constructor", parsed.constructor).map(markupToMarkdown),
    group                   = single("@group", parsed.group).map(markupToMarkdown),
    groupDesc               = filterEmpty(parsed.groupDesc).view.mapValues(markupToMarkdown).toMap,
    groupNames              = filterEmpty(parsed.groupNames).view.mapValues(markupToMarkdown).toMap,
    groupPrio               = filterEmpty(parsed.groupPrio).view.mapValues(markupToMarkdown).toMap,
    hideImplicitConversions = filterEmpty(parsed.hideImplicitConversions).map(markupToMarkdown)
  )
}

case class MarkdownComment(
  ent: Repr,
  parsed: ParsedComment,
  packages: Packages,
) extends MarkupConversion[MarkdownNode](packages) {

  def stringToMarkup(str: String) =
    // null
    HtmlParsers.parseToMarkdown(str, ent, packages)

  def stringToShortMarkdown(str: String) =
    "short"
    // str.toMarkdown(ent, packages).shortenAndShow

  def markupToMarkdown(md: MarkdownNode) =
    // null
    HtmlParsers.renderToText(md)

  def linkedExceptions(m: Map[String, String]) = {
    // val inlineToMarkdown = InlineToMarkdown(ent)
    m.map { case (targetStr, body) =>
      val link = makeRepresentationLink(ent, packages, targetStr, targetStr)
      // (targetStr, inlineToMarkdown(link))
      (targetStr, link.title)
    }
  }

  def filterEmpty(xs: List[String]) =
    xs.map(_.trim)
      .filterNot(_.isEmpty)
      .map(stringToMarkup)

  def filterEmpty(xs: Map[String, String]) =
    xs.view.mapValues(_.trim).toMap
      .filterNot { case (_, v) => v.isEmpty }
      .view.mapValues(stringToMarkup).toMap
}

// case class WikiComment(ent: Representation, parsed: ParsedComment, packages: Map[String, EmulatedPackageRepresentation])
// extends MarkupConversion[Body](packages) {

//   def filterEmpty(xs: Map[String,String]) =
//     xs.view.mapValues(_.toWiki(ent, packages)).toMap
//       .filterNot { case (_, v) => v.blocks.isEmpty }

//   def filterEmpty(xs: List[String]) =
//     xs.map(_.toWiki(ent, packages))

//   def markupToMarkdown(t: Body) =
//     t.show(ent)

//   def stringToMarkup(str: String) =
//     str.toWiki(ent, packages)

//   def stringToShortMarkdown(str: String) = {
//     val parsed = stringToMarkup(str)
//     parsed.summary.getOrElse(parsed).show(ent)
//   }

//   def linkedExceptions(m: Map[String, String]) = {
//     m.view.mapValues(_.toWiki(ent, packages)).toMap.map { case (targetStr, body) =>
//       val link = lookup(Some(ent), packages, targetStr)
//       val newBody = body match {
//         case Body(List(Paragraph(Chain(content)))) =>
//           val descr = Text(" ") +: content
//           val link = makeRepresentationLink(ent, packages, Monospace(Text(targetStr)), targetStr)
//           Body(List(Paragraph(Chain(link +: descr))))
//         case _ => body
//       }

//       (targetStr, newBody.show(ent))
//     }
//   }
// }
