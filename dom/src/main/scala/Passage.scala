package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, Problem, Span,
}
import org.scalajs.dom

/**
  * A stretch of a page, beneath `root`: its text nodes, and the elements within
  * it that are not prose, as code, or a decoration's own, each of which stands
  * in its `text` as one [[Passage.placeholder]], so that the text on either
  * side of it is not taken as side by side. Each `<br>`, and each boundary
  * between blocks within it, as paragraphs, stands in its `text` as a line
  * break. Annotations of the passage are of its `text`.
  */
final class Passage private (
  val root: dom.Node,
  parts: Vector[Passage.Part],
):

  /** Its text: that of its text nodes, and a placeholder for each element. */
  val text: String = parts.map(_.text).mkString

  /** Its text nodes, each with its span in [[text]]. */
  private[dom] val texts: Vector[(dom.Text, Span)] = parts
    .zip(parts.scanLeft(0)(_ + _.text.length))
    .collect { case (Passage.Part.Prose(node), start) =>
      node -> Span(start, start + node.data.length)
    }

  /** `annotations` of its text, if they fit it, or else what is wrong. */
  def annotate[A]
    (annotations: List[Annotation[A]])
    : Either[List[Problem], AnnotatedText[A]] =
    AnnotatedText.of(text, annotations)

  /** The document that it is in. */
  private[dom] def document: dom.Document =
    if root.nodeType == Passage.documentNode then
      root.asInstanceOf[dom.Document]
    else root.ownerDocument

  /**
    * The text node that `span` lies within, if it lies within one, with where
    * it begins and ends in it.
    */
  private[dom] def within(span: Span): Option[(dom.Text, Int, Int)] = texts
    .collectFirst {
      case (node, whole) if whole.covers(span) && !whole.isPoint =>
        (node, span.start - whole.start, span.end - whole.start)
    }

  /**
    * The text node at `offset` and the place in it: at the end of the text
    * before it if `after`, or else at the start of the text after it.
    */
  private[dom] def at(offset: Int, after: Boolean): Option[(dom.Text, Int)] =
    texts.collectFirst {
      case (node, span) if holds(span, offset, after) =>
        node -> (offset - span.start)
    }

  /** Whether `offset` is in `span`, at its end if `after`, or else its start. */
  private def holds(span: Span, offset: Int, after: Boolean): Boolean =
    if after then span.start < offset && offset <= span.end
    else span.start <= offset && offset < span.end

object Passage:

  /** What stands for an element in a passage's text, of which none is prose. */
  val placeholder: Char = '\ufffc'

  /** The attribute held by every element that a decoration puts in a page. */
  val decorated: String = "data-annotated-text"

  /**
    * The passage beneath `root`, in which the elements that are [[nonProse]],
    * those that `skip` holds for too, as code may be, and those of decorations,
    * are not prose.
    */
  def apply
    (
      root: dom.Node,
      skip: dom.Element => Boolean = _ => false,
    )
    : Passage = new Passage(root, partsOf(root, skip).toVector)

  /** A part of a passage: a text node, or an element that is not prose. */
  private[dom] enum Part:

    case Prose(node: dom.Text)
    case Atom(element: dom.Element)

    /** A break between lines, as a `<br>` makes, or between blocks. */
    case Break

    def text: String = this match
      case Prose(node) => node.data
      case Atom(_)     => placeholder.toString
      case Break       => "\n"

  /**
    * The parts beneath `root`, or of it, if it is a text node itself, a break
    * standing between blocks, but none at either end, nor two together.
    */
  private def partsOf
    (
      root: dom.Node,
      skip: dom.Element => Boolean,
    )
    : List[Part] = (if root.nodeType == textNode then List(root)
                    else children(root))
    .flatMap(within(_, skip))
    .foldLeft(List.empty[Part]):
      case (Part.Break :: rest, Part.Break) => Part.Break :: rest
      case (Nil, Part.Break)                => Nil
      case (done, part)                     => part :: done
    .dropWhile(_ == Part.Break)
    .reverse

  /**
    * The parts of `node`: itself, if a text or not prose; a break, if a `<br>`;
    * or else its children's, with a break either side of a block.
    */
  private def within
    (
      node: dom.Node,
      skip: dom.Element => Boolean,
    )
    : List[Part] = node.nodeType match
    case `textNode`    => List(Part.Prose(node.asInstanceOf[dom.Text]))
    case `elementNode` => element(node.asInstanceOf[dom.Element], skip)
    case _             => children(node).flatMap(within(_, skip))

  private def element
    (
      element: dom.Element,
      skip: dom.Element => Boolean,
    )
    : List[Part] =
    if atomic(element, skip) then List(Part.Atom(element))
    else if name(element) == "br" then List(Part.Break)
    else
      val inner = children(element).flatMap(within(_, skip))
      if blocks.contains(name(element)) then (Part.Break :: inner) :+ Part.Break
      else inner

  private def name(element: dom.Element): String = element.tagName.toLowerCase

  /** The elements that stand as blocks of their own, between lines. */
  private val blocks: Set[String] = Set(
    "address",
    "article",
    "aside",
    "blockquote",
    "caption",
    "dd",
    "details",
    "dialog",
    "div",
    "dl",
    "dt",
    "fieldset",
    "figcaption",
    "figure",
    "footer",
    "form",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "header",
    "hgroup",
    "hr",
    "li",
    "main",
    "nav",
    "ol",
    "p",
    "pre",
    "section",
    "summary",
    "table",
    "tbody",
    "td",
    "tfoot",
    "th",
    "thead",
    "tr",
    "ul",
  )

  /**
    * Whether `element` holds no prose, as a script, a style, a field to fill in
    * or a document's head does, which a passage always leaves out.
    */
  val nonProse: dom.Element => Boolean =
    element => unread.contains(name(element))

  private val unread: Set[String] = Set(
    "audio",
    "canvas",
    "datalist",
    "embed",
    "head",
    "iframe",
    "input",
    "math",
    "noscript",
    "object",
    "option",
    "script",
    "select",
    "style",
    "svg",
    "template",
    "textarea",
    "video",
  )

  private def atomic
    (
      element: dom.Element,
      skip: dom.Element => Boolean,
    )
    : Boolean = element.hasAttribute(decorated) || nonProse(element) ||
    skip(element)

  /** The children of `node`, in order. */
  private[dom] def children(node: dom.Node): List[dom.Node] =
    val nodes = node.childNodes
    List.tabulate(nodes.length)(nodes(_))

  // The types of nodes, as numbers, as JavaScript's `Node` may not be global,
  // as in Node.js.
  private[dom] val elementNode  = 1
  private[dom] val textNode     = 3
  private[dom] val documentNode = 9
  private[dom] val fragmentNode = 11
