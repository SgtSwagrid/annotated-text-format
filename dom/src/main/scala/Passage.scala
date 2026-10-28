package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, Problem, Span,
}
import org.scalajs.dom

/**
  * A stretch of a page, beneath `root`: its text nodes, and the elements within
  * it that are not prose, as code, each of which stands in its `text` as one
  * [[Passage.placeholder]], so that the text on either side of it is not taken
  * as side by side. An element that a decoration put there stands as the text
  * it shows, as a translation, so that a page decorated a piece at a time reads
  * as it is seen, though nothing within it may be annotated again. Each `<br>`,
  * and each boundary between blocks within it, as paragraphs, stands in its
  * `text` as a line break. Annotations of the passage are of its `text`.
  */
final class Passage private (
  val root: dom.Node,
  parts: Vector[Passage.Part],
):

  /**
    * Its text: that of its text nodes, what each element of a decoration shows,
    * and a placeholder for each other element.
    */
  val text: String = parts.map(_.text).mkString

  /**
    * Its text as the page held it before any decoration: that of its text
    * nodes, what each element of a decoration took the place of, and a
    * placeholder for each other element, as for the words around a passage. Its
    * offsets are not those of [[text]].
    */
  val original: String = parts.map(_.original).mkString

  /** Where each of its parts begins in [[text]]. */
  private val starts = parts.scanLeft(0)(_ + _.text.length)

  /** Its text nodes, each with its span in [[text]]. */
  private[dom] val texts: Vector[(dom.Text, Span)] = parts
    .zip(starts)
    .collect { case (Passage.Part.Prose(node), start) =>
      node -> Span(start, start + node.data.length)
    }

  /**
    * The spans in [[text]] of its elements that are not prose, those of
    * decorations among them, within which nothing is annotated.
    */
  private[dom] val atoms: Vector[(dom.Element, Span)] = parts
    .zip(starts)
    .collect { case (part @ Passage.Part.Atom(element), start) =>
      element -> Span(start, start + part.text.length)
    }

  /**
    * Where `offset` within `node` lies in its text, if `node` is one of its
    * text nodes, or within an element that is not prose, as a decoration's,
    * where a reader may select what it shows, or an element whose child at
    * `offset`, or else whose end, is at one.
    *
    * A passage is of the page as it was when read, so these, [[spanOf]] and
    * [[rangeOf]] find nothing in what has changed since, as text nodes that a
    * decoration has split; read it again, then, as `Passage(root)`.
    */
  def offsetOf(node: dom.Node, offset: Int): Option[Int] =
    if node.nodeType == Passage.textNode then
      texts
        .collectFirst {
          case (text, span) if (text eq node) && unchanged(text) =>
            span.start + offset.max(0).min(text.data.length)
        }
        .orElse(inAtom(node.asInstanceOf[dom.Text], offset))
    else
      val nodes = node.childNodes
      if offset >= nodes.length then
        last(node).flatMap(text => offsetOf(text, text.data.length))
      else
        atoms
          .collectFirst { case (atom, span) if atom eq nodes(offset) => span.start }
          .orElse(first(nodes(offset)).flatMap(offsetOf(_, 0)))

  /**
    * The span in its text of `range`, as of what the reader selects, if both
    * its ends are at its text nodes.
    */
  def spanOf(range: dom.Range): Option[Span] =
    for
      start <- offsetOf(
        range.startContainer,
        range.startOffset,
      )
      end <- offsetOf(range.endContainer, range.endOffset) if start <= end
    yield Span(start, end)

  /**
    * A range of the page over `span` of its text, if both its ends are at its
    * text nodes: the start at the start of the text after it, and the end at
    * the end of the text before it, or at either, at a point.
    */
  def rangeOf(span: Span): Option[dom.Range] =
    for
      (startNode, start) <-
        at(span.start, after = false).orElse(at(span.start, after = true))
      (endNode, end) <-
        at(span.end, after = true).orElse(at(span.end, after = false))
      if unchanged(startNode) && unchanged(endNode)
    yield
      val range = document.createRange()
      range.setStart(startNode, start)
      range.setEnd(endNode, end)
      range

  /**
    * Where `offset` within `text`, within one of its elements that are not
    * prose, lies in its text: in what a decoration shows, or else at the
    * element's placeholder.
    */
  private def inAtom(text: dom.Text, offset: Int): Option[Int] = atoms
    .collectFirst {
      case (element, span) if element.contains(text) =>
        if !element.hasAttribute(Passage.decorated) then span.start
        else
          val before = Passage
            .textsWithin(element)
            .takeWhile(_ ne text)
            .map(_.data.length)
            .sum
          (span.start + before + offset.max(0).min(text.data.length)).min(
            span.end,
          )
    }

  /**
    * Whether `text`, one of its text nodes, holds what it held when it was
    * read, as one that a decoration has since split does not.
    */
  private def unchanged(text: dom.Text): Boolean = texts.exists((node, span) =>
    (node eq text) && text.data.length == span.end - span.start,
  )

  /** The first of its text nodes within or after `node`, in order. */
  private def first(node: dom.Node): Option[dom.Text] = texts
    .map(_._1)
    .find(text =>
      (text eq node) || node.contains(text) || Passage.follows(text, node),
    )

  /** The last of its text nodes within `node`. */
  private def last(node: dom.Node): Option[dom.Text] = texts
    .map(_._1)
    .findLast(text => node.contains(text))

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

  /**
    * The attribute held by every element that a decoration puts in a page,
    * whose value is the text it took the place of, if any.
    */
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
      case Prose(node)   => node.data
      case Atom(element) =>
        if element.hasAttribute(decorated) then element.textContent
        else placeholder.toString
      case Break => "\n"

    /** Its text before any decoration. */
    def original: String = this match
      case Atom(element) if element.hasAttribute(decorated) =>
        element.getAttribute(decorated)
      case _ => text

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

  /** The text nodes within `node`, in order. */
  private def textsWithin(node: dom.Node): List[dom.Text] =
    if node.nodeType == textNode then List(node.asInstanceOf[dom.Text])
    else children(node).flatMap(textsWithin)

  /** Whether `node` comes after `other` in the page, and not within it. */
  private def follows(node: dom.Node, other: dom.Node): Boolean =
    (other.compareDocumentPosition(node) & following) != 0 &&
    !other.contains(node)

  /** What `compareDocumentPosition` gives for a node that follows. */
  private val following = 4

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
