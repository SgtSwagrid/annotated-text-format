package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, Problem, Span,
}
import org.scalajs.dom

/**
  * A stretch of a page, beneath `root`: its text nodes, and the elements within
  * it that are not prose, as code, or a decoration's own, each of which stands
  * in its `text` as one [[Passage.placeholder]], so that the text on either
  * side of it is not taken as side by side. Annotations of the passage are of
  * its `text`.
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
    * The passage beneath `root`, in which the elements that `skip` holds for,
    * and those of decorations, are not prose.
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

    def text: String = this match
      case Prose(node) => node.data
      case Atom(_)     => placeholder.toString

  /**
    * The parts of `node`: itself, if a text or not prose, or else its
    * children's.
    */
  private def partsOf
    (
      node: dom.Node,
      skip: dom.Element => Boolean,
    )
    : List[Part] = node.nodeType match
    case `textNode` => List(Part.Prose(node.asInstanceOf[dom.Text]))
    case `elementNode` if atomic(node.asInstanceOf[dom.Element], skip) =>
      List(Part.Atom(node.asInstanceOf[dom.Element]))
    case _ => children(node).flatMap(partsOf(_, skip))

  private def atomic
    (
      element: dom.Element,
      skip: dom.Element => Boolean,
    )
    : Boolean = element.hasAttribute(decorated) || skip(element)

  /** The children of `node`, in order. */
  private[dom] def children(node: dom.Node): List[dom.Node] =
    val nodes = node.childNodes
    List.tabulate(nodes.length)(nodes(_))

  // The types of nodes, as numbers, as JavaScript's `Node` may not be global,
  // as in Node.js.
  private[dom] val elementNode  = 1
  private[dom] val textNode     = 3
  private[dom] val documentNode = 9
