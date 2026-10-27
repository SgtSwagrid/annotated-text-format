package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, Problem, Span,
}

/**
  * Quoted annotations anchored to a text: those found, as `annotated`, and
  * those not, as `unplaced`, which may be sent back to the model that wrote
  * them.
  */
final case class Anchored[+A]
  (
    annotated: AnnotatedText[A],
    unplaced: List[Unplaced[A]],
  ):

  /** Whether every quoted annotation was found. */
  def complete: Boolean = unplaced.isEmpty

/**
  * The quoted annotation `quoted`, at `index` among those anchored, whose quote
  * could not be found in the text.
  */
final case class Unplaced[+A](index: Int, quoted: Quoted[A]):

  /** It, described in a sentence, as for the model that wrote it. */
  def message: String =
    s"Annotation $index quotes \"${ quoted.quote }\", which is not in the text."

/** Finds where quoted annotations are in a text, and quotes annotations. */
object Anchoring:

  /**
    * `quoted`, anchored to `text`, as given in the order in which their
    * passages begin. Each is found where it first occurs after the one before
    * it began; or else where that one began, as two may annotate the same
    * words; or else wherever it occurs only once, as a model may give one out
    * of order. Each part of a quote is found after the part before it, and a
    * quote holding an ellipsis is first sought whole, as the text may hold one.
    */
  def anchor[A]
    (
      text: String,
      quoted: List[Quoted[A]],
      matching: Matching = Matching(),
    )
    : Anchored[A] = quoted
    .zipWithIndex
    .foldLeft(Search[A](text, matching))(_.next.tupled(_))
    .anchored

  /**
    * The annotations of `annotated`, quoted as a model would write them, so
    * that, given in the order in which they begin, they anchor back to where
    * they are. Each point is quoted with the word on either side of it.
    */
  def quote[A](annotated: AnnotatedText[A]): List[Quoted[A]] = annotated
    .annotations
    .map(annotation =>
      Quoted(
        annotation
          .spans
          .map(part(annotated.text, _))
          .mkString(Quoted.separator),
        annotation.value,
      ),
    )

  private def part(text: String, span: Span): String =
    if span.isPoint then pointed(text, span.start) else span.in(text)

  /** The point `at` in `text`, quoted with the word on either side of it. */
  private def pointed(text: String, at: Int): String =
    text.substring(wordBefore(text, at), at) + Quoted.caret +
      text.substring(at, wordAfter(text, at))

  /** Where the word before `at` begins, a word being a run of non-space. */
  private def wordBefore(text: String, at: Int): Int = (at - 1 to 0 by -1)
    .find(!text(_).isWhitespace)
    .fold(at)(end => (end to 0 by -1).find(text(_).isWhitespace).fold(0)(_ + 1))

  /** Where the word after `at` ends, a word being a run of non-space. */
  private def wordAfter(text: String, at: Int): Int = (at until text.length)
    .find(!text(_).isWhitespace)
    .fold(at)(start =>
      (start until text.length)
        .find(text(_).isWhitespace)
        .getOrElse(text.length),
    )

/**
  * A search of `text` for quoted annotations, one after another: where the last
  * found began, `after`, what was found and what was not.
  */
private final case class Search[A]
  (
    text: String,
    matching: Matching,
    after: Int = -1,
    found: Vector[Annotation[A]] = Vector.empty,
    unplaced: Vector[Unplaced[A]] = Vector.empty,
  ):

  /** It, with `quoted`, at `index`, found or not. */
  def next(quoted: Quoted[A], index: Int): Search[A] =
    place(quoted).filter(fits(_, index)) match
      case Some(spans) => copy(
          after = spans.head.start,
          found = found :+ Annotation(spans, quoted.value),
        )
      case None => copy(unplaced = unplaced :+ Unplaced(index, quoted))

  def anchored: Anchored[A] = Anchored(
    AnnotatedText(text, found.toList),
    unplaced.toList,
  )

  /** Where `quoted` is, by the first of the ways to look that finds it. */
  private def place(quoted: Quoted[A]): Option[List[Span]] = ways
    .view
    .flatMap(way => readings(quoted).view.flatMap(way))
    .headOption

  /** The ways to look for the parts of a quote, in order. */
  private def ways: List[List[String] => Option[List[Span]]] =
    List(after + 1, after.max(0)).distinct.map(from) :+ once

  /**
    * The ways to read the parts of `quoted`: whole first, if it is in parts, as
    * the text may hold an ellipsis itself, and then in its parts.
    */
  private def readings(quoted: Quoted[A]): List[List[String]] =
    val parts = quoted.parts
    Option.when(parts.size > 1)(List(quoted.quote.trim)).toList :+ parts

  /** Where `parts` are, each first found after the last, from `start`. */
  private def from(start: Int)(parts: List[String]): Option[List[Span]] = parts
    .foldLeft(Option((start, Vector.empty[Span]))):
      case (Some((at, spans)), part) =>
        first(part, at).map(found => (found.end, spans :+ spanOf(found)))
      case (None, _) => None
    .filter(_ => parts.nonEmpty)
    .map(_._2.toList)

  /** Where `parts` are, if the first occurs only once. */
  private def once(parts: List[String]): Option[List[Span]] = parts
    .headOption
    .map(matches)
    .collect { case only :: Nil => only.start }
    .flatMap(from(_)(parts))

  private def first(part: String, start: Int): Option[Found] = matches(part)
    .find(_.start >= start)

  private def matches(part: String): List[Found] = matching.find(part, text)

  /** The span that `found` stands for: its point, if it marks one. */
  private def spanOf(found: Found): Span = found
    .point
    .fold(Span(found.start, found.end))(Span.point)

  private def fits(spans: List[Span], index: Int): Boolean = Problem
    .of(text.length)(Annotation(spans, ()), index)
    .isEmpty
