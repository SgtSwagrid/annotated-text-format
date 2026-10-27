package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, Problem, Span,
}
import scala.collection.mutable

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
  * The quoted annotation `quoted`, at `index` among those anchored, which could
  * not be placed in the text, for `reason`.
  */
final case class Unplaced[+A]
  (
    index: Int,
    quoted: Quoted[A],
    reason: Unplaced.Reason,
  ):

  /** It, described in a sentence or two, as for the model that wrote it. */
  def message: String =
    s"Annotation $index quotes \"${ quoted.quote }\", ${ reason.message }"

object Unplaced:

  /** Why a quoted annotation could not be placed. */
  enum Reason:

    /** Its quote is not in the text, as written or loosely. */
    case Missing

    /**
      * Its quote is in the text, but more than once, and only before the
      * passage of the annotation given before it.
      */
    case OutOfOrder

    /** Its quote has nothing in it to find but markers. */
    case Empty

    /** Its parts were found, but not apart from one another. */
    case Unordered

    /** It, as the end of a sentence describing an unplaced annotation. */
    def message: String = this match
      case Missing =>
        "which is not in the text. Quote the passage exactly as the text writes it."
      case OutOfOrder =>
        s"which is in the text more than once, but not after the passage of the annotation before it. Give the annotations in the order in which their passages begin, or quote a word or two around the passage, marking the passage itself with \"${ Quoted
            .open }\" and \"${ Quoted.close }\"."
      case Empty     => "which holds nothing to find in the text."
      case Unordered =>
        "whose parts are not in the text apart from one another, in order."

/** Finds where quoted annotations are in a text, and quotes annotations. */
object Anchoring:

  /**
    * `quoted`, anchored to `text`, as given in the order in which their
    * passages begin. Each is placed by the first of these ways that finds it:
    *   - where it first occurs from where the one before it began on, but not
    *     just where that one is, as a passage nested in it may begin where it
    *     does, but a word quoted again is a later occurrence;
    *   - else just where the one before it is, as two may annotate the same
    *     words;
    *   - else wherever it occurs only once, as a model may give one out of
    *     order, though where the search goes on from never moves back.
    *
    * Within each way, the quote is sought as written before as loosely as
    * `matching` allows, so that a correction of the case or spacing of a word
    * lands on the word as written. Each part of a quote is found after the part
    * before it, and a quote holding an ellipsis is first sought whole, as the
    * text may hold one.
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
    * The annotations of `annotated`, in the order in which they begin, quoted
    * as a model would write them, each with as few words around it as anchor it
    * back to where it is, as [[anchor]] finds it: none, unless the same words
    * stand earlier where they would be found instead, and then a word or more
    * either side, the passage marked between them. Each point is quoted with
    * the word on either side of it, or more.
    */
  def quote[A]
    (
      annotated: AnnotatedText[A],
      matching: Matching = Matching(),
    )
    : List[Quoted[A]] = annotated
    .annotations
    .sortBy(_.spans.headOption.fold(0)(_.start))
    .foldLeft((Search[A](annotated.text, matching), Vector.empty[Quoted[A]])):
      case ((search, quotes), annotation) =>
        val quoted = Quoting(annotated.text).of(annotation, search)
        (search.next(quoted, quotes.size), quotes :+ quoted)
    ._2
    .toList

/** How the annotations of `text` are quoted. */
private final case class Quoting(text: String):

  /** The most words quoted on either side of a passage to tell it apart. */
  private val reach = 8

  /**
    * `annotation`, quoted with as few words around it as `search` places back
    * where it is; or else with as many as it may be quoted with.
    */
  def of[A]
    (
      annotation: Annotation[A],
      search: Search[A],
    )
    : Quoted[A] =
    val quotes = (0 to reach)
      .map(context =>
        Quoted(
          quote(annotation, context),
          annotation.value,
        ),
      )
      .distinct
    quotes.find(search.placesBack(_, annotation.spans)).getOrElse(quotes.last)

  private def quote(annotation: Annotation[?], context: Int): String =
    annotation.spans.map(part(_, context)).mkString(Quoted.separator)

  /**
    * `span`, quoted with `context` words on either side of it, marked between
    * them; or alone if there are none; or, if it is a point, with a word more.
    */
  private def part(span: Span, context: Int): String =
    if span.isPoint then
      around(
        span,
        context + 1,
        Quoted.caret.toString,
        "",
      )
    else if context == 0 then span.in(text)
    else
      around(
        span,
        context,
        Quoted.open.toString,
        Quoted.close.toString,
      )

  private def around
    (
      span: Span,
      words: Int,
      open: String,
      close: String,
    )
    : String = text.substring(before(span.start, words), span.start) + open +
    span.in(text) + close + text.substring(span.end, after(span.end, words))

  /**
    * Where the `words`th word before `at` begins, a word being a run of
    * non-space.
    */
  private def before(at: Int, words: Int): Int = (1 to words).foldLeft(at)(
    (at, _) => wordBefore(at),
  )

  /** Where the `words`th word after `at` ends, a word being a run of non-space. */
  private def after(at: Int, words: Int): Int = (1 to words).foldLeft(at)(
    (at, _) => wordAfter(at),
  )

  private def wordBefore(at: Int): Int = (at - 1 to 0 by -1)
    .find(!text(_).isWhitespace)
    .fold(at)(end => (end to 0 by -1).find(text(_).isWhitespace).fold(0)(_ + 1))

  private def wordAfter(at: Int): Int = (at until text.length)
    .find(!text(_).isWhitespace)
    .fold(at)(start =>
      (start until text.length)
        .find(text(_).isWhitespace)
        .getOrElse(text.length),
    )

/**
  * A search of `text` for quoted annotations, one after another: where it goes
  * on from, `cursor`, which never moves back; the spans of the annotation last
  * placed, `previous`; what was found; and what was not.
  */
private final case class Search[A]
  (
    text: String,
    matching: Matching,
    cursor: Int = 0,
    previous: Option[List[Span]] = None,
    found: Vector[Annotation[A]] = Vector.empty,
    unplaced: Vector[Unplaced[A]] = Vector.empty,
  ):

  /** It, with `quoted`, at `index`, placed or not. */
  def next(quoted: Quoted[A], index: Int): Search[A] =
    Finding(text, matching, cursor, previous).place(quoted.quote) match
      case Right(spans) => copy(
          cursor = cursor.max(spans.head.start),
          previous = Some(spans),
          found = found :+ Annotation(spans, quoted.value),
        )
      case Left(reason) =>
        copy(unplaced = unplaced :+ Unplaced(index, quoted, reason))

  /** Whether `quoted`, placed next, is placed at `spans`. */
  def placesBack(quoted: Quoted[A], spans: List[Span]): Boolean =
    val next = this.next(quoted, unplaced.size + found.size)
    next.found.size > found.size && next.found.last.spans == spans

  def anchored: Anchored[A] = Anchored(
    AnnotatedText(text, found.toList),
    unplaced.toList,
  )

/**
  * The search of `text` for one quote, from `cursor`, after the annotation last
  * placed at `previous`.
  */
private final case class Finding
  (
    text: String,
    matching: Matching,
    cursor: Int,
    previous: Option[List[Span]],
  ):

  /** Matching as the quote is written, and then as loosely as allowed. */
  private val matchings = List(matching.exact, matching).distinct

  /**
    * Where each part is in the text, as each way of matching finds it, found
    * once.
    */
  private val found = mutable.HashMap.empty[(Matching, String), List[Found]]

  private def matches(matching: Matching, part: String): List[Found] = found
    .getOrElseUpdate(
      (matching, part),
      matching.find(part, text),
    )

  /** Where `quote` is placed, or why it is not. */
  def place(quote: String): Either[Unplaced.Reason, List[Span]] =
    val readings = Finding.readings(quote)
    if !readings.exists(_.exists(Matching.matchable)) then
      Left(Unplaced.Reason.Empty)
    else
      attempts(readings).headOption match
        case Some(spans) if fits(spans) => Right(spans)
        case Some(_)                    => Left(Unplaced.Reason.Unordered)
        case None                       => Left(missing(readings))

  /** Every placing of `readings`, by each way of looking in turn. */
  private def attempts(readings: List[List[String]]): LazyList[List[Span]] =
    ways
      .to(LazyList)
      .flatMap(way =>
        matchings
          .to(LazyList)
          .flatMap(matching => readings.to(LazyList).flatMap(way(matching, _))),
      )

  private def ways: List[(Matching, List[String]) => Option[List[Span]]] =
    List(onwards, once)

  /**
    * Where `parts` first are from the cursor on, not just where the annotation
    * last placed is, unless only there.
    */
  private def onwards
    (matching: Matching, parts: List[String])
    : Option[List[Span]] =
    val all = from(matching, parts, cursor)
    all.find(spans => !previous.contains(spans)).orElse(all.headOption)

  /** Where `parts` are, if the first occurs only once, anywhere. */
  private def once
    (matching: Matching, parts: List[String])
    : Option[List[Span]] = parts
    .headOption
    .map(matches(matching, _))
    .collect { case only :: Nil => only.span.start }
    .flatMap(from(matching, parts, _).headOption)

  /**
    * Each placing of `parts` whose first's passage begins from `start` on, in
    * order, each later part's beginning after the passage of the one before
    * ends: the passages they mark, not the words around them, which may begin
    * before and overlap.
    */
  private def from
    (
      matching: Matching,
      parts: List[String],
      start: Int,
    )
    : LazyList[List[Span]] = parts match
    case Nil          => LazyList.empty
    case head :: rest => matches(matching, head)
        .to(LazyList)
        .filter(_.span.start >= start)
        .flatMap(found =>
          after(matching, rest, found.span.end).map(found.span :: _),
        )

  private def after
    (
      matching: Matching,
      parts: List[String],
      start: Int,
    )
    : Option[List[Span]] = parts
    .foldLeft(Option((start, Vector.empty[Span]))):
      case (Some((at, spans)), part) => matches(matching, part)
          .find(_.span.start >= at)
          .map(found => (found.span.end, spans :+ found.span))
      case (None, _) => None
    .map(_._2.toList)

  /**
    * Why `readings`, placed nowhere, were not: out of order, if they are in the
    * text at all, or else missing.
    */
  private def missing(readings: List[List[String]]): Unplaced.Reason =
    if matchings.exists(matching =>
        readings.exists(from(matching, _, 0).nonEmpty),
      )
    then Unplaced.Reason.OutOfOrder
    else Unplaced.Reason.Missing

  private def fits(spans: List[Span]): Boolean = Problem
    .of(text)(Annotation(spans, ()), 0)
    .isEmpty

private object Finding:

  /**
    * The ways to read the parts of `quote`: whole first, if it is in parts, as
    * the text may hold an ellipsis itself, and then in its parts.
    */
  def readings(quote: String): List[List[String]] =
    val parts = Quoted.parts(quote)
    Option.when(parts.size > 1)(List(quote.trim)).toList :+ parts
