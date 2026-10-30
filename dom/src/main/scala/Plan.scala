package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{AnnotatedText, TextSpan}

/** What is put in a page for an annotated text, before any of it is. */
private[dom] enum Piece[+A]:

  /**
    * The text at `span`, wrapped, under `marks`, and showing the text of
    * `replacement` in its place while its annotation is shown. With no marks,
    * it is a gap: a space taken away with an emptied span.
    */
  case Wrap
    (
      span: TextSpan,
      marks: List[Mark[A]],
      replacement: Option[Replacement],
    )

  /**
    * Text put in at the point `at`, for `mark`, spaced off the text before it
    * and after it if `before` and `after`; or, with no `text`, an empty marker.
    */
  case Insert
    (
      at: Int,
      mark: Mark[A],
      text: Option[String],
      before: Boolean,
      after: Boolean,
    )

  /** The annotations that it belongs to. */
  def annotations: Set[Int] = this match
    case Wrap(_, marks, replacement) => marks.map(_.annotation).toSet ++
        replacement.map(_.annotation)
    case Insert(_, mark, _, _, _) => Set(mark.annotation)

/** `text`, shown in place of a span while the annotation `annotation` is shown. */
private[dom] final case class Replacement(annotation: Int, text: String)

private[dom] object Plan:

  /**
    * The pieces to put in a page for `annotated`, as `look` shows it, whose
    * text is in runs at `texts`, as a page's text nodes are. A wrapped span
    * crossing from one run to another is wrapped in each, its replacement shown
    * in the first, and none of what lies outside them is wrapped. A point
    * within a replaced span, which has no place in what it shows, is left out,
    * and so are the annotations `skipped`, by their indices.
    */
  def of[A]
    (
      annotated: AnnotatedText[A],
      look: Look[A],
      texts: List[TextSpan],
      skipped: Set[Int] = Set.empty,
    )
    : List[Piece[A]] =
    val segments = Layout
      .of(annotated)
      .map(segment =>
        segment.copy(marks =
          segment.marks.filterNot(mark => skipped(mark.annotation)),
        ),
      )
    val replaced = replacements(annotated, look, skipped)
    val wraps    = wrapped(segments, replaced)
    val points   = segments.filter(segment =>
      segment.span.isPoint &&
      !replaced.exists((span, _) => within(segment.span.start, span)),
    )
    (wraps ++ gaps(annotated.text, replaced, wraps)).flatMap(cut(_, texts)) ++
      inserts(annotated.text, points, look)

  /** Whether `at` lies inside `span`, not at either end. */
  private def within(at: Int, span: TextSpan): Boolean = span.start < at &&
    at < span.end

  /**
    * The spans that annotations show other text in place of, with what they
    * show: of any overlapping, only the first.
    */
  private def replacements[A]
    (
      annotated: AnnotatedText[A],
      look: Look[A],
      skipped: Set[Int],
    )
    : List[(TextSpan, Replacement)] = annotated
    .annotations
    .zipWithIndex
    .filterNot((_, index) => skipped(index))
    .flatMap((annotation, index) =>
      annotation
        .spans
        .zipWithIndex
        .collect {
          case (span, part) if !span.isPoint =>
            look
              .shown(annotation.value, part)
              .map(span -> Replacement(index, _))
        }
        .flatten,
    )
    .foldLeft(Vector.empty[(TextSpan, Replacement)])((kept, next) =>
      if kept.exists(_._1.overlaps(next._1)) then kept else kept :+ next,
    )
    .toList

  /**
    * The stretches of `segments` with marks, each wrapped alone, but those
    * within a replaced span, wrapped together with it.
    */
  private def wrapped[A]
    (
      segments: List[Segment[A]],
      replaced: List[(TextSpan, Replacement)],
    )
    : List[Piece.Wrap[A]] =
    val stretches = segments.filterNot(_.span.isPoint)
    val alone     = stretches.filter(segment =>
      segment.marks.nonEmpty && !replaced.exists(_._1.covers(segment.span)),
    )
    (alone.map(segment => new Piece.Wrap(segment.span, segment.marks, None)) ++
      replaced.map(together(stretches))).sortBy(_.span)

  /**
    * The replaced `span`, with the marks of every stretch within it, each
    * annotation's in a lane of its own, as they share one element.
    */
  private def together[A]
    (stretches: List[Segment[A]])
    (span: TextSpan, replacement: Replacement)
    : Piece.Wrap[A] = new Piece.Wrap(
    span,
    apart(
      stretches
        .filter(segment => span.covers(segment.span))
        .flatMap(_.marks)
        .distinct,
    ),
    Some(replacement),
  )

  /**
    * `marks`, each annotation's in a lane no other annotation's holds: its own,
    * if it is free, or else the next one free, as two annotations sharing a
    * lane elsewhere, never side by side, may meet in one element.
    */
  private def apart[A](marks: List[Mark[A]]): List[Mark[A]] =
    val lanes = marks
      .map(_.annotation)
      .distinct
      .sortBy(annotation =>
        marks.find(_.annotation == annotation).fold(0)(_.lane),
      )
      .foldLeft(Map.empty[Int, Int])((lanes, annotation) =>
        val wanted = marks.find(_.annotation == annotation).fold(0)(_.lane)
        lanes +
          (annotation ->
            Iterator.from(wanted).dropWhile(lanes.values.toSet).next()),
      )
    marks.map(mark => mark.copy(lane = lanes(mark.annotation)))

  /**
    * A gap for a space beside each emptied span, as [[Spacing.besides]] wants
    * them, of those that nothing wraps and no other emptied span has taken, so
    * that two emptied side by side take a space each. They are taken from the
    * last span back, as the last may have only the space before it, which the
    * one before it would otherwise take as the space after it.
    */
  private def gaps[A]
    (
      text: String,
      replaced: List[(TextSpan, Replacement)],
      wraps: List[Piece.Wrap[A]],
    )
    : List[Piece.Wrap[A]] = replaced
    .filter((_, replacement) => replacement.text.isEmpty)
    .sortBy((span, _) => -span.start)
    .foldLeft(List.empty[Piece.Wrap[A]])((taken, emptied) =>
      gap(text, emptied, wraps, taken).fold(taken)(_ :: taken),
    )

  /**
    * The gap for the first space beside `emptied` that nothing of `wraps`
    * wraps, and that none of `taken` has, if any.
    */
  private def gap[A]
    (
      text: String,
      emptied: (TextSpan, Replacement),
      wraps: List[Piece.Wrap[A]],
      taken: List[Piece.Wrap[A]],
    )
    : Option[Piece.Wrap[A]] =
    val (span, replacement) = emptied
    Spacing
      .besides(text, span)
      .find(space =>
        !wraps.exists(_.span.overlaps(space)) && !taken.exists(_.span == space),
      )
      .map(space =>
        new Piece.Wrap(
          space,
          Nil,
          Some(replacement.copy(text = "")),
        ),
      )

  /**
    * `wrap`, in a piece for each of `texts` that it lies across, its
    * replacement shown in the first, and none in the rest.
    */
  private def cut[A]
    (
      wrap: Piece.Wrap[A],
      texts: List[TextSpan],
    )
    : List[Piece[A]] = texts
    .flatMap(intersection(wrap.span, _))
    .zipWithIndex
    .map((span, index) =>
      wrap.copy(
        span = span,
        replacement = wrap
          .replacement
          .map(replacement =>
            if index == 0 then replacement else replacement.copy(text = ""),
          ),
      ),
    )

  private def intersection(one: TextSpan, two: TextSpan): Option[TextSpan] =
    val span = TextSpan(
      one.start.max(two.start),
      one.end.min(two.end),
    )
    Option.when(span.start < span.end)(span)

  /** The text put in at each of `points`, spaced as it needs. */
  private def inserts[A]
    (
      text: String,
      points: List[Segment[A]],
      look: Look[A],
    )
    : List[Piece[A]] = points.flatMap(segment =>
    segment
      .marks
      .map(mark =>
        val shown           = look.shown(mark.value, mark.part)
        val (before, after) =
          if shown.exists(_.nonEmpty) then
            Spacing.around(text, segment.span.start)
          else (false, false)
        Piece.Insert(
          segment.span.start,
          mark,
          shown,
          before,
          after,
        ),
      ),
  )
