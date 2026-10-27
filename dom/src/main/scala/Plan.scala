package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Span}

/** What is put in a page for an annotated text, before any of it is. */
private[dom] enum Piece[+A]:

  /**
    * The text at `span`, wrapped, under `marks`, and showing the text of
    * `replacement` in its place while its annotation is shown. With no marks,
    * it is a gap: a space taken away with an emptied span.
    */
  case Wrap
    (
      span: Span,
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
    * within a replaced span, which has no place in what it shows, is left out.
    */
  def of[A]
    (
      annotated: AnnotatedText[A],
      look: Look[A],
      texts: List[Span],
    )
    : List[Piece[A]] =
    val segments = Layout.of(annotated)
    val replaced = replacements(annotated, look)
    val wraps    = wrapped(segments, replaced)
    val points   = segments.filter(segment =>
      segment.span.isPoint &&
      !replaced.exists((span, _) => within(segment.span.start, span)),
    )
    (wraps ++ gaps(annotated.text, replaced, wraps)).flatMap(cut(_, texts)) ++
      inserts(annotated.text, points, look)

  /** Whether `at` lies inside `span`, not at either end. */
  private def within(at: Int, span: Span): Boolean = span.start < at &&
    at < span.end

  /**
    * The spans that annotations show other text in place of, with what they
    * show: of any overlapping, only the first.
    */
  private def replacements[A]
    (
      annotated: AnnotatedText[A],
      look: Look[A],
    )
    : List[(Span, Replacement)] = annotated
    .annotations
    .zipWithIndex
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
    .foldLeft(Vector.empty[(Span, Replacement)])((kept, next) =>
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
      replaced: List[(Span, Replacement)],
    )
    : List[Piece.Wrap[A]] =
    val stretches = segments.filterNot(_.span.isPoint)
    val alone     = stretches.filter(segment =>
      segment.marks.nonEmpty && !replaced.exists(_._1.covers(segment.span)),
    )
    (alone.map(segment => new Piece.Wrap(segment.span, segment.marks, None)) ++
      replaced.map(together(stretches))).sortBy(_.span)

  /** The replaced `span`, with the marks of every stretch within it. */
  private def together[A]
    (stretches: List[Segment[A]])
    (span: Span, replacement: Replacement)
    : Piece.Wrap[A] = new Piece.Wrap(
    span,
    stretches
      .filter(segment => span.covers(segment.span))
      .flatMap(_.marks)
      .distinct,
    Some(replacement),
  )

  /** A gap for the space beside each emptied span, unless something wraps it. */
  private def gaps[A]
    (
      text: String,
      replaced: List[(Span, Replacement)],
      wraps: List[Piece.Wrap[A]],
    )
    : List[Piece.Wrap[A]] = replaced
    .collect {
      case (span, replacement) if replacement.text.isEmpty =>
        Spacing
          .beside(text, span)
          .map(space =>
            new Piece.Wrap(
              space,
              Nil,
              Some(replacement.copy(text = "")),
            ),
          )
    }
    .flatten
    .filterNot(gap => wraps.exists(_.span.overlaps(gap.span)))
    .distinctBy(_.span)

  /**
    * `wrap`, in a piece for each of `texts` that it lies across, its
    * replacement shown in the first, and none in the rest.
    */
  private def cut[A](wrap: Piece.Wrap[A], texts: List[Span]): List[Piece[A]] =
    texts
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

  private def intersection(one: Span, two: Span): Option[Span] =
    val span = Span(
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
