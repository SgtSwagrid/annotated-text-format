package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Annotation, Span}

/**
  * A part of an annotation lying over a [[Segment]]: the annotation, by its
  * index, the part, by the index of its span, the annotation's lane, and its
  * value.
  */
final case class Mark[+A]
  (
    annotation: Int,
    part: Int,
    lane: Int,
    value: A,
  )

/**
  * A stretch of an annotated text over all of which the same parts of
  * annotations lie, as `marks`; or a point, at which the parts that are points
  * stand.
  */
final case class Segment[+A](span: Span, marks: List[Mark[A]])

/** How an annotated text is laid out to be drawn. */
object Layout:

  /**
    * The segments of `annotated`, in order, which together make up its whole
    * text: cut at either end of every span, with each point a segment of its
    * own, before the stretch that begins there.
    */
  def of[A](annotated: AnnotatedText[A]): List[Segment[A]] =
    val marks = marked(annotated)
    (points(marks) ++ stretches(annotated.text, marks)).sortBy(segment =>
      (segment.span.start, !segment.span.isPoint),
    )

  /** Each part of each annotation of `annotated`, with its mark. */
  private def marked[A](annotated: AnnotatedText[A]): List[(Span, Mark[A])] =
    val laned = lanes(annotated.annotations)
    annotated
      .annotations
      .zipWithIndex
      .flatMap((annotation, index) =>
        annotation
          .spans
          .zipWithIndex
          .map((span, part) =>
            span -> Mark(
              index,
              part,
              laned(index),
              annotation.value,
            ),
          ),
      )

  private def points[A](marks: List[(Span, Mark[A])]): List[Segment[A]] = marks
    .filter(_._1.isPoint)
    .groupMap(_._1)(_._2)
    .toList
    .map(Segment(_, _))

  private def stretches[A]
    (
      text: String,
      marks: List[(Span, Mark[A])],
    )
    : List[Segment[A]] =
    val bounds =
      (0 :: text.length ::
        marks.flatMap((span, _) => List(span.start, span.end))).distinct.sorted
    bounds
      .zip(bounds.drop(1))
      .map((start, end) => over(Span(start, end), marks))

  /** The segment of `span`, with the marks of the parts that cover it. */
  private def over[A](span: Span, marks: List[(Span, Mark[A])]): Segment[A] =
    Segment(
      span,
      marks.collect {
        case (part, mark) if !part.isPoint && part.covers(span) => mark
      },
    )

  /**
    * The lane of each annotation of `annotations`, by its index: `0` at the
    * bottom edge of the text, deeper lanes stacked above it, as [[Style]] draws
    * them. An annotation keeps its lane for all its spans, so that its
    * underline runs unbroken, and sorting on where it ends, the furthest first,
    * gives one enclosing another the lower lane.
    */
  def lanes(annotations: List[Annotation[?]]): Vector[Int] =
    val all = annotations.toVector
    all
      .indices
      .sortBy(index =>
        all(index).extent.fold((0, 0))(span => (span.start, -span.end)),
      )
      .foldLeft(Map.empty[Int, Int])((laned, index) =>
        laned + (index -> free(laned, all, index)),
      )
      .toVector
      .sortBy(_._1)
      .map(_._2)

  /** The first lane that no annotation laned already and overlapping it holds. */
  private def free
    (
      laned: Map[Int, Int],
      all: Vector[Annotation[?]],
      index: Int,
    )
    : Int =
    val taken = laned
      .collect { case (other, lane) if all(other).overlaps(all(index)) => lane }
      .toSet
    Iterator.from(0).dropWhile(taken).next()
