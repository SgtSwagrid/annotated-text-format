package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Annotation, Span}
import munit.FunSuite

class LayoutSuite extends FunSuite:

  private def annotated
    (
      text: String,
      annotations: Annotation[String]*,
    ) = AnnotatedText.of(text, annotations.toList).toOption.get

  test("Segments make up the whole text, cut at every end of a span"):
    val segments = Layout.of(annotated(
      "She always is late.",
      Annotation(
        List(Span(4, 10), Span.point(14)),
        "move",
      ),
      Annotation(Span(0, 3), "praise"),
    ))
    assertEquals(
      segments.map(segment =>
        segment.span -> segment.marks.map(mark => (mark.annotation, mark.part)),
      ),
      List(
        Span(0, 3)     -> List((1, 0)),
        Span(3, 4)     -> Nil,
        Span(4, 10)    -> List((0, 0)),
        Span(10, 14)   -> Nil,
        Span.point(14) -> List((0, 1)),
        Span(14, 19)   -> Nil,
      ),
    )

  test("A stretch under overlapping annotations holds the marks of both"):
    val segments = Layout.of(annotated(
      "a b c",
      Annotation(Span(0, 3), "x"),
      Annotation(Span(2, 5), "y"),
    ))
    assertEquals(
      segments.map(_.marks.map(_.annotation)),
      List(List(0), List(0, 1), List(1)),
    )

  test(
    "An enclosing annotation takes the lane against the text, and one nested in it the next",
  ):
    assertEquals(
      Layout.lanes(List(
        Annotation(Span(2, 5), "inner"),
        Annotation(Span(0, 10), "outer"),
      )),
      Vector(1, 0),
    )

  test(
    "An annotation keeps one lane for all its spans, which others between them may share",
  ):
    val split   = Annotation(List(Span(0, 2), Span(6, 8)), "split")
    val between = Annotation(Span(3, 5), "between")
    val across  = Annotation(Span(1, 7), "across")
    assertEquals(
      Layout.lanes(List(split, between, across)),
      Vector(0, 0, 1),
    )
