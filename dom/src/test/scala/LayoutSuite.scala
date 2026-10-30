package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, TextSpan,
}
import munit.FunSuite

class LayoutSuite extends FunSuite:

  private def annotated
    (
      text: String,
      annotations: Annotation[String]*,
    ) = AnnotatedText.of(text, annotations.toList).toOption.get

  test(
    "Segments make up the whole text, cut at every end of a span, each holding the marks of the annotations over it",
  ):
    val segments = Layout.of(annotated(
      "She always is late.",
      Annotation(
        List(TextSpan(4, 10), TextSpan.point(14)),
        "move",
      ),
      Annotation(TextSpan(0, 3), "praise"),
    ))
    assertEquals(
      segments.map(segment =>
        segment.span -> segment.marks.map(mark => (mark.annotation, mark.part)),
      ),
      List(
        TextSpan(0, 3)     -> List((1, 0)),
        TextSpan(3, 4)     -> Nil,
        TextSpan(4, 10)    -> List((0, 0)),
        TextSpan(10, 14)   -> Nil,
        TextSpan.point(14) -> List((0, 1)),
        TextSpan(14, 19)   -> Nil,
      ),
    )
    val overlapping = Layout.of(annotated(
      "a b c",
      Annotation(TextSpan(0, 3), "x"),
      Annotation(TextSpan(2, 5), "y"),
    ))
    assertEquals(
      overlapping.map(_.marks.map(_.annotation)),
      List(List(0), List(0, 1), List(1)),
    )

  test(
    "An enclosing annotation takes the lane against the text, one nested in it the next, and one split keeps one lane, which others between its spans may share",
  ):
    assertEquals(
      Layout.lanes(List(
        Annotation(TextSpan(2, 5), "inner"),
        Annotation(TextSpan(0, 10), "outer"),
      )),
      Vector(1, 0),
    )
    val split = Annotation(
      List(TextSpan(0, 2), TextSpan(6, 8)),
      "split",
    )
    val between = Annotation(TextSpan(3, 5), "between")
    val across  = Annotation(TextSpan(1, 7), "across")
    assertEquals(
      Layout.lanes(List(split, between, across)),
      Vector(0, 0, 1),
    )
