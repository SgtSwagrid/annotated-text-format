package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Annotation, Span}
import munit.FunSuite

class PlanSuite extends FunSuite:

  test(
    "Annotations meeting inside a replaced span each take a lane of their own there",
  ):
    val annotated = AnnotatedText
      .of(
        "0123456789",
        List(
          Annotation(Span(0, 10), "A"),
          Annotation(Span(0, 3), "B"),
          Annotation(Span(5, 8), "C"),
        ),
      )
      .toOption
      .get
    val look = Look[String](
      _ => Colour("teal"),
      (value, _) => Option.when(value == "A")("x"),
    )
    val pieces = Plan.of(annotated, look, List(Span(0, 10)))
    val lanes  = pieces
      .collect { case Piece.Wrap(_, marks, Some(_)) =>
        marks.map(mark => mark.annotation -> mark.lane)
      }
      .flatten
    assertEquals(lanes.map(_._2).distinct.size, 3)
