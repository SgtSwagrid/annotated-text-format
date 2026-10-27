package com.alecdorrington.annotatedtextformat

import munit.FunSuite

class AnnotatedTextSuite extends FunSuite:

  private val text = "Ich stehe früh auf."

  /** The separable verb "stehe … auf", annotated as one. */
  private val verb = Annotation(List(Span(4, 9), Span(15, 18)), "get up")

  test("A span counts the characters within it, and a point holds none"):
    assertEquals(Span(4, 9).length, 5)
    assertEquals(Span(4, 9).in(text), "stehe")
    assert(Span.point(3).isPoint)
    assert(!Span(4, 9).isPoint)

  test(
    "A point lies within a span at either of its ends, but never overlaps it",
  ):
    assert(Span(4, 9).covers(Span.point(4)))
    assert(Span(4, 9).covers(Span.point(9)))
    assert(!Span(4, 9).overlaps(Span.point(6)))
    assert(Span(4, 9).overlaps(Span(8, 12)))
    assert(!Span(4, 9).overlaps(Span(9, 12)))

  test("Spans are as far apart as the characters between them"):
    assertEquals(Span(0, 3).distance(Span(5, 6)), 2)
    assertEquals(Span(5, 6).distance(Span(0, 3)), 2)
    assertEquals(Span(0, 3).distance(Span(2, 6)), 0)

  test("Annotations whose spans fit the text make an annotated text"):
    val annotated = AnnotatedText.of(text, List(verb))
    assertEquals(
      annotated.map(_.annotations),
      Right(List(verb)),
    )
    assertEquals(
      annotated.map(_.annotations.head.in(text)),
      Right(List("stehe", "auf")),
    )

  test("An annotation with no spans is refused"):
    assertEquals(
      AnnotatedText.of(text, List(Annotation(Nil, "nothing"))),
      Left(List(Problem.Unspanned(0))),
    )

  test("A span outside the text, or ending before it begins, is refused"):
    val spans = List(Span(-1, 2), Span(3, 2), Span(15, 30))
    assertEquals(
      AnnotatedText.of(text, spans.map(Annotation(_, ""))),
      Left(
        spans.zipWithIndex.map((span, index) => Problem.Outside(index, span)),
      ),
    )

  test("Spans out of order, overlapping, or the same point twice are refused"):
    val unordered = List(
      List(Span(15, 18), Span(4, 9)),
      List(Span(4, 9), Span(8, 12)),
      List(Span.point(4), Span.point(4)),
    )
    assertEquals(
      AnnotatedText.of(text, unordered.map(Annotation(_, ""))),
      Left(unordered.indices.map(Problem.Unordered(_)).toList),
    )

  test("A span may end where the next begins, and a point stand at its end"):
    val touching = Annotation(
      List(Span(0, 3), Span.point(3), Span(3, 4)),
      "",
    )
    assert(AnnotatedText.of(text, List(touching)).isRight)

  test("Annotations may overlap and nest one another"):
    val nested = List(
      Annotation(Span(4, 18), "outer"),
      Annotation(Span(4, 9), "inner"),
    )
    assert(AnnotatedText.of(text, nested :+ verb).isRight)

  test("Values are mapped, filtered and collected, keeping their spans"):
    val annotated = AnnotatedText
      .of(
        text,
        List(verb, Annotation(Span(10, 14), "early")),
      )
      .toOption
      .get
    assertEquals(
      annotated.map(_.length).annotations.map(_.value),
      List(6, 5),
    )
    assertEquals(
      annotated.filter(_ == "early").annotations.map(_.spans),
      List(List(Span(10, 14))),
    )
    assertEquals(
      annotated.collect { case "get up" => 1 }.annotations,
      List(verb.map(_ => 1)),
    )

  test("Annotated texts join only if they are of the same text"):
    val one = AnnotatedText.of(text, List(verb)).toOption.get
    assertEquals(
      (one ++ one).map(_.annotations.size),
      Right(2),
    )
    assertEquals(
      one ++ AnnotatedText.plain("Other."),
      Left(List(Problem.OtherText)),
    )
