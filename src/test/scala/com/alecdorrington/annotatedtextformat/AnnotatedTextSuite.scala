package com.alecdorrington.annotatedtextformat

import munit.FunSuite

class AnnotatedTextSuite extends FunSuite:

  private val text = "Ich stehe früh auf."

  /** The separable verb "stehe … auf", annotated as one. */
  private val verb = Annotation(
    List(TextSpan(4, 9), TextSpan(15, 18)),
    "get up",
  )

  test(
    "A span counts the characters within it, a point lies at its ends but never overlaps it, and spans are as far apart as the characters between them",
  ):
    assertEquals(TextSpan(4, 9).length, 5)
    assertEquals(TextSpan(4, 9).in(text), "stehe")
    assert(TextSpan.point(3).isPoint)
    assert(!TextSpan(4, 9).isPoint)
    assert(TextSpan(4, 9).covers(TextSpan.point(4)))
    assert(TextSpan(4, 9).covers(TextSpan.point(9)))
    assert(!TextSpan(4, 9).overlaps(TextSpan.point(6)))
    assert(TextSpan(4, 9).overlaps(TextSpan(8, 12)))
    assert(!TextSpan(4, 9).overlaps(TextSpan(9, 12)))
    assertEquals(
      TextSpan(0, 3).distance(TextSpan(5, 6)),
      2,
    )
    assertEquals(
      TextSpan(5, 6).distance(TextSpan(0, 3)),
      2,
    )
    assertEquals(
      TextSpan(0, 3).distance(TextSpan(2, 6)),
      0,
    )
    assertEquals(verb.extent, Some(TextSpan(4, 18)))
    assertEquals(Annotation(Nil, "nothing").extent, None)

  test(
    "Annotations whose spans fit the text make an annotated text, touching, overlapping and nesting as they may",
  ):
    val annotated = AnnotatedText.of(text, List(verb))
    assertEquals(
      annotated.map(_.annotations),
      Right(List(verb)),
    )
    assertEquals(
      annotated.map(_.annotations.head.in(text)),
      Right(List("stehe", "auf")),
    )
    val touching = Annotation(
      List(
        TextSpan(0, 3),
        TextSpan.point(3),
        TextSpan(3, 4),
      ),
      "",
    )
    assert(AnnotatedText.of(text, List(touching)).isRight)
    val nested = List(
      Annotation(TextSpan(4, 18), "outer"),
      Annotation(TextSpan(4, 9), "inner"),
    )
    assert(AnnotatedText.of(text, nested :+ verb).isRight)

  test(
    "An annotation with no spans, or a span outside the text, out of order or splitting a character, is refused",
  ):
    assertEquals(
      AnnotatedText.of(text, List(Annotation(Nil, "nothing"))),
      Left(List(Problem.Unspanned(0))),
    )
    val outside = List(
      TextSpan(-1, 2),
      TextSpan(3, 2),
      TextSpan(15, 30),
    )
    assertEquals(
      AnnotatedText.of(text, outside.map(Annotation(_, ""))),
      Left(
        outside.zipWithIndex.map((span, index) => Problem.Outside(index, span)),
      ),
    )
    val unordered = List(
      List(TextSpan(15, 18), TextSpan(4, 9)),
      List(TextSpan(4, 9), TextSpan(8, 12)),
      List(TextSpan.point(4), TextSpan.point(4)),
    )
    assertEquals(
      AnnotatedText.of(text, unordered.map(Annotation(_, ""))),
      Left(unordered.indices.map(Problem.Unordered(_)).toList),
    )
    val emoji = "I love it 😀 so"
    assertEquals(
      AnnotatedText.of(
        emoji,
        List(Annotation(TextSpan(10, 11), "half")),
      ),
      Left(List(Problem.Splits(0, TextSpan(10, 11)))),
    )
    assert(
      AnnotatedText
        .of(
          emoji,
          List(Annotation(TextSpan(10, 12), "whole")),
        )
        .isRight,
    )

  test("Values are mapped, filtered and collected, keeping their spans"):
    val annotated = AnnotatedText
      .of(
        text,
        List(
          verb,
          Annotation(TextSpan(10, 14), "early"),
        ),
      )
      .toOption
      .get
    assertEquals(
      annotated.map(_.length).annotations.map(_.value),
      List(6, 5),
    )
    assertEquals(
      annotated.filter(_ == "early").annotations.map(_.spans),
      List(List(TextSpan(10, 14))),
    )
    assertEquals(
      annotated.collect { case "get up" => 1 }.annotations,
      List(verb.map(_ => 1)),
    )

  test(
    "Annotated texts join only if they are of the same text, and only the annotations that fit are kept, if asked",
  ):
    val one = AnnotatedText.of(text, List(verb)).toOption.get
    assertEquals(
      (one ++ one).map(_.annotations.size),
      Right(2),
    )
    assertEquals(
      one ++ AnnotatedText.plain("Other."),
      Left(List(Problem.OtherText)),
    )
    val stale = List(
      Annotation(TextSpan(4, 9), "kept"),
      Annotation(TextSpan(15, 40), "left out"),
    )
    assertEquals(
      AnnotatedText.fitting(text, stale).annotations.map(_.value),
      List("kept"),
    )
