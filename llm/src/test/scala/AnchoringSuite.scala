package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Annotation, Span}
import munit.FunSuite

class AnchoringSuite extends FunSuite:

  /** The spans at which each of `quotes` is found in `text`, in order. */
  private def spans(text: String, quotes: String*): List[List[Span]] =
    anchored(text, quotes*).annotated.annotations.map(_.spans)

  private def anchored(text: String, quotes: String*): Anchored[Int] = Anchoring
    .anchor(
      text,
      quotes.toList.zipWithIndex.map(Quoted(_, _)),
    )

  private def parts(text: String, quotes: String*): List[List[String]] =
    anchored(text, quotes*).annotated.annotations.map(_.in(text))

  test("A word quoted again is found after the one before it"):
    assertEquals(
      spans("the cat and the dog", "the", "the"),
      List(List(Span(0, 3)), List(Span(12, 15))),
    )

  test("A quote nested in the one before it is found within it"):
    assertEquals(
      spans("the big dog", "the big dog", "big"),
      List(List(Span(0, 11)), List(Span(4, 7))),
    )

  test("Two annotations of the same words are found at the same place"):
    assertEquals(
      spans("a dog", "dog", "dog"),
      List(List(Span(2, 5)), List(Span(2, 5))),
    )

  test("A quote out of order is found where it occurs only once"):
    assertEquals(
      spans("She is late.", "late", "She"),
      List(List(Span(7, 11)), List(Span(0, 3))),
    )

  test("The parts of a separable verb are one annotation"):
    assertEquals(
      parts(
        "Ich stehe morgen früh auf.",
        "stehe … auf",
      ),
      List(List("stehe", "auf")),
    )

  test("A quote whose ellipsis the text holds is found whole"):
    assertEquals(
      parts("Wait ... what?", "Wait ... what"),
      List(List("Wait ... what")),
    )

  test("A quote matches in any case, spacing, or apostrophe"):
    assertEquals(
      parts("I don’t  KNOW.", "i don't know"),
      List(List("don’t  KNOW").map("I " + _)),
    )

  test("A full stop matches whether the quote spaces it or not"):
    assertEquals(
      parts("She is late.", "late ."),
      List(List("late.")),
    )
    assertEquals(
      parts("She is late .", "late."),
      List(List("late .")),
    )

  test("Words must be spaced apart where the quote spaces them"):
    assertEquals(parts("concatenate", "con cat"), Nil)

  test(
    "A word is not found within another, unless whole words are not asked for",
  ):
    assertEquals(
      parts("concatenate the cat", "cat"),
      List(List("cat")),
    )
    assertEquals(
      spans("concatenate the cat", "cat"),
      List(List(Span(16, 19))),
    )
    assertEquals(
      Anchoring
        .anchor(
          "concatenate",
          List(Quoted("cat", ())),
          Matching(wholeWords = false),
        )
        .annotated
        .annotations
        .map(_.spans),
      List(List(Span(3, 6))),
    )

  test("A word is not found as part of one joined to it by an apostrophe"):
    assertEquals(spans("l'eau", "eau"), Nil)

  test("Words of a script that does not space them are found within others"):
    assertEquals(parts("我的猫很可爱", "猫"), List(List("猫")))

  test(
    "A caret marks the point at which it stands, beside the space it is beside",
  ):
    assertEquals(
      spans("She is late.", "is ‸late"),
      List(List(Span.point(7))),
    )
    assertEquals(
      spans("She is late.", "is‸ late"),
      List(List(Span.point(6))),
    )
    assertEquals(
      spans("She is late.", "is ‸ late"),
      List(List(Span.point(7))),
    )

  test("Words to move, and where they belong, are one annotation"):
    assertEquals(
      spans(
        "She always is late.",
        "always … is ‸late",
      ),
      List(List(Span(4, 10), Span.point(14))),
    )

  test("A quote with nothing but a caret, or not in the text, is not placed"):
    val result = anchored("She is late.", "‸", "early", "late")
    assertEquals(
      result.unplaced.map(_.index),
      List(0, 1),
    )
    assertEquals(
      result.unplaced.map(_.message),
      List(
        "Annotation 0 quotes \"‸\", which is not in the text.",
        "Annotation 1 quotes \"early\", which is not in the text.",
      ),
    )
    assertEquals(
      result.annotated.annotations,
      List(Annotation(Span(7, 11), 2)),
    )
    assert(!result.complete)

  test(
    "Annotations quoted in the order they begin anchor back to where they are",
  ):
    val text      = "She always is late, and always will be."
    val annotated = AnnotatedText
      .of(
        text,
        List(
          Annotation(
            List(Span(4, 10), Span.point(14)),
            "move",
          ),
          Annotation(Span(24, 30), "again"),
          Annotation(Span(31, 38), "tense"),
        ),
      )
      .toOption
      .get
    val quoted = Anchoring.quote(annotated)
    assertEquals(
      quoted.map(_.quote),
      List(
        "always … is ‸late,",
        "always",
        "will be",
      ),
    )
    assertEquals(
      Anchoring.anchor(text, quoted),
      Anchored(annotated, Nil),
    )
