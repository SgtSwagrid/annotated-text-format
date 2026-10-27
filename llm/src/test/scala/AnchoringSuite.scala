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

  private def reasons(text: String, quotes: String*): List[Unplaced.Reason] =
    anchored(text, quotes*).unplaced.map(_.reason)

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

  test("A quote nested at the start of the one before it is found there"):
    assertEquals(
      spans(
        "The cat sat. The dog ran.",
        "The cat sat.",
        "The",
      ),
      List(List(Span(0, 12)), List(Span(0, 3))),
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

  test("A quote out of order leaves the search going on from where it was"):
    assertEquals(
      spans(
        "A cat sat. A cat ran. A dog ran. A cat hid.",
        "dog",
        "sat",
        "cat",
      ),
      List(
        List(Span(24, 27)),
        List(Span(6, 9)),
        List(Span(35, 38)),
      ),
    )

  test("A quote as written is found before one matching only loosely"):
    assertEquals(
      spans(
        "Paris is nice. I love paris.",
        "paris",
      ),
      List(List(Span(22, 27))),
    )

  test("A doubled word is found, though the first match lies within a word"):
    assertEquals(
      spans("This is is wrong.", "is is"),
      List(List(Span(5, 10))),
    )
    assertEquals(
      spans("Breathe the the air", "the the"),
      List(List(Span(8, 15))),
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

  test("Characters beyond the basic plane are found"):
    assertEquals(
      parts("I love it 😀 so", "it 😀"),
      List(List("it 😀")),
    )
    assertEquals(parts("名字𠀋好", "𠀋"), List(List("𠀋")))

  test("Case is folded alike on every platform, so a dotless i is not an i"):
    assertEquals(parts("kız", "kiz"), Nil)
    assertEquals(parts("KIZ", "kiz"), List(List("KIZ")))

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

  test("A caret at a part's edge keeps its side, and its word whole"):
    assertEquals(
      spans("chocolate is late", "‸late"),
      List(List(Span.point(13))),
    )
    assertEquals(
      spans("She is late.", "is‸"),
      List(List(Span.point(6))),
    )
    assertEquals(
      spans("She is late.", "is ‸"),
      List(List(Span.point(7))),
    )

  test(
    "A passage marked among words around it is told apart from the same words",
  ):
    assertEquals(
      spans("the cat and the dog", "and ⟦the⟧ dog"),
      List(List(Span(12, 15))),
    )

  test("Words to move, and where they belong, are one annotation"):
    assertEquals(
      spans(
        "She always is late.",
        "always … is ‸late",
      ),
      List(List(Span(4, 10), Span.point(14))),
    )

  test("Quotes across line breaks match"):
    assertEquals(
      parts("one\r\ntwo three", "one two"),
      List(List("one\r\ntwo")),
    )

  test("Markers that do not mark a passage are taken as written"):
    assertEquals(
      parts("a ⟧b", "a ⟧b"),
      List(List("a ⟧b")),
    )

  test("A quote not placed says why, as for the model that wrote it"):
    val result = anchored("She is late.", "‸", "early", "late")
    assertEquals(
      result.unplaced.map(_.index),
      List(0, 1),
    )
    assertEquals(
      result.unplaced.map(_.reason),
      List(
        Unplaced.Reason.Empty,
        Unplaced.Reason.Missing,
      ),
    )
    assert(
      result
        .unplaced(1)
        .message
        .startsWith("Annotation 1 quotes \"early\", which is not in the text."),
    )
    assertEquals(
      result.annotated.annotations,
      List(Annotation(Span(7, 11), 2)),
    )
    assert(!result.complete)

  test(
    "A quote only before the one before it, more than once, is out of order",
  ):
    assertEquals(
      reasons("a cat, a cat, a dog", "dog", "cat"),
      List(Unplaced.Reason.OutOfOrder),
    )

  test(
    "Annotations quote back to where they are, with words around them where needed",
  ):
    val text      = "the cat and the dog, and the cat."
    val annotated = AnnotatedText
      .of(
        text,
        List(
          Annotation(Span(12, 15), "the"),
          Annotation(List(Span(4, 7), Span.point(20)), "move"),
          Annotation(Span(29, 32), "cat"),
          Annotation(Span(7, 11), " and"),
        ),
      )
      .toOption
      .get
    val quoted = Anchoring.quote(annotated)
    val sorted = annotated.annotations.sortBy(_.spans.head.start)
    assertEquals(
      Anchoring.anchor(text, quoted).annotated.annotations,
      sorted,
    )
    assertEquals(
      quoted.map(_.value),
      sorted.map(_.value),
    )

  test(
    "A passage quoted with words around it is found after the one before, though they begin before it",
  ):
    assertEquals(
      spans(
        "the big dog and the big dog",
        "big",
        "the big ⟦dog⟧",
      ),
      List(List(Span(4, 7)), List(Span(8, 11))),
    )
    assertEquals(
      spans(
        "She is late. He is late too.",
        "late",
        "is ‸late",
      ),
      List(List(Span(7, 11)), List(Span.point(7))),
    )

  test("Adjacent parts may both have words around them, which may overlap"):
    assertEquals(
      spans(
        "She always is late, and she always is late.",
        "she ⟦always⟧ is … is ‸late.",
      ),
      List(List(Span(28, 34), Span.point(38))),
    )

  test(
    "Every word, and every word with a point after the next, quotes back to where it is",
  ):
    val text  = "the cat and the dog, and the cat sat on the mat by the cat."
    val words = """\S+"""
      .r
      .findAllMatchIn(text)
      .map(found => Span(found.start, found.end))
      .toList
    val annotations = words.map(Annotation(_, ())) ++
      words
        .zip(words.drop(2))
        .map((word, later) =>
          Annotation(List(word, Span.point(later.start)), ()),
        )
    annotations.foreach(annotation =>
      val annotated = AnnotatedText.of(text, List(annotation)).toOption.get
      assertEquals(
        Anchoring
          .anchor(text, Anchoring.quote(annotated))
          .annotated
          .annotations,
        List(annotation),
        annotation,
      ),
    )

  test("Every word of a text, annotated at once, quotes back to where it is"):
    val text  = "the cat and the dog, and the cat sat on the mat by the cat."
    val words = """\S+"""
      .r
      .findAllMatchIn(text)
      .map(found => Span(found.start, found.end))
      .toList
    val annotated = AnnotatedText
      .of(text, words.map(Annotation(_, ())))
      .toOption
      .get
    assertEquals(
      Anchoring.anchor(text, Anchoring.quote(annotated)).annotated.annotations,
      annotated.annotations,
    )
