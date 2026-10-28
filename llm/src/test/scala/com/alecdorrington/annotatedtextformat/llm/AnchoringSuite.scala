package com.alecdorrington.annotatedtextformat.llm

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Annotation, Span}
import munit.FunSuite

class AnchoringSuite extends FunSuite:

  /** The spans at which each of `quotes` is found in `text`, in order. */
  private def spans(text: String, quotes: String*): List[List[Span]] =
    anchored(text, quotes*).annotated.annotations.map(_.spans)

  private def anchored(text: String, quotes: String*): Anchored[Int] =
    QuotedAnnotation.anchor(
      text,
      quotes.toList.zipWithIndex.map(QuotedAnnotation(_, _)),
    )

  private def parts(text: String, quotes: String*): List[List[String]] =
    anchored(text, quotes*).annotated.annotations.map(_.in(text))

  private def reasons(text: String, quotes: String*): List[Unplaced.Reason] =
    anchored(text, quotes*).unplaced.map(_.reason)

  /** The annotations of `annotated`, quoted and then found again. */
  private def requoted[A](annotated: AnnotatedText[A]): List[Annotation[A]] =
    QuotedAnnotation
      .anchor(
        annotated.text,
        QuotedAnnotation.quote(annotated),
      )
      .annotated
      .annotations

  test(
    "A quote is found from where the one before it begins, nested in it or after it, and at the same place only if nowhere else",
  ):
    assertEquals(
      spans("the cat and the dog", "the", "the"),
      List(List(Span(0, 3)), List(Span(12, 15))),
    )
    assertEquals(
      spans("the big dog", "the big dog", "big"),
      List(List(Span(0, 11)), List(Span(4, 7))),
    )
    assertEquals(
      spans(
        "The cat sat. The dog ran.",
        "The cat sat.",
        "The",
      ),
      List(List(Span(0, 12)), List(Span(0, 3))),
    )
    assertEquals(
      spans("a dog", "dog", "dog"),
      List(List(Span(2, 5)), List(Span(2, 5))),
    )

  test(
    "A quote out of order is found where it occurs only once, and the search goes on from where it was",
  ):
    assertEquals(
      spans("She is late.", "late", "She"),
      List(List(Span(7, 11)), List(Span(0, 3))),
    )
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

  test(
    "A quote matches in any case, folded alike on every platform, and any spacing, apostrophe, full stop or line break, though as written first",
  ):
    List(
      ("Paris is nice. I love paris.", "paris", List(List("paris"))),
      ("I don’t  KNOW.", "i don't know", List(List("I don’t  KNOW"))),
      ("She is late.", "late .", List(List("late."))),
      ("She is late .", "late.", List(List("late ."))),
      ("one\r\ntwo three", "one two", List(List("one\r\ntwo"))),
      ("KIZ", "kiz", List(List("KIZ"))),
      ("kız", "kiz", Nil),
    ).foreach((text, quote, found) =>
      assertEquals(parts(text, quote), found, quote),
    )

  test(
    "A quote is found as whole words, spaced where it spaces them, unless they are not asked for or their script does not space them",
  ):
    assertEquals(
      spans("This is is wrong.", "is is"),
      List(List(Span(5, 10))),
    )
    assertEquals(
      spans("Breathe the the air", "the the"),
      List(List(Span(8, 15))),
    )
    assertEquals(parts("concatenate", "con cat"), Nil)
    assertEquals(
      spans("concatenate the cat", "cat"),
      List(List(Span(16, 19))),
    )
    assertEquals(spans("l'eau", "eau"), Nil)
    assertEquals(
      QuotedAnnotation
        .anchor(
          "concatenate",
          List(QuotedAnnotation("cat", ())),
          Matching(wholeWords = false),
        )
        .annotated
        .annotations
        .map(_.spans),
      List(List(Span(3, 6))),
    )
    assertEquals(parts("我的猫很可爱", "猫"), List(List("猫")))
    assertEquals(
      parts("I love it 😀 so", "it 😀"),
      List(List("it 😀")),
    )
    assertEquals(parts("名字𠀋好", "𠀋"), List(List("𠀋")))

  test(
    "The parts of a quote are one annotation, as are words to move and where they belong, unless the text holds the ellipsis",
  ):
    assertEquals(
      parts(
        "Ich stehe morgen früh auf.",
        "stehe … auf",
      ),
      List(List("stehe", "auf")),
    )
    assertEquals(
      spans(
        "She always is late.",
        "always … is ‸late",
      ),
      List(List(Span(4, 10), Span.point(14))),
    )
    assertEquals(
      parts("Wait ... what?", "Wait ... what"),
      List(List("Wait ... what")),
    )

  test(
    "A caret marks the point at which it stands, beside the space it is beside, and at a part's edge keeps its side and its word whole",
  ):
    List(
      ("She is late.", "is ‸late", 7),
      ("She is late.", "is‸ late", 6),
      ("She is late.", "is ‸ late", 7),
      ("chocolate is late", "‸late", 13),
      ("She is late.", "is‸", 6),
      ("She is late.", "is ‸", 7),
    ).foreach((text, quote, point) =>
      assertEquals(
        spans(text, quote),
        List(List(Span.point(point))),
        quote,
      ),
    )

  test(
    "A passage marked among words around it is told apart from the same words, and markers that mark none are taken as written",
  ):
    assertEquals(
      spans("the cat and the dog", "and ⟦the⟧ dog"),
      List(List(Span(12, 15))),
    )
    assertEquals(
      parts("a ⟧b", "a ⟧b"),
      List(List("a ⟧b")),
    )

  test(
    "Words around a passage may begin before the one before it, and those around adjacent parts may overlap",
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
    assertEquals(
      spans(
        "She always is late, and she always is late.",
        "she ⟦always⟧ is … is ‸late.",
      ),
      List(List(Span(28, 34), Span.point(38))),
    )

  test(
    "A quote not placed says why, empty, missing or out of order, as for the model that wrote it",
  ):
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
    assertEquals(
      reasons("a cat, a cat, a dog", "dog", "cat"),
      List(Unplaced.Reason.OutOfOrder),
    )

  test(
    "Annotations quote back to where they are, with words around them where needed",
  ):
    val annotated = AnnotatedText
      .of(
        "the cat and the dog, and the cat.",
        List(
          Annotation(Span(12, 15), "the"),
          Annotation(List(Span(4, 7), Span.point(20)), "move"),
          Annotation(Span(29, 32), "cat"),
          Annotation(Span(7, 11), " and"),
        ),
      )
      .toOption
      .get
    val sorted = annotated.annotations.sortBy(_.spans.head.start)
    assertEquals(requoted(annotated), sorted)
    assertEquals(
      QuotedAnnotation.quote(annotated).map(_.value),
      sorted.map(_.value),
    )

  test(
    "Every word, alone or all at once, and every word with a point after the next, quotes back to where it is",
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
      assertEquals(
        requoted(AnnotatedText.of(text, List(annotation)).toOption.get),
        List(annotation),
        annotation,
      ),
    )
    val annotated = AnnotatedText
      .of(text, words.map(Annotation(_, ())))
      .toOption
      .get
    assertEquals(
      requoted(annotated),
      annotated.annotations,
    )
