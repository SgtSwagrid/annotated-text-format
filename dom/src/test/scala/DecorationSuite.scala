package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, Problem, Span,
}
import munit.FunSuite
import org.scalajs.dom

class DecorationSuite extends FunSuite:

  /** Each annotation shows the parts of its value in place of its spans. */
  private val translated: Look[List[String]] = Look(
    _ => Colour("teal"),
    (pieces, part) => pieces.lift(part),
  )

  /** Each annotation keeps its text. */
  private val kept: Look[String] = Look(_ => Colour("teal"))

  private def decorate[A]
    (
      element: dom.Element,
      look: Look[A],
      annotations: Annotation[A]*,
    )
    : Decoration[A] =
    val passage = Passage(element)
    Decorator
      .decorate(
        passage,
        passage.annotate(annotations.toList).toOption.get,
        look,
      )
      .toOption
      .get

  /** The texts of the elements of the class `kind` beneath `element`. */
  private def texts
    (
      element: dom.Element,
      kind: String = "annotation",
    )
    : List[String] =
    val found = element.querySelectorAll(s".$kind")
    List.tabulate(found.length)(found(_).textContent)

  test("A highlight wraps its words, keeping the text, until it is taken away"):
    val paragraph  = Jsdom.element("<p>She always is late.</p>")
    val page       = paragraph.firstChild
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(Span(4, 10), "move"),
    )
    assertEquals(
      paragraph.textContent,
      "She always is late.",
    )
    assertEquals(texts(paragraph), List("always"))
    assertEquals(
      paragraph.querySelector(".annotation").getAttribute("data-annotation"),
      "0",
    )
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "She always is late.",
    )
    assert(paragraph.firstChild eq page)

  test("A translation reads in place of its words, and as them while hidden"):
    val paragraph  = Jsdom.element("<p>The apple is red.</p>")
    val page       = paragraph.firstChild.asInstanceOf[dom.Text]
    val decoration = decorate(
      paragraph,
      translated,
      Annotation(Span(0, 9), List("Der Apfel")),
    )
    assertEquals(
      paragraph.textContent,
      "Der Apfel is red.",
    )
    // The page's own text node stays where it was, emptied, as the page may hold on to it.
    assert(page.parentNode eq paragraph)
    assertEquals(page.data, "")
    decoration.hide(0)
    assertEquals(
      paragraph.textContent,
      "The apple is red.",
    )
    assert(
      paragraph
        .querySelector(".annotation")
        .classList
        .contains("annotation-hidden"),
    )
    decoration.show(0)
    assertEquals(
      paragraph.textContent,
      "Der Apfel is red.",
    )
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "The apple is red.",
    )
    assert(paragraph.firstChild eq page)
    assertEquals(page.data, "The apple is red.")

  test(
    "A piece put in after another word is spaced off it, and goes while hidden",
  ):
    val paragraph  = Jsdom.element("<p>I get up early.</p>")
    val decoration = decorate(
      paragraph,
      translated,
      Annotation(
        List(Span(2, 8), Span.point(14)),
        List("stehe", "auf"),
      ),
    )
    assertEquals(
      paragraph.textContent,
      "I stehe early auf.",
    )
    assertEquals(texts(paragraph), List("stehe", "auf"))
    decoration.hide(0)
    assertEquals(
      paragraph.textContent,
      "I get up early.",
    )
    decoration.show(0)
    decoration.restore()
    assertEquals(paragraph.innerHTML, "I get up early.")
    assertEquals(paragraph.childNodes.length, 1)

  test(
    "An emptied word takes a space away with it, so that no two are left together",
  ):
    val paragraph  = Jsdom.element("<p>I really like it.</p>")
    val decoration = decorate(
      paragraph,
      translated,
      Annotation(Span(2, 8), List("")),
    )
    assertEquals(paragraph.textContent, "I like it.")
    decoration.hide(0)
    assertEquals(
      paragraph.textContent,
      "I really like it.",
    )
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "I really like it.",
    )

  test("A point with nothing put in is an empty marker"):
    val paragraph  = Jsdom.element("<p>She always is late.</p>")
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(
        List(Span(4, 10), Span.point(14)),
        "move",
      ),
    )
    assertEquals(
      paragraph.textContent,
      "She always is late.",
    )
    assertEquals(
      paragraph.querySelectorAll(".annotation-point").length,
      1,
    )
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "She always is late.",
    )

  test(
    "Words across an element are wrapped on either side of it, and lit together",
  ):
    val paragraph  = Jsdom.element("<p>A <em>big</em> dog</p>")
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(Span(2, 9), "big dog"),
    )
    assertEquals(texts(paragraph), List("big", " dog"))
    assertEquals(
      paragraph.querySelector("em").textContent,
      "big",
    )
    Jsdom.send(
      paragraph.querySelector(".annotation"),
      "mouseenter",
    )
    assertEquals(
      texts(paragraph, "annotation-lit"),
      List("big", " dog"),
    )
    Jsdom.send(
      paragraph.querySelector(".annotation"),
      "mouseleave",
    )
    assertEquals(texts(paragraph, "annotation-lit"), Nil)
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "A <em>big</em> dog",
    )

  test(
    "Elements that are not prose stand as one placeholder, and are left alone",
  ):
    val paragraph = Jsdom.element("<p>Run <code>npm</code> now</p>")
    val passage   = Passage(paragraph, _.tagName == "CODE")
    assertEquals(
      passage.text,
      s"Run ${ Passage.placeholder } now",
    )
    val decoration = Decorator
      .decorate(
        passage,
        passage.annotate(List(Annotation(Span(4, 9), "run"))).toOption.get,
        kept,
      )
      .toOption
      .get
    assertEquals(texts(paragraph), List(" now"))
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "Run <code>npm</code> now",
    )

  test(
    "Overlapping annotations are cut where either ends, each piece under both",
  ):
    val paragraph  = Jsdom.element("<p>a b c</p>")
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(Span(0, 3), "x"),
      Annotation(Span(2, 5), "y"),
    )
    val pieces = paragraph.querySelectorAll(".annotation")
    assertEquals(
      List.tabulate(pieces.length)(pieces(_).getAttribute("data-annotation")),
      List("0", "0 1", "1"),
    )
    assertEquals(
      decoration.at(pieces(1).firstChild),
      List(0, 1),
    )
    decoration.hide(1)
    assertEquals(
      decoration.at(pieces(1).firstChild),
      List(0),
    )
    assertEquals(
      decoration.elements(1).map(_.textContent),
      List("b", " c"),
    )
    decoration.restore()
    assertEquals(paragraph.innerHTML, "a b c")

  test(
    "A point within a replaced span is left out, the replacement shown whole",
  ):
    val paragraph  = Jsdom.element("<p>The apple is red.</p>")
    val decoration = decorate(
      paragraph,
      translated,
      Annotation(Span(0, 9), List("Der Apfel")),
      Annotation(Span.point(4), List("x")),
    )
    assertEquals(
      paragraph.textContent,
      "Der Apfel is red.",
    )
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "The apple is red.",
    )

  test("Annotations of another text are refused"):
    val paragraph = Jsdom.element("<p>Some text.</p>")
    assertEquals(
      Decorator.decorate(
        Passage(paragraph),
        AnnotatedText.plain("Other text."),
        kept,
      ),
      Left(Problem.OtherText),
    )
