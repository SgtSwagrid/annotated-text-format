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
  private def texts(element: dom.Element, kind: String = "atf"): List[String] =
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
      paragraph.querySelector(".atf").getAttribute("data-atf"),
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
    assert(paragraph.querySelector(".atf").classList.contains("atf-hidden"))
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
      paragraph.querySelectorAll(".atf-point").length,
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
      paragraph.querySelector(".atf"),
      "mouseenter",
    )
    assertEquals(
      texts(paragraph, "atf-lit"),
      List("big", " dog"),
    )
    Jsdom.send(
      paragraph.querySelector(".atf"),
      "mouseleave",
    )
    assertEquals(texts(paragraph, "atf-lit"), Nil)
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
    val pieces = paragraph.querySelectorAll(".atf")
    assertEquals(
      List.tabulate(pieces.length)(pieces(_).getAttribute("data-atf")),
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

  test("Words put in at a passage's ends, two at one point, keep their order"):
    val paragraph  = Jsdom.element("<p>cat sat</p>")
    val decoration = decorate(
      paragraph,
      translated,
      Annotation(Span.point(0), List("The")),
      Annotation(Span.point(7), List("down")),
      Annotation(Span.point(7), List("quietly")),
    )
    assertEquals(
      paragraph.textContent,
      "The cat sat down quietly",
    )
    decoration.restore()
    assertEquals(paragraph.innerHTML, "cat sat")

  test("Of two overlapping replacements, only the first is shown"):
    val paragraph  = Jsdom.element("<p>get up early</p>")
    val decoration = decorate(
      paragraph,
      translated,
      Annotation(Span(0, 6), List("stehe")),
      Annotation(Span(4, 12), List("früh")),
    )
    assertEquals(paragraph.textContent, "stehe early")
    decoration.restore()
    assertEquals(paragraph.innerHTML, "get up early")

  test(
    "Restoring twice, or text with line ends of two characters, gives the page back",
  ):
    // The HTML parser would turn the line end into one character, so the text
    // goes in as it is.
    val paragraph = Jsdom.element("<pre></pre>")
    paragraph.appendChild(paragraph.ownerDocument.createTextNode("one\r\ntwo"))
    val original   = paragraph.innerHTML
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(Span(0, 8), "lines"),
    )
    decoration.restore()
    decoration.restore()
    assertEquals(paragraph.innerHTML, original)

  test("A word put in a script that does not space its words takes no space"):
    val paragraph = Jsdom.element("<p>我的猫。</p>")
    decorate(
      paragraph,
      translated,
      Annotation(Span.point(3), List("狗")),
    )
    assertEquals(paragraph.textContent, "我的猫狗。")

  test("A word put in beside an element that is not prose goes beside it"):
    val paragraph  = Jsdom.element("<p>Run <code>npm</code> now</p>")
    val passage    = Passage(paragraph, _.tagName == "CODE")
    val decoration = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(Annotation(Span.point(5), List("again"))))
          .toOption
          .get,
        translated,
      )
      .toOption
      .get
    assertEquals(
      paragraph.textContent,
      "Run npm again now",
    )
    decoration.restore()
    assertEquals(
      paragraph.innerHTML,
      "Run <code>npm</code> now",
    )

  test("Words across a line break are wrapped on either side of it"):
    val paragraph  = Jsdom.element("<p>one<br>two</p>")
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(Span(0, 7), "both"),
    )
    assertEquals(texts(paragraph), List("one", "two"))
    decoration.restore()
    assertEquals(paragraph.innerHTML, "one<br>two")

  test("An application's own layers are drawn beneath the highlight"):
    val shaded = Look[String](
      _ => Colour("teal"),
      beneath = _ =>
        List(Style.Layer(
          Style.fill(Colour("grey")),
          "100% 100%",
          "0 0",
        )),
    )
    val paragraph = Jsdom.element("<p>She is late.</p>")
    decorate(
      paragraph,
      shaded,
      Annotation(Span(7, 11), "late"),
    )
    val image = paragraph
      .querySelector(".atf")
      .asInstanceOf[dom.HTMLElement]
      .style
      .getPropertyValue("background-image")
    assert(image.contains("grey"), image)

  test("Hiding an annotation under the pointer puts its light out"):
    val paragraph  = Jsdom.element("<p>She is late.</p>")
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(Span(7, 11), "late"),
    )
    Jsdom.send(
      paragraph.querySelector(".atf"),
      "mouseenter",
    )
    assertEquals(
      texts(paragraph, "atf-lit"),
      List("late"),
    )
    decoration.hide(0)
    assertEquals(texts(paragraph, "atf-lit"), Nil)

  test("Every element put in holds the indices of its annotations"):
    val paragraph = Jsdom.element("<p>I get up early.</p>")
    decorate(
      paragraph,
      translated,
      Annotation(
        List(Span(2, 8), Span.point(14)),
        List("stehe", "auf"),
      ),
    )
    val tagged = paragraph.querySelectorAll(".atf")
    assertEquals(
      List.tabulate(tagged.length)(tagged(_).getAttribute("data-atf")),
      List("0", "0"),
    )

  test(
    "Where the system forces its own colours, highlighted text is underlined",
  ):
    val paragraph = Jsdom.element("<p>She is late.</p>")
    decorate(
      paragraph,
      kept,
      Annotation(Span(7, 11), "late"),
    )
    decorate(
      paragraph.ownerDocument.createElement("p").asInstanceOf[dom.Element],
      kept,
    )
    val sheets = paragraph.ownerDocument.querySelectorAll("#atf-forced-colours")
    assertEquals(sheets.length, 1)
    assert(sheets(0).textContent.contains("forced-colors"))

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
