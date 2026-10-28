package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{AnnotatedText, Annotation, Span}
import munit.FunSuite
import org.scalajs.dom
import scala.scalajs.js

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
      Left(Refusal.OtherText),
    )

  test("Hiding one annotation leaves lit an element that another still lights"):
    val paragraph  = Jsdom.element("<p>a b c</p>")
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(Span(0, 3), "x"),
      Annotation(Span(2, 5), "y"),
    )
    Jsdom.send(
      paragraph.querySelectorAll(".atf")(1),
      "mouseenter",
    )
    decoration.hide(0)
    assertEquals(
      texts(paragraph, "atf-lit"),
      List("b", " c"),
    )

  test("Redrawing draws what the look gives now"):
    var shaded = false
    val look   = Look[String](
      _ => Colour("teal"),
      beneath = _ =>
        if shaded then
          List(Style.Layer(
            Style.fill(Colour("grey")),
            "100% 100%",
            "0 0",
          ))
        else Nil,
    )
    val paragraph  = Jsdom.element("<p>She is late.</p>")
    val decoration = decorate(
      paragraph,
      look,
      Annotation(Span(7, 11), "late"),
    )
    def image = paragraph
      .querySelector(".atf")
      .asInstanceOf[dom.HTMLElement]
      .style
      .getPropertyValue("background-image")
    assert(!image.contains("grey"))
    shaded = true
    decoration.redraw()
    assert(image.contains("grey"))

  test("The stylesheet for forced colours goes once no decoration uses it"):
    val both = Jsdom.element("<div><p>She is late.</p><p>He is too.</p></div>")
    val document = both.ownerDocument
    val first    = decorate(
      both.firstElementChild,
      kept,
      Annotation(Span(7, 11), "late"),
    )
    val second = decorate(
      both.lastElementChild,
      kept,
      Annotation(Span(3, 5), "is"),
    )
    def sheets = document.querySelectorAll("#atf-forced-colours").length
    assertEquals(sheets, 1)
    first.restore()
    assertEquals(sheets, 1)
    second.restore()
    assertEquals(sheets, 0)

  test("Within a shadow root, the stylesheet for forced colours goes there"):
    val host   = Jsdom.element("<div></div>")
    val shadow = host
      .asInstanceOf[js.Dynamic]
      .attachShadow(js.Dynamic.literal(mode = "open"))
    val inner = host.ownerDocument.createElement("p")
    inner.textContent = "She is late."
    shadow.appendChild(inner)
    decorate(
      inner,
      kept,
      Annotation(Span(7, 11), "late"),
    )
    assertEquals(
      shadow.querySelectorAll("#atf-forced-colours").length.asInstanceOf[Int],
      1,
    )
    assertEquals(
      host.ownerDocument.querySelectorAll("#atf-forced-colours").length,
      0,
    )

  test("A prefix that could not begin a class's name is refused"):
    val paragraph = Jsdom.element("<p>She is late.</p>")
    val passage   = Passage(paragraph)
    assertEquals(
      Decorator
        .decorate(
          passage,
          passage.annotate(List(Annotation(Span(7, 11), "late"))).toOption.get,
          kept,
          "my.notes",
        )
        .left
        .toOption,
      Some(Refusal.Unnamed("my.notes")),
    )
    assertEquals(paragraph.innerHTML, "She is late.")

  test("A text node as a passage's root is joined back together when restored"):
    val paragraph  = Jsdom.element("<p>She is late.</p>")
    val page       = paragraph.firstChild
    val passage    = Passage(page)
    val decoration = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(
            Annotation(Span(0, 3), "she"),
            Annotation(Span(4, 10), "is late"),
          ))
          .toOption
          .get,
        kept,
      )
      .toOption
      .get
    decoration.restore()
    assertEquals(paragraph.childNodes.length, 1)
    assert(paragraph.firstChild eq page)
    assertEquals(page.textContent, "She is late.")

  test("Restoring twice uses the stylesheet up no more than once"):
    val both  = Jsdom.element("<div><p>She is late.</p><p>He is too.</p></div>")
    val first = decorate(
      both.firstElementChild,
      kept,
      Annotation(Span(7, 11), "late"),
    )
    val second = decorate(
      both.lastElementChild,
      kept,
      Annotation(Span(3, 5), "is"),
    )
    def sheets = both
      .ownerDocument
      .querySelectorAll("#atf-forced-colours")
      .length
    first.restore()
    first.restore()
    assertEquals(sheets, 1)
    second.restore()
    assertEquals(sheets, 0)

  test(
    "In a fragment not yet in the document, the stylesheet goes in the document's head",
  ):
    val document = Jsdom.element("<p></p>").ownerDocument
    val fragment = document.createDocumentFragment()
    val inner    = document.createElement("p")
    inner.textContent = "She is late."
    fragment.appendChild(inner)
    val decoration = decorate(
      inner,
      kept,
      Annotation(Span(7, 11), "late"),
    )
    assertEquals(
      document.querySelectorAll("head #atf-forced-colours").length,
      1,
    )
    document.asInstanceOf[dom.HTMLDocument].body.appendChild(fragment)
    assertEquals(
      document.querySelectorAll("body #atf-forced-colours").length,
      0,
    )
    decoration.restore()
    assertEquals(
      document.querySelectorAll("#atf-forced-colours").length,
      0,
    )

  test("A page decorated a piece at a time reads as it is seen"):
    val page = Jsdom.element(
      "<div><p>Ich stehe früh auf.</p><p>Er steht spät auf.</p></div>",
    )
    val before = page.innerHTML
    val first  = decorate(
      page,
      translated,
      Annotation(List(Span(4, 9)), List("stand")),
    )
    val passage = Passage(page)
    assertEquals(
      passage.text,
      "Ich stand früh auf.\nEr steht spät auf.",
    )
    assertEquals(
      passage.original,
      "Ich stehe früh auf.\nEr steht spät auf.",
    )
    val second = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(
            Annotation(List(Span(23, 28)), List("stands")),
            Annotation(List(Span(6, 12)), List("x")),
          ))
          .toOption
          .get,
        translated,
        whole = true,
      )
      .toOption
      .get
    assertEquals(second.unplaced, Set(1))
    assertEquals(second.elements(1), Nil)
    assertEquals(
      Passage(page).text,
      "Ich stand früh auf.\nEr stands spät auf.",
    )
    second.restore()
    first.restore()
    assertEquals(page.innerHTML, before)

  test("A point with no text beside it is not placed whole"):
    val paragraph  = Jsdom.element("<p><input></p>")
    val passage    = Passage(paragraph)
    val decoration = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(Annotation(List(Span.point(0)), List("here"))))
          .toOption
          .get,
        translated,
        whole = true,
      )
      .toOption
      .get
    assertEquals(decoration.unplaced, Set(0))

  test("Each text node split is told of, with what was split off it"):
    val paragraph = Jsdom.element("<p>She always is late.</p>")
    val passage   = Passage(paragraph)
    var splits    = List.empty[(String, String)]
    Decorator
      .decorate(
        passage,
        passage.annotate(List(Annotation(List(Span(4, 10)), "x"))).toOption.get,
        kept,
        split = (node, rest) => splits :+= (node.data -> rest.data),
      )
      .toOption
      .get
    assertEquals(splits, List("She " -> " is late."))

  test("The classes that a look gives are on its elements while shown"):
    val paragraph = Jsdom.element("<p>She always is late.</p>")
    val look      = Look[String](
      _ => Colour("teal"),
      classes = value => List(value, "two words"),
    )
    val decoration = decorate(
      paragraph,
      look,
      Annotation(List(Span(4, 10)), "known"),
    )
    val element = decoration.elements(0).head
    assert(element.classList.contains("known"))
    assert(!element.classList.contains("two"))
    decoration.hide(0)
    assert(!element.classList.contains("known"))
    decoration.show(0)
    assert(element.classList.contains("known"))

  test("Decorations of one page may be restored in any order"):
    val paragraph = Jsdom.element("<p>She always is late.</p>")
    val page      = paragraph.firstChild
    val first     = decorate(
      paragraph,
      kept,
      Annotation(List(Span(4, 10)), "x"),
    )
    val second = decorate(
      paragraph,
      kept,
      Annotation(List(Span(14, 18)), "y"),
    )
    first.restore()
    second.restore()
    assertEquals(paragraph.childNodes.length, 1)
    assert(paragraph.firstChild eq page)
    assertEquals(
      page.textContent,
      "She always is late.",
    )

  test("A passage read before the page changed finds nothing there"):
    val paragraph  = Jsdom.element("<p>She always is late.</p>")
    val stale      = Passage(paragraph)
    val decoration = decorate(
      paragraph,
      kept,
      Annotation(List(Span(4, 10)), "x"),
    )
    assertEquals(stale.rangeOf(Span(14, 18)), None)
    val fresh = Passage(paragraph)
    assertEquals(
      fresh.rangeOf(Span(14, 18)).map(_.toString()),
      Some("late"),
    )
    assertEquals(
      fresh.offsetOf(
        decoration.elements(0).head.firstChild,
        2,
      ),
      Some(6),
    )

  test("Placed whole, an annotation drawn only in part is left out"):
    val page = Jsdom.element("<div><p>Ich stehe auf.</p><p>Er geht.</p></div>")
    val passage    = Passage(page)
    val decoration = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(
            Annotation(List(Span(4, 9)), List("stand")),
            Annotation(List(Span(6, 9)), List("x")),
            Annotation(List(Span.point(6)), List("y")),
            Annotation(List(Span(14, 15)), List("z")),
          ))
          .toOption
          .get,
        translated,
        whole = true,
      )
      .toOption
      .get
    assertEquals(decoration.unplaced, Set(1, 2, 3))
    assertEquals(texts(page), List("stand"))

  test("A split hook that throws is passed over, the page decorated whole"):
    val paragraph  = Jsdom.element("<p>She always is late.</p>")
    val passage    = Passage(paragraph)
    val decoration = Decorator
      .decorate(
        passage,
        passage.annotate(List(Annotation(List(Span(4, 10)), "x"))).toOption.get,
        kept,
        split = (_, _) => throw new RuntimeException("refused"),
      )
      .toOption
      .get
    assertEquals(texts(paragraph), List("always"))
    decoration.restore()
    assertEquals(
      paragraph.textContent,
      "She always is late.",
    )

  test("A look cannot give the decoration's own classes"):
    val paragraph  = Jsdom.element("<p>She always is late.</p>")
    val decoration = decorate(
      paragraph,
      Look[String](
        _ => Colour("teal"),
        classes = _ => List("atf-lit", "atf"),
      ),
      Annotation(List(Span(4, 10)), "x"),
    )
    val element = decoration.elements(0).head
    assert(!element.classList.contains("atf-lit"))
    decoration.hide(0)
    decoration.show(0)
    assert(element.classList.contains("atf"))

  test("A passage read before its nodes changed gives no range in them"):
    val paragraph = Jsdom.element("<p>The apple is red.</p>")
    val stale     = Passage(paragraph)
    decorate(
      paragraph,
      translated,
      Annotation(List(Span(0, 9)), List("Der Apfel")),
    )
    assertEquals(stale.rangeOf(Span(1, 3)), None)
    assertEquals(stale.rangeOf(Span.point(0)), None)

  test("A replacement beginning at a break between blocks is drawn whole"):
    val page       = Jsdom.element("<div><p>one</p><p>two</p></div>")
    val passage    = Passage(page)
    val decoration = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(Annotation(List(Span(3, 7)), List("zwei"))))
          .toOption
          .get,
        translated,
        whole = true,
      )
      .toOption
      .get
    assertEquals(decoration.unplaced, Set.empty[Int])
    assertEquals(texts(page), List("zwei"))

  test("Placed whole, one left out only for another left out is put back"):
    val paragraph = Jsdom.element("<p>Ich stehe auf.<input><input></p>")
    val document  = paragraph.ownerDocument
    paragraph.insertBefore(
      document.createTextNode(""),
      paragraph.lastChild,
    )
    val passage    = Passage(paragraph)
    val decoration = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(
            Annotation(
              List(Span(4, 9), Span.point(15)),
              List("stand", "x"),
            ),
            Annotation(List(Span(6, 12)), List("geht")),
          ))
          .toOption
          .get,
        translated,
        whole = true,
      )
      .toOption
      .get
    assertEquals(decoration.unplaced, Set(0))
    assertEquals(texts(paragraph), List("geht"))

  test(
    "An offset at an element's child that is not prose is where that child is",
  ):
    val paragraph = Jsdom.element("<p>Ich <input> auf</p>")
    assertEquals(
      Passage(paragraph).offsetOf(paragraph, 1),
      Some(4),
    )
