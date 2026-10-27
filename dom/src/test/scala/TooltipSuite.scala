package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{Annotation, Span}
import munit.FunSuite
import org.scalajs.dom
import scala.scalajs.js

class TooltipSuite extends FunSuite:

  private val look: Look[String] = Look(_ => Colour("teal"))

  /** What the shadow root of `tooltip` shows. */
  private def shown(tooltip: Tooltip): String = tooltip
    .host
    .asInstanceOf[js.Dynamic]
    .shadowRoot
    .lastChild
    .textContent
    .asInstanceOf[String]

  private def text(document: dom.Document, content: String): dom.Node = document
    .createTextNode(content)

  test("A tooltip shows its content in the top of the page, and closes"):
    val paragraph = Jsdom.element("<p>Ich stehe auf.</p>")
    val document  = paragraph.ownerDocument
    val tooltip   = Tooltip(document, ".card { padding: 8px }")
    assert(!tooltip.isOpen)
    tooltip.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    assert(tooltip.isOpen)
    assert(tooltip.host.parentNode eq document.documentElement)
    assert(!tooltip.host.hasAttribute("hidden"))
    assertEquals(shown(tooltip), "get up")
    tooltip.hide()
    assert(!tooltip.isOpen)
    assert(tooltip.host.hasAttribute("hidden"))
    tooltip.remove()
    assert(tooltip.host.parentNode == null)

  test("A tooltip's content may be replaced while it is open"):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val tooltip  = Tooltip(document)
    tooltip.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    tooltip.update(text(
      document,
      "aufstehen: to get up; to stand up",
    ))
    assert(tooltip.isOpen)
    assertEquals(
      shown(tooltip),
      "aufstehen: to get up; to stand up",
    )

  test(
    "Hovering over an annotation shows what it is, until the pointer moves away",
  ):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val passage    = Passage(paragraph)
    val decoration = Decorator
      .decorate(
        passage,
        passage
          .annotate(List(Annotation(List(Span(4, 9), Span(15, 18)), "get up")))
          .toOption
          .get,
        look,
      )
      .toOption
      .get
    val tooltip  = Tooltip(document)
    val hovering = Hovering(
      decoration,
      tooltip,
      annotations =>
        annotations
          .headOption
          .map(index =>
            text(
              document,
              decoration.annotated.annotations(index).value,
            ),
          ),
    )
    val parts = decoration.elements(0)
    Jsdom.move(parts.last.firstChild, 0, 0)
    assert(tooltip.isOpen)
    assertEquals(shown(tooltip), "get up")
    Jsdom.move(paragraph.firstChild, 500, 500)
    assert(!tooltip.isOpen)
    Jsdom.move(parts.head.firstChild, 0, 0)
    assert(tooltip.isOpen)
    Jsdom.press(document, "Escape")
    assert(!tooltip.isOpen)
    hovering.stop()
    Jsdom.move(parts.head.firstChild, 0, 0)
    assert(!tooltip.isOpen)

  /** `paragraph`, with the annotation `get up` over `spans`, decorated. */
  private def decorated
    (
      paragraph: dom.Element,
      spans: List[Span],
    )
    : Decoration[String] =
    val passage = Passage(paragraph)
    Decorator
      .decorate(
        passage,
        passage.annotate(List(Annotation(spans, "get up"))).toOption.get,
        look,
      )
      .toOption
      .get

  /** A card of `document` with two buttons, as for rating an entry. */
  private def card(document: dom.Document): dom.Node =
    val card = document.createElement("div")
    List("known", "new").foreach(label =>
      val button = document.createElement("button")
      button.textContent = label
      card.appendChild(button),
    )
    card

  test("The pointer's presses and clicks in a tooltip never reach the page"):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val tooltip  = Tooltip(document)
    val content  = document.createElement("div")
    content.innerHTML = "<button>known</button><input>"
    tooltip.show(content, Box(10, 10, 40, 20))
    var heard = List.empty[String]
    List("mousedown", "click", "pointermove").foreach(kind =>
      document.addEventListener(kind, (_: dom.Event) => heard :+= kind),
    )
    val button = content.firstChild
    assert(!Jsdom.click(button, "mousedown"))
    assert(Jsdom.click(button, "click"))
    assert(Jsdom.click(button, "pointermove"))
    assert(Jsdom.click(content.lastChild, "mousedown"))
    assertEquals(heard, Nil)
    Jsdom.click(
      document.asInstanceOf[dom.HTMLDocument].body,
      "click",
    )
    assertEquals(heard, List("click"))

  test("A tooltip that is not interactive keeps nothing from the page"):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val tooltip  = Tooltip(document, interactive = false)
    val content  = document.createElement("button")
    tooltip.show(content, Box(10, 10, 40, 20))
    var heard = 0
    document.addEventListener("click", (_: dom.Event) => heard += 1)
    Jsdom.click(content, "click")
    assertEquals(heard, 1)

  test("A tooltip is styled within by the sheets given to it"):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val window   = document.asInstanceOf[js.Dynamic].defaultView
    val sheet    = js.Dynamic.newInstance(window.CSSStyleSheet)()
    sheet.replaceSync(".card { padding: 8px; }")
    val tooltip = Tooltip(
      document,
      sheets = List(sheet.asInstanceOf[dom.CSSStyleSheet]),
    )
    val root   = tooltip.host.asInstanceOf[js.Dynamic].shadowRoot
    val styled =
      if !js.isUndefined(root.adoptedStyleSheets) then
        root.adoptedStyleSheets.length.asInstanceOf[Int] == 1
      else root.textContent.asInstanceOf[String].contains(".card {")
    assert(styled)
    tooltip.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    assertEquals(shown(tooltip), "get up")

  test("A tap on an annotation shows what it is, and one elsewhere closes it"):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9), Span(15, 18)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering(
      decoration,
      tooltip,
      _ => Some(text(document, "get up")),
    )
    Jsdom.tap(
      decoration.elements(0).head.firstChild,
      0,
      0,
    )
    assert(tooltip.isOpen)
    Jsdom.tap(paragraph.firstChild, 500, 500)
    assert(!tooltip.isOpen)
    hovering.stop()

  test(
    "The keyboard reaches each annotation, and Tab, Shift and Escape move through its tooltip",
  ):
    val paragraph =
      Jsdom.element("<p>Ich stehe früh auf. <button>Weiter</button></p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9), Span(15, 18)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering(
      decoration,
      tooltip,
      _ => Some(card(document)),
      focusable = true,
    )
    val parts = decoration.elements(0)
    assertEquals(
      parts.map(_.getAttribute("tabindex")),
      List("0", "0"),
    )
    val first = parts.head.asInstanceOf[dom.HTMLElement]
    first.focus()
    assert(tooltip.isOpen)
    Jsdom.press(document, "Tab")
    assertEquals(
      tooltip.focused.map(_.textContent),
      Some("known"),
    )
    assert(tooltip.isOpen)
    Jsdom.press(document, "Tab", shift = true)
    assert(document.asInstanceOf[dom.HTMLDocument].activeElement eq first)
    assert(tooltip.isOpen)
    Jsdom.press(document, "Tab")
    Jsdom.press(document, "Escape")
    assert(document.asInstanceOf[dom.HTMLDocument].activeElement eq first)
    assert(!tooltip.isOpen)
    parts.last.asInstanceOf[dom.HTMLElement].focus()
    assert(tooltip.isOpen)
    paragraph.lastElementChild.asInstanceOf[dom.HTMLElement].focus()
    assert(!tooltip.isOpen)
    hovering.stop()
    assert(parts.forall(!_.hasAttribute("tabindex")))

  test("Tab from a tooltip's last part gives the focus back, and closes it"):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9), Span(15, 18)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering(
      decoration,
      tooltip,
      _ => Some(card(document)),
      focusable = true,
    )
    val first = decoration.elements(0).head.asInstanceOf[dom.HTMLElement]
    first.focus()
    Jsdom.press(document, "Tab")
    tooltip.focusables.last.focus()
    Jsdom.press(document, "Tab")
    assert(document.asInstanceOf[dom.HTMLDocument].activeElement eq first)
    assert(!tooltip.isOpen)
    hovering.stop()
