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
    val showing  = tooltip.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    assert(showing.update(text(
      document,
      "aufstehen: to get up; to stand up",
    )))
    assert(tooltip.isOpen)
    assertEquals(
      shown(tooltip),
      "aufstehen: to get up; to stand up",
    )

  test("A late answer is dropped once the tooltip closes or moves on"):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val tooltip  = Tooltip(document)
    val first    = tooltip.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    val second = tooltip.show(
      text(document, "early"),
      Box(60, 10, 40, 20),
    )
    assert(!first.isCurrent)
    assert(!first.update(text(document, "aufstehen")))
    assertEquals(shown(tooltip), "early")
    tooltip.hide()
    assert(!second.update(text(document, "früh")))
    tooltip.show(
      text(document, "early"),
      Box(60, 10, 40, 20),
    )
    assert(!second.update(text(document, "früh")))
    assertEquals(shown(tooltip), "early")

  test(
    "Hovering over an annotation shows what it is, until the pointer moves away",
  ):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9), Span(15, 18)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering[String](
      tooltip,
      (hovered, _) => hovered.values.headOption.map(text(document, _)),
    )
    hovering.add(decoration)
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
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(text(document, "get up")),
    )
    hovering.add(decoration)
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
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(card(document)),
      focusable = true,
    )
    hovering.add(decoration)
    val parts = decoration.elements(0)
    assertEquals(
      parts.map(part => Option(part.getAttribute("tabindex"))),
      List(Some("0"), None),
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
    val button = paragraph.lastElementChild.asInstanceOf[dom.HTMLElement]
    button.focus()
    first.focus()
    assert(tooltip.isOpen)
    button.focus()
    assert(!tooltip.isOpen)
    hovering.stop()
    assert(parts.forall(!_.hasAttribute("tabindex")))

  test("Tab from a tooltip's last part gives the focus back, and closes it"):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9), Span(15, 18)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(card(document)),
      focusable = true,
    )
    hovering.add(decoration)
    val first = decoration.elements(0).head.asInstanceOf[dom.HTMLElement]
    first.focus()
    Jsdom.press(document, "Tab")
    tooltip.focusables.last.focus()
    Jsdom.press(document, "Tab")
    assert(document.asInstanceOf[dom.HTMLDocument].activeElement eq first)
    assert(!tooltip.isOpen)
    hovering.stop()

  test("One hovering covers decorations as they are added and removed"):
    val page = Jsdom.element(
      "<div><p>Ich stehe früh auf.</p><p>Er steht spät auf.</p></div>",
    )
    val document = page.ownerDocument
    val first    = decorated(
      page.firstElementChild,
      List(Span(4, 9)),
    )
    val second   = decorated(page.lastElementChild, List(Span(3, 8)))
    val tooltip  = Tooltip(document)
    var answers  = List.empty[Tooltip.Showing]
    val hovering = Hovering[String](
      tooltip,
      (hovered, showing) =>
        answers :+= showing
        Some(text(
          document,
          s"${ hovered.values.mkString } in ${ hovered
              .decoration
              .annotated
              .text }",
        ))
      ,
      focusable = true,
    )
    hovering.add(first)
    hovering.add(second)
    Jsdom.move(first.elements(0).head.firstChild, 0, 0)
    assertEquals(
      shown(tooltip),
      "get up in Ich stehe früh auf.",
    )
    Jsdom.move(
      second.elements(0).head.firstChild,
      0,
      0,
    )
    assertEquals(
      shown(tooltip),
      "get up in Er steht spät auf.",
    )
    assert(!answers.head.update(text(document, "aufstehen")))
    assertEquals(
      shown(tooltip),
      "get up in Er steht spät auf.",
    )
    assert(answers.last.update(text(document, "aufstehen")))
    assertEquals(shown(tooltip), "aufstehen")
    hovering.remove(second)
    assert(!tooltip.isOpen)
    assert(!second.elements(0).head.hasAttribute("tabindex"))
    assert(first.elements(0).head.hasAttribute("tabindex"))
    Jsdom.move(
      second.elements(0).head.firstChild,
      0,
      0,
    )
    assert(!tooltip.isOpen)
    hovering.clear()
    assert(!first.elements(0).head.hasAttribute("tabindex"))
    Jsdom.move(first.elements(0).head.firstChild, 0, 0)
    assert(!tooltip.isOpen)
    hovering.add(first)
    Jsdom.move(first.elements(0).head.firstChild, 0, 0)
    assert(tooltip.isOpen)
    hovering.stop()
    assert(!tooltip.isOpen)

  test("A tooltip says what it is, and describes what it is open for"):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9)))
    val tooltip    = Tooltip(document, label = "Wörterbuch")
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(text(document, "get up")),
    )
    hovering.add(decoration)
    assertEquals(
      tooltip.host.tagName.toLowerCase,
      "atf-tooltip",
    )
    assertEquals(
      tooltip.host.getAttribute("role"),
      "dialog",
    )
    assertEquals(
      tooltip.host.getAttribute("aria-label"),
      "Wörterbuch",
    )
    assert(tooltip.id.nonEmpty)
    assertNotEquals(
      Tooltip(document, interactive = false).id,
      tooltip.id,
    )
    assertEquals(
      Tooltip(document, interactive = false).host.getAttribute("role"),
      "tooltip",
    )
    val element = decoration.elements(0).head
    Jsdom.move(element.firstChild, 0, 0)
    assertEquals(
      element.getAttribute("aria-describedby"),
      tooltip.id,
    )
    Jsdom.press(document, "Escape")
    assert(!element.hasAttribute("aria-describedby"))
    hovering.stop()

  test("A second tap closes the tooltip, and a tap's click goes no further"):
    val paragraph =
      Jsdom.element("<p><a href=\"#elsewhere\">Ich stehe früh auf.</a></p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(text(document, "get up")),
    )
    hovering.add(decoration)
    val word = decoration.elements(0).head.firstChild
    Jsdom.tap(word, 0, 0)
    assert(tooltip.isOpen)
    assert(!Jsdom.click(word, "click"))
    assert(Jsdom.click(word, "click"))
    Jsdom.tap(word, 0, 0)
    assert(!tooltip.isOpen)
    hovering.stop()

  test("Keys typed in a tooltip never reach the page"):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val tooltip  = Tooltip(document)
    val field    = document.createElement("input")
    tooltip.show(field, Box(10, 10, 40, 20))
    var heard = 0
    document.addEventListener("keydown", (_: dom.Event) => heard += 1)
    val window = document.asInstanceOf[js.Dynamic].defaultView
    field.dispatchEvent(
      js.Dynamic
        .newInstance(window.KeyboardEvent)(
          "keydown",
          js.Dynamic.literal(bubbles = true, composed = true, key = "k"),
        )
        .asInstanceOf[dom.Event],
    )
    assertEquals(heard, 0)

  test("A sheet whose rules may not be read is passed over"):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val sheet    = js.Dynamic.literal()
    js.Object
      .defineProperty(
        sheet.asInstanceOf[js.Object],
        "cssRules",
        js.Dynamic
          .literal(get =
            (() => throw js.JavaScriptException("SecurityError")): js.Function0[
              Any,
            ],
          )
          .asInstanceOf[js.PropertyDescriptor],
      )
    val tooltip = Tooltip(
      document,
      sheets = List(sheet.asInstanceOf[dom.CSSStyleSheet]),
    )
    tooltip.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    assertEquals(shown(tooltip), "get up")

  test("Annotations within a shadow root of the page show a tooltip too"):
    val page     = Jsdom.element("<div></div>")
    val document = page.ownerDocument
    val root     = page
      .asInstanceOf[js.Dynamic]
      .attachShadow(js.Dynamic.literal(mode = "open"))
    val paragraph = document.createElement("p")
    paragraph.textContent = "Ich stehe früh auf."
    root.appendChild(paragraph)
    val decoration = decorated(paragraph, List(Span(4, 9)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(card(document)),
      focusable = true,
    )
    hovering.add(decoration)
    val element = decoration.elements(0).head
    Jsdom.move(element.firstChild, 0, 0)
    assert(tooltip.isOpen)
    Jsdom.press(document, "Escape")
    assert(!tooltip.isOpen)
    element.asInstanceOf[dom.HTMLElement].focus()
    assert(tooltip.isOpen)
    Jsdom.press(document, "Tab")
    assertEquals(
      tooltip.focused.map(_.textContent),
      Some("known"),
    )
    Jsdom.press(document, "Escape")
    assert(!tooltip.isOpen)
    hovering.stop()

  test("Focus on a word whose tooltip is open keeps that showing"):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(Span(4, 9)))
    val tooltip    = Tooltip(document)
    var showings   = List.empty[Tooltip.Showing]
    val hovering   = Hovering[String](
      tooltip,
      (_, showing) =>
        showings :+= showing
        Some(text(document, "get up"))
      ,
      focusable = true,
    )
    hovering.add(decoration)
    val element = decoration.elements(0).head
    element.setAttribute("aria-describedby", "note-3")
    Jsdom.move(element.firstChild, 0, 0)
    assertEquals(
      element.getAttribute("aria-describedby"),
      s"note-3 ${ tooltip.id }",
    )
    element.asInstanceOf[dom.HTMLElement].focus()
    assertEquals(showings.size, 1)
    assert(showings.head.update(text(document, "aufstehen")))
    Jsdom.press(document, "Escape")
    assertEquals(
      element.getAttribute("aria-describedby"),
      "note-3",
    )
    hovering.stop()
