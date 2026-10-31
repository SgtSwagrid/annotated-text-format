package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{Annotation, TextSpan}
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

  test(
    "A tooltip shows its content in the top of the page, replaced while it is open, but not by a late answer once it closes or moves on",
  ):
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
    val replaced = Tooltip(document)
    val showing  = replaced.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    assert(showing.update(text(
      document,
      "aufstehen: to get up; to stand up",
    )))
    assert(replaced.isOpen)
    assertEquals(
      shown(replaced),
      "aufstehen: to get up; to stand up",
    )
    val late  = Tooltip(document)
    val first = late.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    val second = late.show(
      text(document, "early"),
      Box(60, 10, 40, 20),
    )
    assert(!first.isCurrent)
    assert(!first.update(text(document, "aufstehen")))
    assertEquals(shown(late), "early")
    late.hide()
    assert(!second.update(text(document, "früh")))
    late.show(
      text(document, "early"),
      Box(60, 10, 40, 20),
    )
    assert(!second.update(text(document, "früh")))
    assertEquals(shown(late), "early")

  test(
    "Hovering over an annotation shows what it is, until the pointer moves away",
  ):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(
      paragraph,
      List(TextSpan(4, 9), TextSpan(15, 18)),
    )
    val tooltip  = Tooltip(document)
    val hovering = Hovering[String](
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
      spans: List[TextSpan],
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

  test(
    "The pointer's presses and clicks, and keys typed, in a tooltip never reach the page, unless it is not interactive",
  ):
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
    var keys = 0
    document.addEventListener("keydown", (_: dom.Event) => keys += 1)
    val window = document.asInstanceOf[js.Dynamic].defaultView
    content
      .lastChild
      .dispatchEvent(
        js.Dynamic
          .newInstance(window.KeyboardEvent)(
            "keydown",
            js.Dynamic.literal(bubbles = true, composed = true, key = "k"),
          )
          .asInstanceOf[dom.Event],
      )
    assertEquals(keys, 0)
    val passive = Tooltip(document, interactive = false)
    val target  = document.createElement("button")
    passive.show(target, Box(10, 10, 40, 20))
    var clicks = 0
    document.addEventListener("click", (_: dom.Event) => clicks += 1)
    Jsdom.click(target, "click")
    assertEquals(clicks, 1)

  test(
    "A tooltip is styled within by the sheets given to it, passing over one whose rules may not be read",
  ):
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
    val unreadable = js.Dynamic.literal()
    js.Object
      .defineProperty(
        unreadable.asInstanceOf[js.Object],
        "cssRules",
        js.Dynamic
          .literal(get =
            (() => throw js.JavaScriptException("SecurityError")): js.Function0[
              Any,
            ],
          )
          .asInstanceOf[js.PropertyDescriptor],
      )
    val guarded = Tooltip(
      document,
      sheets = List(unreadable.asInstanceOf[dom.CSSStyleSheet]),
    )
    guarded.show(
      text(document, "get up"),
      Box(10, 10, 40, 20),
    )
    assertEquals(shown(guarded), "get up")

  test(
    "A tap on an annotation shows what it is, a second tap or one elsewhere closes it, and a tap's click goes no further",
  ):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(
      paragraph,
      List(TextSpan(4, 9), TextSpan(15, 18)),
    )
    val tooltip  = Tooltip(document)
    val hovering = Hovering[String](
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
    val linked =
      Jsdom.element("<p><a href=\"#elsewhere\">Ich stehe früh auf.</a></p>")
    val link    = decorated(linked, List(TextSpan(4, 9)))
    val tapped  = Tooltip(linked.ownerDocument)
    val tapping = Hovering[String](
      tapped,
      (_, _) => Some(text(linked.ownerDocument, "get up")),
    )
    tapping.add(link)
    val word = link.elements(0).head.firstChild
    Jsdom.tap(word, 0, 0)
    assert(tapped.isOpen)
    assert(!Jsdom.click(word, "click"))
    assert(Jsdom.click(word, "click"))
    Jsdom.tap(word, 0, 0)
    assert(!tapped.isOpen)
    tapping.stop()

  test(
    "The keyboard reaches each annotation, and Tab, Shift and Escape move through its tooltip, Tab from its last part giving the focus back",
  ):
    val paragraph =
      Jsdom.element("<p>Ich stehe früh auf. <button>Weiter</button></p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(
      paragraph,
      List(TextSpan(4, 9), TextSpan(15, 18)),
    )
    val tooltip  = Tooltip(document)
    val hovering = Hovering[String](
      tooltip,
      (_, _) => Some(card(document)),
      focus = Focus.Each(_ => true),
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
    first.focus()
    Jsdom.press(document, "Tab")
    tooltip.focusables.last.focus()
    Jsdom.press(document, "Tab")
    assert(document.asInstanceOf[dom.HTMLDocument].activeElement eq first)
    assert(!tooltip.isOpen)
    hovering.stop()
    assert(parts.forall(!_.hasAttribute("tabindex")))

  test(
    "One hovering covers decorations as they are added and removed, letting go of those restored, but not of those yet to be shown",
  ):
    val page = Jsdom.element(
      "<div><p>Ich stehe früh auf.</p><p>Er steht spät auf.</p></div>",
    )
    val document = page.ownerDocument
    val first    = decorated(
      page.firstElementChild,
      List(TextSpan(4, 9)),
    )
    val second = decorated(
      page.lastElementChild,
      List(TextSpan(3, 8)),
    )
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
      focus = Focus.Each(_ => true),
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
    val other = Jsdom.element(
      "<div><p>Ich stehe früh auf.</p><p>Er steht spät auf.</p></div>",
    )
    val apart = other.ownerDocument.createElement("p")
    apart.textContent = "Sie steht auf."
    val restored = decorated(
      other.firstElementChild,
      List(TextSpan(4, 9)),
    )
    val detached = decorated(apart, List(TextSpan(4, 9)))
    val held     = Tooltip(other.ownerDocument)
    val covering = Hovering[String](
      held,
      (_, _) => Some(text(other.ownerDocument, "get up")),
      focus = Focus.Each(_ => true),
    )
    covering.add(restored)
    covering.add(detached)
    restored.restore()
    covering.add(decorated(
      other.lastElementChild,
      List(TextSpan(3, 8)),
    ))
    other.appendChild(apart)
    Jsdom.move(
      detached.elements(0).head.firstChild,
      0,
      0,
    )
    assert(held.isOpen)
    covering.stop()

  test(
    "A tooltip says what it is, and describes what it is open for beside what else does, focus on its word keeping it showing",
  ):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = decorated(paragraph, List(TextSpan(4, 9)))
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
    val described  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val annotation = decorated(described, List(TextSpan(4, 9)))
    val focused    = Tooltip(described.ownerDocument)
    var showings   = List.empty[Tooltip.Showing]
    val focusing   = Hovering[String](
      focused,
      (_, showing) =>
        showings :+= showing
        Some(text(described.ownerDocument, "get up"))
      ,
      focus = Focus.Each(_ => true),
    )
    focusing.add(annotation)
    val word = annotation.elements(0).head
    word.setAttribute("aria-describedby", "note-3")
    Jsdom.move(word.firstChild, 0, 0)
    assertEquals(
      word.getAttribute("aria-describedby"),
      s"note-3 ${ focused.id }",
    )
    word.asInstanceOf[dom.HTMLElement].focus()
    assertEquals(showings.size, 1)
    assert(showings.head.update(text(described.ownerDocument, "aufstehen")))
    Jsdom.press(described.ownerDocument, "Escape")
    assertEquals(
      word.getAttribute("aria-describedby"),
      "note-3",
    )
    focusing.stop()

  test("Annotations within a shadow root of the page show a tooltip too"):
    val page     = Jsdom.element("<div></div>")
    val document = page.ownerDocument
    val root     = page
      .asInstanceOf[js.Dynamic]
      .attachShadow(js.Dynamic.literal(mode = "open"))
    val paragraph = document.createElement("p")
    paragraph.textContent = "Ich stehe früh auf."
    root.appendChild(paragraph)
    val decoration = decorated(paragraph, List(TextSpan(4, 9)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(card(document)),
      focus = Focus.Each(_ => true),
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

  /** `paragraph`, with `annotations` of its text, decorated. */
  private def annotated
    (
      paragraph: dom.Element,
      annotations: Annotation[String]*,
    )
    : Decoration[String] =
    val passage = Passage(paragraph)
    Decorator
      .decorate(
        passage,
        passage.annotate(annotations.toList).toOption.get,
        look,
      )
      .toOption
      .get

  /** The `tabindex` of the first element of each annotation of `decoration`. */
  private def stops(decoration: Decoration[String]): List[Option[String]] =
    decoration
      .annotated
      .annotations
      .indices
      .toList
      .map(index =>
        Option(decoration.elements(index).head.getAttribute("tabindex")),
      )

  private def active(document: dom.Document): dom.Element = document
    .asInstanceOf[dom.HTMLDocument]
    .activeElement

  test(
    "Roving, a decoration is one stop of Tab, the arrow keys moving among its annotations, and the last focused the stop",
  ):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val decoration = annotated(
      paragraph,
      Annotation(TextSpan(0, 3), "I"),
      Annotation(TextSpan(4, 9), "stand"),
      Annotation(TextSpan(10, 14), "early"),
    )
    val tooltip  = Tooltip(document)
    val hovering = Hovering[String](
      tooltip,
      (hovered, _) => hovered.values.headOption.map(text(document, _)),
      focus = Focus.Roving(_ => true),
    )
    hovering.add(decoration)
    assertEquals(
      stops(decoration),
      List(Some("0"), Some("-1"), Some("-1")),
    )
    val first = decoration.elements(0).head.asInstanceOf[dom.HTMLElement]
    first.focus()
    assertEquals(shown(tooltip), "I")
    Jsdom.press(document, "ArrowRight")
    assert(active(document) eq decoration.elements(1).head)
    assertEquals(shown(tooltip), "stand")
    assertEquals(
      stops(decoration),
      List(Some("-1"), Some("0"), Some("-1")),
    )
    Jsdom.press(document, "ArrowRight")
    Jsdom.press(document, "ArrowRight")
    assert(active(document) eq decoration.elements(2).head)
    Jsdom.press(document, "ArrowLeft")
    assert(active(document) eq decoration.elements(1).head)
    assertEquals(shown(tooltip), "stand")
    hovering.stop()
    assertEquals(
      stops(decoration),
      List(None, None, None),
    )

  test(
    "Roving in text that runs from right to left, the left arrow key moves to the next annotation",
  ):
    val paragraph  = Jsdom.element("""<p dir="rtl">אני קם מוקדם.</p>""")
    val document   = paragraph.ownerDocument
    val decoration = annotated(
      paragraph,
      Annotation(TextSpan(0, 3), "I"),
      Annotation(TextSpan(4, 6), "get up"),
    )
    val hovering = Hovering[String](
      Tooltip(document),
      (hovered, _) => hovered.values.headOption.map(text(document, _)),
      focus = Focus.Roving(_ => true),
    )
    hovering.add(decoration)
    decoration.elements(0).head.asInstanceOf[dom.HTMLElement].focus()
    Jsdom.press(document, "ArrowRight")
    assert(active(document) eq decoration.elements(0).head)
    Jsdom.press(document, "ArrowLeft")
    assert(active(document) eq decoration.elements(1).head)
    Jsdom.press(document, "ArrowRight")
    assert(active(document) eq decoration.elements(0).head)
    hovering.stop()

  test(
    "Only the annotations whose values are given take the focus, and none within a link",
  ):
    val paragraph =
      Jsdom.element("""<p>Ich <a href="#">stehe</a> früh auf.</p>""")
    val document   = paragraph.ownerDocument
    val decoration = annotated(
      paragraph,
      Annotation(TextSpan(0, 3), "I"),
      Annotation(TextSpan(4, 9), "stand"),
      Annotation(TextSpan(10, 14), "early"),
    )
    val hovering = Hovering[String](
      Tooltip(document),
      (hovered, _) => hovered.values.headOption.map(text(document, _)),
      focus = Focus.Each(_ != "I"),
    )
    hovering.add(decoration)
    assertEquals(
      stops(decoration),
      List(None, None, Some("0")),
    )
    hovering.stop()

  test(
    "Escape closes a tooltip that the pointer opened, and goes on to the page, but not one that the focus opened",
  ):
    val paragraph  = Jsdom.element("<p>Ich stehe früh auf.</p>")
    val document   = paragraph.ownerDocument
    val window     = document.asInstanceOf[js.Dynamic].defaultView
    val decoration = decorated(paragraph, List(TextSpan(4, 9)))
    val tooltip    = Tooltip(document)
    val hovering   = Hovering[String](
      tooltip,
      (_, _) => Some(text(document, "get up")),
      focus = Focus.Each(_ => true),
    )
    hovering.add(decoration)
    var heard    = 0
    val listener =
      (event: dom.KeyboardEvent) => if event.key == "Escape" then heard += 1
    window.addEventListener(
      "keydown",
      listener: js.Function1[dom.KeyboardEvent, Unit],
    )
    val element = decoration.elements(0).head
    Jsdom.move(element.firstChild, 0, 0)
    assert(tooltip.isOpen)
    Jsdom.press(document, "Escape")
    assert(!tooltip.isOpen)
    assertEquals(heard, 1)
    element.asInstanceOf[dom.HTMLElement].focus()
    assert(tooltip.isOpen)
    Jsdom.press(document, "Escape")
    assert(!tooltip.isOpen)
    assertEquals(heard, 1)
    hovering.stop()

  test(
    "A tooltip's own styles hold against the page's rules for every element",
  ):
    val tooltip = Tooltip(Jsdom.element("<p>x</p>").ownerDocument)
    val own     = tooltip
      .host
      .asInstanceOf[js.Dynamic]
      .shadowRoot
      .firstChild
      .textContent
      .asInstanceOf[String]
    List(
      "display: block !important",
      "position: fixed !important",
      "padding: 0 !important",
      "display: none !important",
    ).foreach(rule => assert(own.contains(rule), rule))
    assert(!own.contains("inset: auto !important"))

  /** Makes `element` measure as `box`, as jsdom measures every box as empty. */
  private def measured(element: dom.Element, box: Box): Unit = element
    .asInstanceOf[js.Dynamic]
    .updateDynamic("getBoundingClientRect")(
      (
        () =>
          js.Dynamic
            .literal(
              left = box.left,
              top = box.top,
              width = box.width,
              height = box.height,
            ),
      ): js.Function0[js.Dynamic],
    )

  /** The box covering the gap between `tooltip` and its line, if any. */
  private def cover(tooltip: Tooltip): Option[Box] = Option(
    tooltip
      .host
      .asInstanceOf[js.Dynamic]
      .shadowRoot
      .querySelector("atf-tooltip-bridge")
      .asInstanceOf[dom.HTMLElement],
  ).filter(_.style.display == "block")
    .map(bridge =>
      def px(property: String): Double =
        bridge.style.getPropertyValue(property).stripSuffix("px").toDouble
      Box(
        px("left"),
        px("top"),
        px("width"),
        px("height"),
      ),
    )

  test(
    "An interactive tooltip covers the gap to its line, above or below, and one that is not leaves it",
  ):
    val document = Jsdom.element("<p>x</p>").ownerDocument
    val tooltip  = Tooltip(document)
    measured(tooltip.host, Box(0, 0, 200, 100))
    assertEquals(cover(tooltip), None)
    tooltip.show(
      text(document, "get up"),
      Box(400, 400, 100, 20),
    )
    assertEquals(
      cover(tooltip),
      Some(Box(350, 394, 200, 6)),
    )
    assertEquals(shown(tooltip), "get up")
    tooltip.show(
      text(document, "early"),
      Box(400, 20, 100, 20),
    )
    assertEquals(
      cover(tooltip),
      Some(Box(350, 40, 200, 6)),
    )
    val passive = Tooltip(document, interactive = false)
    measured(passive.host, Box(0, 0, 200, 100))
    passive.show(
      text(document, "get up"),
      Box(400, 400, 100, 20),
    )
    assertEquals(cover(passive), None)
