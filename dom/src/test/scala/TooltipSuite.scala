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
