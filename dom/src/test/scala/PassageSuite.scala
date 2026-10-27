package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.Span
import munit.FunSuite
import org.scalajs.dom

class PassageSuite extends FunSuite:

  test("A line break reads as one"):
    assertEquals(
      Passage(Jsdom.element("<p>one<br>two</p>")).text,
      "one\ntwo",
    )

  test("Blocks are read apart, a line break between them"):
    assertEquals(
      Passage(Jsdom.element("<div><p>one</p><p>two</p>three</div>")).text,
      "one\ntwo\nthree",
    )

  test("Scripts, styles and fields to fill in are not prose, by default"):
    val placeholder = Passage.placeholder.toString
    assertEquals(
      Passage(Jsdom.element(
        "<div>hi<script>x()</script><style>p{}</style><textarea>t</textarea> there</div>",
      )).text,
      s"hi$placeholder$placeholder$placeholder there",
    )

  test("A text node is a passage of its own text"):
    val document = Jsdom.element("<p></p>").ownerDocument
    assertEquals(
      Passage(document.createTextNode("abc")).text,
      "abc",
    )

  test("What is skipped is skipped as well as what is not prose"):
    val placeholder = Passage.placeholder.toString
    assertEquals(
      Passage(
        Jsdom.element("<div>Run <code>npm</code><style>p{}</style> now</div>"),
        _.tagName == "CODE",
      ).text,
      s"Run $placeholder$placeholder now",
    )

  test("A whole document's head is not prose"):
    val document = Jsdom.element("<p>Hi</p>").ownerDocument
    document.asInstanceOf[dom.HTMLDocument].title = "Title"
    assert(!Passage(document).text.contains("Title"))

  test("Offsets in a page's nodes and ranges map to the passage's text"):
    val paragraph = Jsdom.element("<p>Ich <em>stehe</em> auf.</p>")
    val passage   = Passage(paragraph)
    val em        = paragraph.childNodes(1)
    assertEquals(
      passage.offsetOf(em.firstChild, 2),
      Some(6),
    )
    assertEquals(passage.offsetOf(paragraph, 1), Some(4))
    assertEquals(
      passage.offsetOf(paragraph, 3),
      Some(14),
    )
    val range = passage.rangeOf(Span(4, 9)).get
    assertEquals(range.toString(), "stehe")
    assertEquals(passage.spanOf(range), Some(Span(4, 9)))
    assertEquals(
      passage.rangeOf(Span(4, 9)).flatMap(passage.spanOf),
      Some(Span(4, 9)),
    )
