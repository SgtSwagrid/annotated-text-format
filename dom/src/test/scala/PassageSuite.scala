package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.TextSpan
import munit.FunSuite
import org.scalajs.dom

class PassageSuite extends FunSuite:

  test(
    "A line break reads as one, blocks are read apart, a line break between them, and a text node as its own text",
  ):
    assertEquals(
      Passage(Jsdom.element("<p>one<br>two</p>")).text,
      "one\ntwo",
    )
    val blocks = Jsdom.element("<div><p>one</p><p>two</p>three</div>")
    assertEquals(
      Passage(blocks).text,
      "one\ntwo\nthree",
    )
    assertEquals(
      Passage(blocks.ownerDocument.createTextNode("abc")).text,
      "abc",
    )

  test(
    "Scripts, styles, fields to fill in and a document's head are not prose, and what is skipped is skipped as well",
  ):
    val placeholder = Passage.placeholder.toString
    assertEquals(
      Passage(Jsdom.element(
        "<div>hi<script>x()</script><style>p{}</style><textarea>t</textarea> there</div>",
      )).text,
      s"hi$placeholder$placeholder$placeholder there",
    )
    val skipping =
      Jsdom.element("<div>Run <code>npm</code><style>p{}</style> now</div>")
    assertEquals(
      Passage(skipping, _.tagName == "CODE").text,
      s"Run $placeholder$placeholder now",
    )
    val document = skipping.ownerDocument
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
    val range = passage.rangeOf(TextSpan(4, 9)).get
    assertEquals(range.toString(), "stehe")
    assertEquals(
      passage.spanOf(range),
      Some(TextSpan(4, 9)),
    )
    assertEquals(
      passage.rangeOf(TextSpan(4, 9)).flatMap(passage.spanOf),
      Some(TextSpan(4, 9)),
    )
