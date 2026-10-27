package com.alecdorrington.annotatedtextformat.dom

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
