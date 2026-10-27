package com.alecdorrington.annotatedtextformat.dom

import munit.FunSuite

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
