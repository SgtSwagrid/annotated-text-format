package com.alecdorrington.annotatedtextformat.dom

import munit.FunSuite

class ColourSuite extends FunSuite:

  test("The forms of a colour in CSS are colours"):
    List(
      "#14b8a6",
      "#abc",
      "#14b8a6cc",
      "teal",
      "rgb(20 184 166 / 50%)",
      "hsl(170, 80%, 40%)",
      "var(--critic)",
    ).foreach(css =>
      assertEquals(
        Colour.parse(css).map(_.css),
        Some(css),
      ),
    )

  test("Anything else is not, so that nothing else can go into a style"):
    List(
      "red,red),url(https://x/t),linear-gradient(red",
      "url(https://x/t)",
      "red; background: blue",
      "var(--a), url(x)",
      "rgb(1, 2, 3)) url(x",
      "",
    ).foreach(css => assertEquals(Colour.parse(css), None))

  test("A colour that is not one is the colour of the text"):
    val colour = Colour("red,red),url(https://x/t),linear-gradient(red")
    assertEquals(colour.css, "currentcolor")
    assert(!Style.of(List(colour -> 0)).exists(_._2.contains("url(")))
