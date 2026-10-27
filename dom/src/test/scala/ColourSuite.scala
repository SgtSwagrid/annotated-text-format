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

  test("Only the named colours of CSS are names"):
    assertEquals(Colour("bogus").css, "currentcolor")
    assertEquals(
      Colour("RebeccaPurple").css,
      "RebeccaPurple",
    )
    assertEquals(
      Colour("transparent").css,
      "transparent",
    )

  test("A colour's function holds only numbers, with units or none"):
    assertEquals(
      Colour.parse("oklch(70% 0.1 200deg / none)").map(_.css),
      Some("oklch(70% 0.1 200deg / none)"),
    )
    assertEquals(
      Colour.parse("rgb(from red r g b)"),
      None,
    )
    assertEquals(Colour.parse("rgb(1 2 three)"), None)
