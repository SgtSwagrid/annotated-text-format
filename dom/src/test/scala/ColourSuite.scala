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

  test("A colour's function takes all its arguments, set apart alike"):
    List(
      "rgb(1)",
      "hsl(1 2)",
      "rgb(1, 2 3)",
      "rgb(1 2 3 4)",
    ).foreach(css => assertEquals(Colour.parse(css), None, css))
    List(
      "rgb(1, 2, 3)",
      "rgba(1, 2, 3, 0.5)",
      "rgb(1 2 3 / 50%)",
      "hwb(120 10% 20%)",
    ).foreach(css => assert(Colour.parse(css).isDefined, css))

  test("A custom property may fall back on a colour, and nothing else"):
    assert(Colour.parse("var(--x, #fff)").isDefined)
    assert(Colour.parse("VAR(--x)").isDefined)
    assert(Colour.parse("var(--x, rgb(1 2 3))").isDefined)
    assertEquals(Colour.parse("var(--x, url(y))"), None)
    assertEquals(
      Colour.parse("var(--x, red), url(y)"),
      None,
    )

  test("The system's colours are colours"):
    assert(Colour.parse("Canvas").isDefined)
    assert(Colour.parse("Highlight").isDefined)

  test(
    "A browser that takes anything holding a custom property is not asked of one",
  ):
    val global = scala.scalajs.js.Dynamic.global.globalThis
    global.CSS = scala
      .scalajs
      .js
      .Dynamic
      .literal(supports = (_: String, _: String) => true)
    try
      assertEquals(Colour.parse("var(--a), junk"), None)
      assert(Colour.parse("color-mix(in srgb, red 50%, blue)").isDefined)
    finally global.CSS = scala.scalajs.js.undefined
