package com.alecdorrington.annotatedtextformat.dom

import munit.FunSuite

class ColourSuite extends FunSuite:

  test(
    "The forms of a colour in CSS are colours: hex, functions of numbers, names, the system's colours, and custom properties falling back on a colour",
  ):
    List(
      "#14b8a6",
      "#abc",
      "#14b8a6cc",
      "teal",
      "RebeccaPurple",
      "transparent",
      "Canvas",
      "Highlight",
      "rgb(20 184 166 / 50%)",
      "hsl(170, 80%, 40%)",
      "oklch(70% 0.1 200deg / none)",
      "rgb(1, 2, 3)",
      "rgba(1, 2, 3, 0.5)",
      "rgb(1 2 3 / 50%)",
      "hwb(120 10% 20%)",
      "var(--critic)",
      "var(--x, #fff)",
      "VAR(--x)",
      "var(--x, rgb(1 2 3))",
    ).foreach(css =>
      assertEquals(
        Colour.parse(css).map(_.css),
        Some(css),
        css,
      ),
    )

  test(
    "Anything else is not, nor a function missing arguments or set apart unlike, so that nothing else can go into a style",
  ):
    List(
      "red,red),url(https://x/t),linear-gradient(red",
      "url(https://x/t)",
      "red; background: blue",
      "var(--a), url(x)",
      "rgb(1, 2, 3)) url(x",
      "",
      "bogus",
      "rgb(from red r g b)",
      "rgb(1 2 three)",
      "rgb(1)",
      "hsl(1 2)",
      "rgb(1, 2 3)",
      "rgb(1 2 3 4)",
      "var(--x, url(y))",
      "var(--x, red), url(y)",
    ).foreach(css => assertEquals(Colour.parse(css), None, css))
    val colour = Colour("red,red),url(https://x/t),linear-gradient(red")
    assertEquals(colour.css, "currentcolor")
    assertEquals(Colour("bogus").css, "currentcolor")
    assert(!Style.of(List(colour -> 0)).exists(_._2.contains("url(")))

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
      assertEquals(
        Colour.parse("v\\61r(--a), junk"),
        None,
      )
      assert(Colour.parse("color-mix(in srgb, red 50%, blue)").isDefined)
    finally global.CSS = scala.scalajs.js.undefined
