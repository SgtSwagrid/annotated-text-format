package com.alecdorrington.annotatedtextformat.dom

import munit.FunSuite

class StyleSuite extends FunSuite:

  private val teal = Colour("#14b8a6")
  private val grey = Colour("#94a3b8")

  test(
    "A highlight is an underline in its lane, over a tint, and text under none is not drawn",
  ):
    assertEquals(Style.of(Nil), Nil)
    assertEquals(
      Style.of(List(teal -> 1)).toMap,
      Map(
        "background-image" ->
          "linear-gradient(#14b8a6, #14b8a6), linear-gradient(color-mix(in srgb, #14b8a6 20.0%, transparent), color-mix(in srgb, #14b8a6 20.0%, transparent))",
        "background-size"     -> "100% 2px, 100% 100%",
        "background-position" -> "0 calc(100% - 2px), 0 0",
        "background-repeat"   -> "no-repeat, no-repeat",
      ),
    )

  test(
    "Overlapping highlights split the tint between them, so as to be no darker",
  ):
    val image = Style
      .of(List(teal -> 0, grey -> 1))
      .toMap
      .apply("background-image")
    assertEquals("10.6%".r.findAllIn(image).size, 4)

  test("An application's own layer is drawn with the highlight's"):
    val shade = Style.Layer(
      Style.fill(Colour("grey")),
      "100% 100%",
      "0 0",
    )
    val drawn = Style.declarations(Style.layers(List(teal -> 0)) :+ shade).toMap
    assertEquals(
      drawn("background-image").split("linear-gradient").length - 1,
      3,
    )
    assertEquals(
      drawn("background-repeat"),
      "no-repeat, no-repeat, no-repeat",
    )
    assertEquals(Style.declarations(Nil), Nil)
