package com.alecdorrington.annotatedtextformat.dom

/** A colour in which annotations are highlighted: any colour of CSS. */
opaque type Colour = String

object Colour:

  /** The colour that CSS writes as `css`, as `#14b8a6` or `teal`. */
  def apply(css: String): Colour = css

  extension (colour: Colour)

    /** It, as CSS writes it. */
    def css: String = colour

    /**
      * It, with only `share` of its opacity, from `0` to `1`, to a tenth of a
      * percent, written alike on the JVM and in JavaScript, which would write
      * `20.0` as `20`.
      */
    def faded(share: Double): String =
      val tenths = math.round(share * 1000)
      s"color-mix(in srgb, $colour ${ tenths / 10 }.${ tenths %
          10 }%, transparent)"
