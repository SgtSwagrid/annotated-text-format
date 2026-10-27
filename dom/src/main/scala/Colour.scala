package com.alecdorrington.annotatedtextformat.dom

/**
  * A colour in which annotations are highlighted: one of the forms of a colour
  * in CSS, and nothing more, as it goes into styles, where anything else, such
  * as a `url(...)`, could make every viewer's browser fetch it.
  */
opaque type Colour = String

object Colour:

  /**
    * The colour that CSS writes as `css`, as `#14b8a6`, `teal`,
    * `rgb(20 184 166 / 50%)` or `var(--critic)`; or else the colour of the
    * text, `currentcolor`, if `css` is not one of those forms.
    */
  def apply(css: String): Colour = parse(css).getOrElse(text)

  /**
    * The colour that CSS writes as `css`, if it is one of the forms of a
    * colour: a hex colour, a function of numbers such as `rgb(...)` or
    * `hsl(...)`, a name, or a custom property, as `var(--name)`.
    */
  def parse(css: String): Option[Colour] =
    val trimmed = css.trim
    Option.when(forms.exists(_.matches(trimmed)))(trimmed)

  /** The colour of the text. */
  val text: Colour = "currentcolor"

  /** The forms of a colour, none of which may hold anything but a colour. */
  private val forms = List(
    """#(?:[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})""".r,
    """(?:rgba?|hsla?|hwb|lab|lch|oklab|oklch)\([-+0-9.%a-zA-Z\s,/]*\)""".r,
    """var\(\s*--[a-zA-Z0-9_-]+\s*\)""".r,
    """[a-zA-Z]+""".r,
  )

  extension (colour: Colour)

    /** It, as CSS writes it. */
    def css: String = colour

    /**
      * It, with only `share` of its opacity, from `0` to `1`, to a tenth of a
      * percent, written alike on the JVM and in JavaScript, which would write
      * `20.0` as `20`.
      */
    def faded(share: Double): String =
      val tenths  = math.round(share * 1000)
      val percent = s"${ tenths / 10 }.${ tenths % 10 }"
      s"color-mix(in srgb, $colour $percent%, transparent)"
