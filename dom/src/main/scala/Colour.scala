package com.alecdorrington.annotatedtextformat.dom

/**
  * A colour in which annotations are highlighted: one of the forms of a colour
  * in CSS, and nothing more, as it goes into styles, where anything else, such
  * as a `url(...)`, could make every viewer's browser fetch it, and a colour
  * the browser does not know would drop the whole background it is a layer of.
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
    * colour: a hex colour; a function of a colour space, as `rgb(...)` or
    * `hsl(...)`, of numbers, with units or `none`; a named colour; or a custom
    * property, as `var(--name)`.
    */
  def parse(css: String): Option[Colour] =
    val trimmed = css.trim
    Option.when(
      names.contains(trimmed.toLowerCase) || forms.exists(_.matches(trimmed)),
    )(trimmed)

  /** The colour of the text. */
  val text: Colour = "currentcolor"

  /**
    * An argument of a function of a colour space: a number, with a unit, or
    * `none`.
    */
  private val argument =
    """(?:[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?(?:%|deg|rad|grad|turn)?|none)"""

  /** The forms of a colour but names, none of which may hold anything else. */
  private val forms = List(
    """#(?:[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})""".r,
    s"""(?i)(?:rgba?|hsla?|hwb|lab|lch|oklab|oklch)\\(\\s*$argument(?:(?:\\s*[,/]\\s*|\\s+)$argument)*\\s*\\)"""
      .r,
    """var\(\s*--[a-zA-Z0-9_-]+\s*\)""".r,
  )

  /** The named colours of CSS, and `transparent` and `currentcolor`. */
  private val names: Set[String] = Set(
    "aliceblue",
    "antiquewhite",
    "aqua",
    "aquamarine",
    "azure",
    "beige",
    "bisque",
    "black",
    "blanchedalmond",
    "blue",
    "blueviolet",
    "brown",
    "burlywood",
    "cadetblue",
    "chartreuse",
    "chocolate",
    "coral",
    "cornflowerblue",
    "cornsilk",
    "crimson",
    "cyan",
    "darkblue",
    "darkcyan",
    "darkgoldenrod",
    "darkgray",
    "darkgreen",
    "darkgrey",
    "darkkhaki",
    "darkmagenta",
    "darkolivegreen",
    "darkorange",
    "darkorchid",
    "darkred",
    "darksalmon",
    "darkseagreen",
    "darkslateblue",
    "darkslategray",
    "darkslategrey",
    "darkturquoise",
    "darkviolet",
    "deeppink",
    "deepskyblue",
    "dimgray",
    "dimgrey",
    "dodgerblue",
    "firebrick",
    "floralwhite",
    "forestgreen",
    "fuchsia",
    "gainsboro",
    "ghostwhite",
    "gold",
    "goldenrod",
    "gray",
    "green",
    "greenyellow",
    "grey",
    "honeydew",
    "hotpink",
    "indianred",
    "indigo",
    "ivory",
    "khaki",
    "lavender",
    "lavenderblush",
    "lawngreen",
    "lemonchiffon",
    "lightblue",
    "lightcoral",
    "lightcyan",
    "lightgoldenrodyellow",
    "lightgray",
    "lightgreen",
    "lightgrey",
    "lightpink",
    "lightsalmon",
    "lightseagreen",
    "lightskyblue",
    "lightslategray",
    "lightslategrey",
    "lightsteelblue",
    "lightyellow",
    "lime",
    "limegreen",
    "linen",
    "magenta",
    "maroon",
    "mediumaquamarine",
    "mediumblue",
    "mediumorchid",
    "mediumpurple",
    "mediumseagreen",
    "mediumslateblue",
    "mediumspringgreen",
    "mediumturquoise",
    "mediumvioletred",
    "midnightblue",
    "mintcream",
    "mistyrose",
    "moccasin",
    "navajowhite",
    "navy",
    "oldlace",
    "olive",
    "olivedrab",
    "orange",
    "orangered",
    "orchid",
    "palegoldenrod",
    "palegreen",
    "paleturquoise",
    "palevioletred",
    "papayawhip",
    "peachpuff",
    "peru",
    "pink",
    "plum",
    "powderblue",
    "purple",
    "rebeccapurple",
    "red",
    "rosybrown",
    "royalblue",
    "saddlebrown",
    "salmon",
    "sandybrown",
    "seagreen",
    "seashell",
    "sienna",
    "silver",
    "skyblue",
    "slateblue",
    "slategray",
    "slategrey",
    "snow",
    "springgreen",
    "steelblue",
    "tan",
    "teal",
    "thistle",
    "tomato",
    "turquoise",
    "violet",
    "wheat",
    "white",
    "whitesmoke",
    "yellow",
    "yellowgreen",
    "transparent",
    "currentcolor",
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
