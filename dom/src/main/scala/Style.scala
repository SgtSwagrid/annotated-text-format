package com.alecdorrington.annotatedtextformat.dom

/** How highlighted text is drawn, as declarations of CSS. */
object Style:

  /** The height of each underline, and so of each lane, in pixels. */
  val laneHeight: Int = 2

  /** How opaque the tint of highlighted text is, however many highlight it. */
  val tint: Double = 0.2

  /** The properties of CSS that [[of]] declares. */
  val properties: List[String] = List(
    "background-image",
    "background-size",
    "background-position",
    "background-repeat",
  )

  /**
    * The declarations that highlight text under `highlights`, each a colour and
    * its lane: an underline in each lane, as a background, not an `inset`
    * shadow, which would thicken over empty lanes; and a tint of each colour,
    * [[tint]] split between them, so that overlaps are no darker. None if there
    * are no highlights.
    */
  def of(highlights: List[(Colour, Int)]): List[(String, String)] =
    val alpha = share(highlights.size)
    Option
      .when(highlights.nonEmpty)(
        highlights.map((colour, lane) => underline(colour, lane)) ++
          highlights.map((colour, _) => tinted(colour, alpha)),
      )
      .fold(Nil)(declarations)

  private def declarations(layers: List[Layer]): List[(String, String)] = List(
    layers.map(_.image),
    layers.map(_.size),
    layers.map(_.position),
    layers.map(_ => "no-repeat"),
  ).map(_.mkString(", ")).zip(properties).map(_.swap)

  private def underline(colour: Colour, lane: Int): Layer = Layer(
    fill(colour.css),
    s"100% ${ laneHeight }px",
    s"0 calc(100% - ${ laneHeight * lane }px)",
  )

  private def tinted(colour: Colour, alpha: Double): Layer = Layer(
    fill(colour.faded(alpha)),
    "100% 100%",
    "0 0",
  )

  /** The opacity of each of `count` tints, which together make [[tint]]. */
  private def share(count: Int): Double = 1 - math.pow(1 - tint, 1.0 / count)

  private def fill(colour: String): String =
    s"linear-gradient($colour, $colour)"

  /** A layer of a background: its image, its size and its position. */
  private final case class Layer
    (
      image: String,
      size: String,
      position: String,
    )
