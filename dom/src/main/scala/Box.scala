package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom

/**
  * A box in the window, as the DOM measures an element: its `left`, `top`,
  * `width` and `height`, in pixels.
  */
final case class Box
  (
    left: Double,
    top: Double,
    width: Double,
    height: Double,
  ):

  def right: Double = left + width

  def bottom: Double = top + height

  /** The middle of it, from side to side. */
  def centre: Double = left + width / 2

  /** Whether the point `(x, y)` lies within it, or on its edge. */
  def contains(x: Double, y: Double): Boolean = left <= x && x <= right &&
    top <= y && y <= bottom

  /** The smallest box holding both it and `other`. */
  def hull(other: Box): Box = Box.around(
    left.min(other.left),
    top.min(other.top),
    right.max(other.right),
    bottom.max(other.bottom),
  )

  /** How far the point `(x, y)` lies from it: `0` within it. */
  def distance(x: Double, y: Double): Double = math.hypot(
    (left - x).max(x - right).max(0),
    (top - y).max(y - bottom).max(0),
  )

  /** It, moved to `left` and `top`. */
  def at(left: Double, top: Double): Box = copy(left = left, top = top)

object Box:

  /** The box from `(left, top)` to `(right, bottom)`. */
  def around
    (
      left: Double,
      top: Double,
      right: Double,
      bottom: Double,
    )
    : Box = Box(left, top, right - left, bottom - top)

  /** The box that `rect` measures. */
  def of(rect: dom.DOMRect): Box = Box(
    rect.left,
    rect.top,
    rect.width,
    rect.height,
  )

  /** The boxes of the lines of `element`, as it wraps, in order. */
  def lines(element: dom.Element): List[Box] =
    val rects = element.getClientRects()
    List.tabulate(rects.length)(index => of(rects(index)))

  /**
    * The box of the window that `document` is shown in, but its scroll bars,
    * which a tooltip must not sit beneath.
    */
  def window(document: dom.Document): Box =
    val root   = document.documentElement
    val window = document.asInstanceOf[dom.HTMLDocument].defaultView
    Box(
      0,
      0,
      if root.clientWidth > 0 then root.clientWidth else window.innerWidth,
      if root.clientHeight > 0 then root.clientHeight else window.innerHeight,
    )

/** Where a tooltip stands of what it is for. */
enum Side:
  case Above, Below

/** Where a tooltip goes, as the line it is for is placed in the window. */
object Placement:

  /** How far a tooltip stands off the line it is for, in pixels. */
  val gap: Double = 6

  /** How far a tooltip keeps from the window's edges, in pixels. */
  val margin: Double = 8

  /**
    * Where `tip` goes for `line` within `viewport`: centred over the line, kept
    * within the viewport's sides; above it if there is room there, or else
    * below it if there is room there; or else on the side with more room, kept
    * within the viewport, over which a tip too tall for both scrolls.
    */
  def place(line: Box, tip: Box, viewport: Box): (Box, Side) =
    val side = sideOf(line, tip, viewport)
    val top  = side match
      case Side.Above => line.top - gap - tip.height
      case Side.Below => line.bottom + gap
    val left = line.centre - tip.width / 2
    (
      tip.at(
        clamp(
          left,
          tip.width,
          viewport.left,
          viewport.right,
        ),
        clamp(
          top,
          tip.height,
          viewport.top,
          viewport.bottom,
        ),
      ),
      side,
    )

  /**
    * The gap between `line` and `tip`, placed for it, which the pointer crosses
    * from one to the other: from the edge of one to the edge of the other, and
    * as wide as both together; or none if they touch or overlap, as a tip too
    * tall for the window may.
    */
  def between(line: Box, tip: Box): Option[Box] =
    val left  = line.left.min(tip.left)
    val right = line.right.max(tip.right)
    if tip.bottom < line.top then
      Some(Box.around(left, tip.bottom, right, line.top))
    else if line.bottom < tip.top then
      Some(Box.around(left, line.bottom, right, tip.top))
    else None

  private def sideOf(line: Box, tip: Box, viewport: Box): Side =
    val above = line.top - gap - (viewport.top + margin)
    val below = viewport.bottom - margin - (line.bottom + gap)
    if tip.height <= above then Side.Above
    else if tip.height <= below || below > above then Side.Below
    else Side.Above

  /**
    * `start`, of something `size` long, moved as little as keeps it within
    * `low` and `high` and the margin inside them, or at `low`'s margin if it is
    * too long for that.
    */
  private def clamp
    (
      start: Double,
      size: Double,
      low: Double,
      high: Double,
    )
    : Double = start.min(high - margin - size).max(low + margin)

  /** The line of `lines` that `(x, y)` lies within, or else the nearest. */
  def lineAt(lines: List[Box], x: Double, y: Double): Option[Box] = lines
    .minByOption(_.distance(x, y))
