package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom
import scala.annotation.tailrec

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

  /** Its corners, from its top left, clockwise. */
  def corners: List[(Double, Double)] = List(
    (left, top),
    (right, top),
    (right, bottom),
    (left, bottom),
  )

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
    * within the viewport's sides; on `side` of it, if given; or else above it
    * if there is room there for it, and for `reserve` if that is taller, as for
    * content to come, or else below it if there is room there; or else on the
    * side with more room, kept within the viewport, over which a tip too tall
    * for both scrolls.
    */
  def place
    (
      line: Box,
      tip: Box,
      viewport: Box,
      side: Option[Side] = None,
      reserve: Double = 0,
    )
    : (Box, Side) =
    val on  = side.getOrElse(sideOf(line, tip.height.max(reserve), viewport))
    val top = on match
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
      on,
    )

  /**
    * How tall a tip may be on `side` of `line` within `viewport`, standing off
    * the line and keeping from the viewport's edge.
    */
  def room(line: Box, side: Side, viewport: Box): Double = side match
    case Side.Above => line.top - gap - (viewport.top + margin)
    case Side.Below => viewport.bottom - margin - (line.bottom + gap)

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

  /**
    * The corners of the smallest convex shape holding both `line` and `tip`, in
    * order around it, over which the pointer crosses from one to the other; or
    * the one point that both are, if they are empty there.
    */
  def around(line: Box, tip: Box): List[(Double, Double)] =
    val corners = (line.corners ++ tip.corners).distinct.sorted
    @tailrec
    def straighten
      (
        kept: List[(Double, Double)],
        next: (Double, Double),
      )
      : List[(Double, Double)] = kept match
      case to :: from :: rest if turn(from, to, next) <= 0 =>
        straighten(from :: rest, next)
      case _ => kept
    def side(corners: List[(Double, Double)]): List[(Double, Double)] = corners
      .foldLeft(List.empty[(Double, Double)])((kept, next) =>
        next :: straighten(kept, next),
      )
      .reverse
    if corners.lengthCompare(2) < 0 then corners
    else side(corners).init ++ side(corners.reverse).init

  /**
    * Whether `(x, y)` lies within the convex shape whose corners are `corners`,
    * in order around it, or on its edge; or, if it has no area, as the hull of
    * two empty boxes, within the box holding its corners, if any.
    */
  def within
    (
      corners: List[(Double, Double)],
      x: Double,
      y: Double,
    )
    : Boolean =
    if corners.lengthCompare(3) < 0 then
      corners.nonEmpty &&
      Box
        .around(
          corners.map(_._1).min,
          corners.map(_._2).min,
          corners.map(_._1).max,
          corners.map(_._2).max,
        )
        .contains(x, y)
    else
      val turns = corners
        .zip(corners.drop(1) :+ corners.head)
        .map((from, to) => turn(from, to, (x, y)))
      turns.forall(_ >= 0) || turns.forall(_ <= 0)

  /**
    * What covers the way from `line` to `tip`, placed for it, so that nothing
    * beneath catches the pointer crossing from one to the other: the box
    * holding both, and within it, as an SVG path to fill by the even-odd rule,
    * the smallest convex shape holding both, but each of them, measured from
    * the box's corner; or none if they touch or overlap, as a tip too tall for
    * the window may. Neither is covered, so that each takes the pointer itself,
    * as a link that the line is in must, and the tip's own buttons.
    */
  def crossing(line: Box, tip: Box): Option[(Box, String)] = between(line, tip)
    .map(_ =>
      val bounds                                        = line.hull(tip)
      def path(corners: List[(Double, Double)]): String = corners
        .map((x, y) => s"${ x - bounds.left } ${ y - bounds.top }")
        .mkString("M ", " L ", " Z")
      bounds -> List(
        around(line, tip),
        tip.corners,
        line.corners,
      ).map(path).mkString(" "),
    )

  /**
    * Which way the way from `from` to `to` turns to reach `next`: one way if
    * positive, the other if negative, and straight on if neither.
    */
  private def turn
    (
      from: (Double, Double),
      to: (Double, Double),
      next: (Double, Double),
    )
    : Double = (to._1 - from._1) * (next._2 - from._2) - (to._2 - from._2) *
    (next._1 - from._1)

  private def sideOf(line: Box, tall: Double, viewport: Box): Side =
    val above = room(line, Side.Above, viewport)
    val below = room(line, Side.Below, viewport)
    if tall <= above then Side.Above
    else if tall <= below || below > above then Side.Below
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
