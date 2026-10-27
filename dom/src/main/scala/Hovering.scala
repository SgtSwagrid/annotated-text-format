package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom
import scala.scalajs.js

/**
  * Shows `tooltip` with what `content` gives for the annotations of
  * `decoration` under the pointer, for the line of the element under it. The
  * tooltip stays open while the pointer crosses from that line to it, and while
  * it is over it, so that its content may be scrolled or used; it closes as the
  * pointer moves elsewhere, leaves the window, or the page scrolls beneath it,
  * and on Escape.
  */
final class Hovering[A] private (
  decoration: Decoration[A],
  val tooltip: Tooltip,
  content: List[Int] => Option[dom.Node],
):

  /** The element the tooltip is for, and the line of it, while it is open. */
  private var shown: Option[(dom.Element, Box)] = None

  private val document = tooltip.host.ownerDocument

  private val listeners = List[(String, js.Function1[dom.Event, Unit])](
    "mousemove" -> (event => point(event.asInstanceOf[dom.MouseEvent])),
    "mouseout"  -> (event => if left(event) then hide()),
    "scroll"    -> (event => if !within(event.target) then hide()),
    "keydown"   -> (event => if escaped(event) then hide()),
  )

  listeners.foreach((kind, listener) =>
    document.addEventListener(kind, listener, true),
  )

  /** Stops showing the tooltip, and closes it. */
  def stop(): Unit =
    listeners.foreach((kind, listener) =>
      document.removeEventListener(kind, listener, true),
    )
    hide()

  /**
    * Shows what is under the pointer: the tooltip for a highlighted element
    * other than the one it is for, if `content` gives any for it; or nothing,
    * once the pointer is beyond the line the tooltip is for and the tooltip.
    */
  private def point(event: dom.MouseEvent): Unit =
    val target = event.target.asInstanceOf[dom.Node]
    decoration.elementAt(target) match
      case Some(element) if shown.exists(_._1 eq element) => ()
      case Some(element) if !within(target)               =>
        content(
          decoration.at(target),
        ).fold(hideBeyond(event))(show(element, _, event))
      case _ => hideBeyond(event)

  private def show
    (
      element: dom.Element,
      node: dom.Node,
      event: dom.MouseEvent,
    )
    : Unit =
    val line = Placement
      .lineAt(
        Box.lines(element),
        event.clientX,
        event.clientY,
      )
      .getOrElse(Box.of(element.getBoundingClientRect()))
    shown = Some(element -> line)
    tooltip.show(node, line)

  /** Closes the tooltip unless the pointer is on the way to it or over it. */
  private def hideBeyond(event: dom.MouseEvent): Unit =
    if !bridged(event.clientX, event.clientY) && !within(event.target) then
      hide()

  /**
    * Whether `(x, y)` lies within the box holding both the line the tooltip is
    * for and the tooltip, across which the pointer crosses to it.
    */
  private def bridged(x: Double, y: Double): Boolean = shown
    .flatMap((_, line) => tooltip.box.map(line.hull))
    .exists(_.contains(x, y))

  /** Whether `target` is the tooltip, or within it. */
  private def within(target: dom.EventTarget): Boolean = Option(target).exists(
    target => tooltip.host.contains(target.asInstanceOf[dom.Node]),
  )

  /** Whether `event` is the pointer leaving the window. */
  private def left(event: dom.Event): Boolean = event
    .asInstanceOf[dom.MouseEvent]
    .relatedTarget == null

  private def escaped(event: dom.Event): Boolean = event
    .asInstanceOf[dom.KeyboardEvent]
    .key == "Escape"

  private def hide(): Unit =
    shown = None
    tooltip.hide()

object Hovering:

  /**
    * Shows `tooltip` with what `content` gives for the annotations, by their
    * indices, of `decoration` under the pointer, until it is stopped.
    */
  def apply[A]
    (
      decoration: Decoration[A],
      tooltip: Tooltip,
      content: List[Int] => Option[dom.Node],
    )
    : Hovering[A] = new Hovering(decoration, tooltip, content)
