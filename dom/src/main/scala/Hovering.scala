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
  *
  * A touch or a pen, which has no pointer to hover, shows it by a tap, and a
  * tap elsewhere closes it. If `focusable`, each highlighted element takes the
  * focus in its turn, as by Tab, showing the tooltip for its first line: Tab
  * moves the focus on into the tooltip, if anything there takes it, and on from
  * its last part back to the element, and so on beyond it; Shift and Tab moves
  * from its first part back to the element; and Escape closes it, giving the
  * focus back to the element. The elements of hidden annotations take the focus
  * too, but show nothing.
  */
final class Hovering[A] private (
  decoration: Decoration[A],
  val tooltip: Tooltip,
  content: List[Int] => Option[dom.Node],
  focusable: Boolean,
):

  /** The element the tooltip is for, and the line of it, while it is open. */
  private var shown: Option[(dom.Element, Box)] = None

  /** Whether the tooltip was opened by the focus, rather than the pointer. */
  private var focused = false

  /** Whether the focus is being given back, which shows nothing again. */
  private var quiet = false

  private val document = tooltip.host.ownerDocument

  private val listeners = List[(String, js.Function1[dom.Event, Unit])](
    "mousemove"   -> (event => point(event.asInstanceOf[dom.MouseEvent])),
    "pointerdown" -> (event => tap(event.asInstanceOf[dom.PointerEvent])),
    "mouseout"    -> (event => if left(event) then hide()),
    "scroll"      -> (event => if !within(event.target) then hide()),
    "keydown"     -> (event => key(event.asInstanceOf[dom.KeyboardEvent])),
    "focusin"     -> (event => focus(event.target)),
    "focusout"    -> (event => blur(event.asInstanceOf[dom.FocusEvent])),
  )

  listeners.foreach((kind, listener) =>
    document.addEventListener(kind, listener, true),
  )

  /** The highlighted elements made to take the focus, if `focusable`. */
  private val stops: List[dom.Element] =
    if !focusable then Nil
    else
      decoration
        .annotated
        .annotations
        .indices
        .toList
        .flatMap(decoration.elements)
        .distinct
        .filterNot(_.hasAttribute("tabindex"))

  stops.foreach(_.setAttribute("tabindex", "0"))

  /**
    * Stops showing the tooltip, and closes it, the highlighted elements no
    * longer taking the focus.
    */
  def stop(): Unit =
    listeners.foreach((kind, listener) =>
      document.removeEventListener(kind, listener, true),
    )
    stops.foreach(_.removeAttribute("tabindex"))
    hide()

  /**
    * Shows what is under the pointer: the tooltip for a highlighted element
    * other than the one it is for, if `content` gives any for it; or nothing,
    * once the pointer is beyond the line the tooltip is for and the tooltip,
    * unless the focus opened it.
    */
  private def point(event: dom.MouseEvent): Unit =
    val target = event.target.asInstanceOf[dom.Node]
    decoration.elementAt(target) match
      case Some(element) if shown.exists(_._1 eq element) => ()
      case Some(element) if !within(target)               =>
        content(decoration.at(target)).fold(hideBeyond(event))(node =>
          show(
            element,
            node,
            lineAt(element, event),
            byFocus = false,
          ),
        )
      case _ => if !focused then hideBeyond(event)

  /** Shows, for a touch or a pen, what is tapped, as the pointer would. */
  private def tap(event: dom.PointerEvent): Unit =
    if event.pointerType == "touch" || event.pointerType == "pen" then
      focused = false
      point(event)

  /** Shows the tooltip for the highlighted element given the focus, if any. */
  private def focus(target: dom.EventTarget): Unit =
    val node = target.asInstanceOf[dom.Node]
    if !quiet && !within(target) then
      decoration
        .elementAt(node)
        .filter(_ eq node)
        .foreach(element =>
          content(decoration.at(element)).foreach(show(
            element,
            _,
            firstLine(element),
            byFocus = true,
          )),
        )

  /**
    * Closes the tooltip the focus opened, as the focus goes elsewhere than it
    * or the element it is for.
    */
  private def blur(event: dom.FocusEvent): Unit =
    val going = event.relatedTarget
    if focused && !within(going) && !shown.exists(_._1 eq going) then hide()

  private def key(event: dom.KeyboardEvent): Unit = event.key match
    case "Escape" => shown.foreach((element, _) =>
        if within(document.activeElement) then giveBack(element)
        hide(),
      )
    case "Tab" => shown.foreach((element, _) => tab(event, element))
    case _     => ()

  /**
    * Moves the focus by Tab from `element` into the tooltip, and from its first
    * or last part back to `element`, from which, going on, it moves to what
    * follows `element` in the page.
    */
  private def tab
    (
      event: dom.KeyboardEvent,
      element: dom.Element,
    )
    : Unit =
    val parts = tooltip.focusables
    if parts.nonEmpty then
      if !event.shiftKey && (document.activeElement eq element) then
        event.preventDefault()
        parts.head.focus()
      else if within(document.activeElement) then
        val at = tooltip.focused.fold(-1)(part => parts.indexWhere(_ eq part))
        if event.shiftKey && at == 0 then
          event.preventDefault()
          giveBack(element)
        else if !event.shiftKey && at == parts.size - 1 then
          giveBack(element)
          hide()

  /** Gives the focus back to `element`, showing nothing again for it. */
  private def giveBack(element: dom.Element): Unit =
    quiet = true
    try element.asInstanceOf[dom.HTMLElement].focus()
    finally quiet = false

  private def show
    (
      element: dom.Element,
      node: dom.Node,
      line: Box,
      byFocus: Boolean,
    )
    : Unit =
    shown = Some(element -> line)
    focused = byFocus
    tooltip.show(node, line)

  /** The line of `element` that `event` is on, or nearest to. */
  private def lineAt
    (
      element: dom.Element,
      event: dom.MouseEvent,
    )
    : Box = Placement
    .lineAt(
      Box.lines(element),
      event.clientX,
      event.clientY,
    )
    .getOrElse(Box.of(element.getBoundingClientRect()))

  private def firstLine(element: dom.Element): Box = Box
    .lines(element)
    .headOption
    .getOrElse(Box.of(element.getBoundingClientRect()))

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

  private def hide(): Unit =
    shown = None
    focused = false
    tooltip.hide()

object Hovering:

  /**
    * Shows `tooltip` with what `content` gives for the annotations, by their
    * indices, of `decoration` under the pointer, or tapped, or, if `focusable`,
    * given the focus, until it is stopped.
    */
  def apply[A]
    (
      decoration: Decoration[A],
      tooltip: Tooltip,
      content: List[Int] => Option[dom.Node],
      focusable: Boolean = false,
    )
    : Hovering[A] = new Hovering(decoration, tooltip, content, focusable)
