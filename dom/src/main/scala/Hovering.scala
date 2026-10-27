package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom
import scala.annotation.tailrec
import scala.collection.mutable
import scala.scalajs.js

/**
  * The annotations, by their indices in `decoration`, that a tooltip is shown
  * for, as those in the highlighted element under the pointer.
  */
final case class Hovered[A]
  (
    decoration: Decoration[A],
    annotations: List[Int],
  ):

  /** Their values, in order. */
  def values: List[A] =
    annotations.map(decoration.annotated.annotations(_).value)

/**
  * Shows `tooltip` with what `content` gives for the annotations under the
  * pointer, of whichever decoration it covers, as they are added and removed,
  * for the line of the element under it, within the page's open shadow roots
  * too. `content` is given the showing that it is for, by which it may replace
  * what it gave, as with an answer that comes later, which is dropped if the
  * tooltip has since closed or moved on. The tooltip stays open while the
  * pointer crosses from that line to it, and while it is over it, so that its
  * content may be scrolled or used; it closes as the pointer moves elsewhere,
  * leaves the window, or the page moves the line beneath it, and on Escape.
  * While it is open, the element it is for is described by it.
  *
  * A touch or a pen, which has no pointer to hover, shows it by a tap, whose
  * click goes no further, as to a link the element is in; and a tap on the
  * element again, or elsewhere, closes it. If `focusable`, each annotation
  * takes the focus in its turn, as by Tab, at the first of its elements that
  * holds any text, showing the tooltip for its first line, which follows the
  * line as the page moves: Tab moves the focus on into the tooltip, if anything
  * there takes it, and on from its last part back to the element, and so on
  * beyond it; Shift and Tab moves from its first part back to the element; and
  * Escape closes it, giving the focus back to the element. Hidden annotations
  * take the focus too, but show nothing.
  *
  * If `trusted`, it heeds only what the user does, not events that the page's
  * scripts make.
  */
final class Hovering[A] private (
  val tooltip: Tooltip,
  content: (Hovered[A], Tooltip.Showing) => Option[dom.Node],
  focusable: Boolean,
  trusted: Boolean,
):

  /** What the tooltip is for, while it is open. */
  private var shown: Option[Shown] = None

  /** Whether the tooltip was opened by the focus, rather than the pointer. */
  private var focused = false

  /** Whether the focus is being given back, which shows nothing again. */
  private var quiet = false

  /**
    * Whether the pointer is a touch or a pen, whose moves the browser also
    * gives as the mouse's, which show nothing.
    */
  private var touching = false

  /** The element that a tap just showed the tooltip for, until its click. */
  private var tapped: Option[dom.Element] = None

  private val document = tooltip.host.ownerDocument

  private val window = document
    .asInstanceOf[dom.HTMLDocument]
    .defaultView
    .asInstanceOf[dom.EventTarget]

  /**
    * Each decoration covered, by the outermost elements that it placed in the
    * page, so that the one an element is of is found from the element up.
    */
  private val decorations = mutable.HashMap.empty[dom.Node, Decoration[A]]

  /**
    * The decorations covered, each with the elements of it made to take the
    * focus.
    */
  private val covered = mutable
    .LinkedHashMap
    .empty[Decoration[A], List[dom.Element]]

  private val listeners = List(
    on("mousemove")(event => point(event.asInstanceOf[dom.MouseEvent])),
    on("pointermove")(event => sense(event.asInstanceOf[dom.PointerEvent])),
    on("pointerdown")(event => tap(event.asInstanceOf[dom.PointerEvent])),
    on("click")(click),
    on("mouseout")(event => if left(event) then hide()),
    on("scroll")(event => if !within(event.target) then relocate()),
    on("keydown")(event => key(event.asInstanceOf[dom.KeyboardEvent])),
    on("focusin")(event => focus(Tooltip.origin(event))),
    on("focusout")(event => blur(event.asInstanceOf[dom.FocusEvent])),
  )

  private val resized: js.Function1[dom.Event, Unit] =
    event => if heard(event) then relocate()

  listeners.foreach((kind, listener) =>
    document.addEventListener(kind, listener, true),
  )

  window.addEventListener("resize", resized)

  /** Covers `decoration` too, its annotations taking the focus if `focusable`. */
  def add(decoration: Decoration[A]): Unit =
    covered.keys.filterNot(present).toList.foreach(remove)
    if !covered.contains(decoration) then
      decoration.outermost.foreach(decorations.update(_, decoration))
      val stops =
        if !focusable then Nil
        else
          decoration
            .annotated
            .annotations
            .indices
            .toList
            .flatMap(decoration.elements(_).find(_.textContent.trim.nonEmpty))
            .distinct
            .filterNot(_.hasAttribute("tabindex"))
      stops.foreach(_.setAttribute("tabindex", "0"))
      covered.update(decoration, stops)

  /**
    * No longer covers `decoration`, whose annotations no longer take the focus,
    * closing the tooltip if it is for one of its elements.
    */
  def remove(decoration: Decoration[A]): Unit = covered
    .remove(decoration)
    .foreach(stops =>
      stops.foreach(_.removeAttribute("tabindex"))
      decoration.outermost.foreach(decorations.remove)
      if shown.exists(shown =>
          decoration.outermost.exists(_.contains(shown.element)),
        )
      then hide(),
    )

  /**
    * Whether any of `decoration` is still in the page, as it is not once it is
    * restored.
    */
  private def present(decoration: Decoration[A]): Boolean = decoration
    .outermost
    .exists(_.isConnected)

  /** Covers no decoration. */
  def clear(): Unit = covered.keys.toList.foreach(remove)

  /** Stops showing the tooltip, covering no decoration, and closes it. */
  def stop(): Unit =
    listeners.foreach((kind, listener) =>
      document.removeEventListener(kind, listener, true),
    )
    window.removeEventListener("resize", resized)
    clear()
    hide()

  /** A listener to `kind`, heeding only the user's events if `trusted`. */
  private def on
    (kind: String)
    (listener: dom.Event => Unit)
    : (String, js.Function1[dom.Event, Unit]) = kind ->
    (event => if heard(event) then listener(event))

  private def heard(event: dom.Event): Boolean = !trusted || event.isTrusted

  /**
    * The decoration covered that `node` is within, with its highlighted element
    * there, if any annotation is shown in it.
    */
  private def found(node: dom.Node): Option[(Decoration[A], dom.Element)] =
    Iterator
      .iterate(node)(_.parentNode)
      .takeWhile(_ != null)
      .flatMap(decorations.get)
      .nextOption()
      .flatMap(decoration => decoration.elementAt(node).map(decoration -> _))

  /**
    * Shows what is under the pointer: the tooltip for a highlighted element
    * other than the one it is for, if `content` gives any for it; or nothing,
    * once the pointer is beyond the line the tooltip is for and the tooltip,
    * unless the focus opened it.
    */
  private def point(event: dom.MouseEvent): Unit = if !touching then
    found(Tooltip.origin(event).asInstanceOf[dom.Node]) match
      case Some((_, element)) if isShown(element) => ()
      case Some((decoration, element))            =>
        if !open(
            decoration,
            element,
            lineAt(element, event),
            byFocus = false,
          )
        then hideBeyond(event)
      case _ => if !focused then hideBeyond(event)

  /** Notes whether the pointer moving is a touch or a pen. */
  private def sense(event: dom.PointerEvent): Unit =
    touching = event.pointerType != "mouse"

  /**
    * Shows, for a touch or a pen, what is tapped, or closes the tooltip on a
    * tap on what it is for again, or elsewhere.
    */
  private def tap(event: dom.PointerEvent): Unit =
    tapped = None
    sense(event)
    if touching then
      focused = false
      found(Tooltip.origin(event).asInstanceOf[dom.Node]) match
        case Some((_, element)) if isShown(element) => hide()
        case Some((decoration, element))            =>
          if open(
              decoration,
              element,
              lineAt(element, event),
              byFocus = false,
            )
          then tapped = Some(element)
          else hide()
        case None => if !within(event.target) then hide()

  /**
    * Keeps the click of a tap that showed the tooltip from the page, as from a
    * link that the element is in.
    */
  private def click(event: dom.Event): Unit =
    tapped.foreach(element =>
      if element.contains(Tooltip.origin(event).asInstanceOf[dom.Node]) then
        event.preventDefault()
        event.stopPropagation(),
    )
    tapped = None

  /** Shows the tooltip for the highlighted element given the focus, if any. */
  private def focus(target: dom.EventTarget): Unit =
    val node = target.asInstanceOf[dom.Node]
    if !quiet then
      found(node)
        .filter(_._2 eq node)
        .foreach((decoration, element) =>
          if isShown(element) then focused = true
          else
            open(
              decoration,
              element,
              firstLine(element),
              byFocus = true,
            ),
        )

  /**
    * Shows the tooltip for `element` of `decoration`, at `line`, if `content`
    * gives anything for it; and gives whether it did.
    */
  private def open
    (
      decoration: Decoration[A],
      element: dom.Element,
      line: Box,
      byFocus: Boolean,
    )
    : Boolean =
    val showing = tooltip.next()
    content(
      Hovered(decoration, decoration.at(element)),
      showing,
    ) match
      case Some(node) =>
        undescribe()
        shown = Some(Shown(
          element,
          line,
          Box.lines(element).indexOf(line),
        ))
        focused = byFocus
        tooltip.start(showing, node, line, Some(element))
        describe(element)
        true
      case None => false

  /**
    * Follows the line the tooltip is for, as the page scrolls or the window
    * changes size: placing the tooltip again for it, if the focus opened it, or
    * else closing it, if the line moved from beneath the pointer.
    */
  private def relocate(): Unit = shown.foreach(shown =>
    val now =
      if !shown.element.isConnected then None
      else
        Some(
          Box
            .lines(shown.element)
            .lift(shown.index)
            .getOrElse(Box.of(shown.element.getBoundingClientRect())),
        )
    now match
      case Some(line) if line == shown.line => ()
      case Some(line) if focused            =>
        this.shown = Some(shown.copy(line = line))
        tooltip.follow(line)
      case _ => hide(),
  )

  /**
    * Closes the tooltip the focus opened, as the focus goes elsewhere than it
    * or the element it is for.
    */
  private def blur(event: dom.FocusEvent): Unit =
    val going = event.relatedTarget
    if focused && !within(going) &&
      !shown.exists(shown => holds(going, shown.element))
    then hide()

  private def key(event: dom.KeyboardEvent): Unit = event.key match
    case "Escape" => shown.foreach(shown =>
        event.stopPropagation()
        if active.exists(within) && shown.element.isConnected then
          giveBack(shown.element)
        hide(),
      )
    case "Tab" => shown.foreach(shown => tab(event, shown.element))
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
      if !event.shiftKey && active.exists(_ eq element) then
        event.preventDefault()
        parts.head.focus()
      else if active.exists(within) then
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

  /**
    * The element that has the focus, within the page's open shadow roots too,
    * but not within the tooltip's.
    */
  private def active: Option[dom.Element] =
    @tailrec
    def inner(element: dom.Element): dom.Element =
      val root = element.asInstanceOf[js.Dynamic].shadowRoot
      if (element eq tooltip.host) || js.isUndefined(root) || (root: Any) ==
          null || (root.activeElement: Any) == null
      then element
      else inner(root.activeElement.asInstanceOf[dom.Element])
    Option(document.asInstanceOf[dom.HTMLDocument].activeElement).map(inner)

  /** Whether `target` is `element`, or holds it, within shadow roots too. */
  private def holds
    (
      target: dom.EventTarget,
      element: dom.Node,
    )
    : Boolean = Iterator
    .iterate(element)(node =>
      Option(node.parentNode).getOrElse(
        if node.nodeType == Passage.fragmentNode then
          node.asInstanceOf[js.Dynamic].host.asInstanceOf[dom.Node]
        else null,
      ),
    )
    .takeWhile(node => node != null && !js.isUndefined(node))
    .exists(_ eq target)

  private def isShown(element: dom.Element): Boolean =
    shown.exists(_.element eq element)

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
    .flatMap(shown => tooltip.box.map(shown.line.hull))
    .exists(_.contains(x, y))

  /** Whether `target` is the tooltip, or within it. */
  private def within(target: dom.EventTarget): Boolean = Option(target).exists(
    target => tooltip.host.contains(target.asInstanceOf[dom.Node]),
  )

  /** Whether `event` is the pointer leaving the window. */
  private def left(event: dom.Event): Boolean = event
    .asInstanceOf[dom.MouseEvent]
    .relatedTarget == null

  /** Stops describing the element the tooltip was for by it. */
  private def undescribe(): Unit = shown.foreach(shown =>
    val rest = described(shown.element).filterNot(_ == tooltip.id)
    if rest.isEmpty then shown.element.removeAttribute("aria-describedby")
    else shown.element.setAttribute("aria-describedby", rest.mkString(" ")),
  )

  /** Describes `element` by the tooltip too, as well as by what it was. */
  private def describe(element: dom.Element): Unit =
    val ids = described(element)
    if !ids.contains(tooltip.id) then
      element.setAttribute(
        "aria-describedby",
        (ids :+ tooltip.id).mkString(" "),
      )

  /** The `id`s of what describes `element`. */
  private def described(element: dom.Element): List[String] = Option(
    element.getAttribute("aria-describedby"),
  ).toList.flatMap(_.split(" ").toList).filter(_.nonEmpty)

  private def hide(): Unit =
    undescribe()
    shown = None
    focused = false
    tooltip.hide()

/**
  * The highlighted `element` that a tooltip is for, and the `line` of it, the
  * `index`th of its lines, if it is one.
  */
private final case class Shown
  (
    element: dom.Element,
    line: Box,
    index: Int,
  )

object Hovering:

  /**
    * Shows `tooltip` with what `content` gives for the annotations under the
    * pointer, or tapped, or, if `focusable`, given the focus, of the
    * decorations added to it, until it is stopped, heeding only the user's
    * events if `trusted`.
    */
  def apply[A]
    (
      tooltip: Tooltip,
      content: (Hovered[A], Tooltip.Showing) => Option[dom.Node],
      focusable: Boolean = false,
      trusted: Boolean = false,
    )
    : Hovering[A] = new Hovering(tooltip, content, focusable, trusted)
