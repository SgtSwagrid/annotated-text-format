package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom
import scala.scalajs.js

/**
  * A tooltip in a page's top layer, where no part of the page can clip or cover
  * it, holding any content in a shadow root, which the page's styles do not
  * reach. It is kept within the window, and content too large for it scrolls
  * within it. Each time it is shown is a [[Tooltip.Showing]] of its own, by
  * which its content may be replaced while it lasts, as by an entry that
  * arrives later, and it is placed again as its size changes.
  *
  * Its `host` is the element in the page, an `atf-tooltip`, which no rule of
  * the page for its own elements matches, with an `id` of its own, by which
  * what it is for is described, and the role of a dialog if it is
  * `interactive`, or else of a tooltip. It takes the attribute `data-side`,
  * `above` or `below`, as it stands of the line it is for. It goes at the root
  * of the page, or in the modal dialog open, outside which all is inert.
  *
  * Its content inherits nothing from the page, so its styles must set its font,
  * its colours and its background.
  *
  * The pointer's presses, clicks and moves within it, and keys typed in it, if
  * it is `interactive`, go no further than it, so that the page does not act on
  * them, and a press leaves the focus where it was, but in a field, so that its
  * text cannot be selected by the pointer. The page's own listeners on its
  * window or its document that capture events still hear of them first, as
  * nothing within the page can stop them.
  */
final class Tooltip private (
  val host: dom.HTMLElement,
  root: js.Dynamic,
  body: dom.Element,
):

  private var line: Option[Box] = None

  /** What it shows, while it is open. */
  private var current: Option[Tooltip.Showing] = None

  /** What places it again as its size changes, where the browser can tell. */
  private var observer: Option[js.Dynamic] = None

  /** The `id` of its host, by which what it is for is described. */
  def id: String = host.id

  /** Whether it is open. */
  def isOpen: Boolean = line.isDefined

  /** The box that it covers, while it is open. */
  def box: Option[Box] = line.map(_ => Box.of(host.getBoundingClientRect()))

  /** What within it takes the focus, in order: its links, buttons and fields. */
  def focusables: List[dom.HTMLElement] =
    val found = body.querySelectorAll(Tooltip.focusable)
    (0 until found.length).toList.map(found(_).asInstanceOf[dom.HTMLElement])

  /** What within it has the focus, if anything. */
  def focused: Option[dom.Element] =
    Option(root.activeElement.asInstanceOf[dom.Element])

  /**
    * Shows `content` for `line`, the box of the line of text it is for, as a
    * showing of its own, by which it may be replaced until the tooltip closes
    * or shows something else.
    */
  def show(content: dom.Node, line: Box): Tooltip.Showing =
    val showing = next()
    start(showing, content, line, None)
    showing

  /** A showing to come, which shows nothing until it is started. */
  private[dom] def next(): Tooltip.Showing = new Tooltip.Showing(this)

  /**
    * Shows `content` for `line` as `showing`, as for `near`, if anything, in
    * the modal dialog that holds it, if any.
    */
  private[dom] def start
    (
      showing: Tooltip.Showing,
      content: dom.Node,
      line: Box,
      near: Option[dom.Node],
    )
    : Unit =
    fill(content)
    this.line = Some(line)
    current = Some(showing)
    open(near)
    fit()

  /** Places it for `line`, as after the line has moved, if it is open. */
  private[dom] def follow(line: Box): Unit = if isOpen then
    this.line = Some(line)
    fit()

  /** Closes it, keeping it in the page to open again. */
  def hide(): Unit =
    line = None
    current = None
    host.setAttribute("hidden", "")
    Tooltip.popover(host).foreach(_.hidePopover())

  /** Takes it out of the page. */
  def remove(): Unit =
    hide()
    observer.foreach(_.disconnect())
    observer = None
    Option(host.parentNode).foreach(_.removeChild(host))

  private def fill(content: dom.Node): Unit =
    Passage.children(body).foreach(body.removeChild)
    body.appendChild(content)

  /** Puts it in the top layer, if it is not there. */
  private def open(near: Option[dom.Node]): Unit =
    val container = Tooltip.container(host.ownerDocument, near)
    if !(host.parentNode eq container) then container.appendChild(host)
    host.removeAttribute("hidden")
    Tooltip
      .popover(host)
      .filterNot(_ => host.matches(":popover-open"))
      .foreach(_.showPopover())

  /** Places it for its line, as it is now measured. */
  private def fit(): Unit = line.foreach(line =>
    val (placed, side) = Placement.place(
      line,
      Box.of(host.getBoundingClientRect()),
      Box.window(host.ownerDocument),
    )
    host
      .style
      .setProperty(
        "left",
        s"${ placed.left }px",
        "important",
      )
    host.style.setProperty("top", s"${ placed.top }px", "important")
    host.setAttribute("data-side", side.toString.toLowerCase),
  )

object Tooltip:

  /**
    * One showing of `tooltip`: what it shows from when it opens for a line,
    * until it closes or shows something else.
    */
  final class Showing private[dom] (tooltip: Tooltip):

    /** Whether the tooltip still shows it. */
    def isCurrent: Boolean = tooltip.current.exists(_ eq this)

    /**
      * Shows `content` in its place, where it stands, if the tooltip still
      * shows it, or else nothing, as for an answer come too late; and gives
      * whether it did.
      */
    def update(content: dom.Node): Boolean = isCurrent && {
      tooltip.fill(content)
      tooltip.fit()
      true
    }

  /**
    * A tooltip for `document`, not yet in it, whose content is styled within
    * its shadow root by the CSS `styles`, as `.card { padding: 8px }`, and by
    * `sheets`, made for `document`, and which the pointer may move onto and
    * use, unless not `interactive`, when it is named `label`, as for a reader
    * of the screen. Its shadow root is `closed` to the page's scripts if so.
    *
    * The page's own rules for every element, as `* { padding: 0 }`, outweigh
    * rules for the host, `:host`, unless those are marked `!important`, as its
    * own are; so mark those of `styles` too, but never `top`, `left` or `all`,
    * which would then outweigh where it is placed.
    */
  def apply
    (
      document: dom.Document,
      styles: String = "",
      sheets: Seq[dom.CSSStyleSheet] = Nil,
      interactive: Boolean = true,
      label: String = "",
      closed: Boolean = false,
    )
    : Tooltip =
    val host = document.createElement(tag).asInstanceOf[dom.HTMLElement]
    val root = host
      .asInstanceOf[js.Dynamic]
      .attachShadow(
        js.Dynamic.literal(mode = if closed then "closed" else "open"),
      )
    made += 1
    host.id = s"$tag-$bundle-$made"
    host.setAttribute(
      "role",
      if interactive then "dialog" else "tooltip",
    )
    if label.nonEmpty then host.setAttribute("aria-label", label)
    val style = document.createElement("style")
    val body  = document.createElement("div")
    style.textContent = own(interactive) + styles
    root.appendChild(style)
    adopt(root, document, sheets)
    root.appendChild(body)
    host.setAttribute("popover", "manual")
    host.setAttribute(Passage.decorated, "")
    host.setAttribute("hidden", "")
    if interactive then keep(host)
    val tooltip = new Tooltip(host, root, body)
    tooltip.observer = resizing(host, tooltip)
    tooltip

  /** The name of the host's element. */
  private val tag = "atf-tooltip"

  /** How many tooltips have been made, for their `id`s. */
  private var made = 0

  /**
    * A part of each `id` drawn at random, so that tooltips made by two scripts
    * in one page, each with this library, are told apart.
    */
  private val bundle = java
    .lang
    .Long
    .toString((math.random() * 1e12).toLong, 36)

  /**
    * Where a tooltip of `document` goes: in the modal dialog open that holds
    * `near`, or else the last open, outside which all is inert; or else at its
    * root.
    */
  private def container
    (
      document: dom.Document,
      near: Option[dom.Node],
    )
    : dom.Node =
    def modal(element: dom.Element): Option[dom.Element] =
      try Option(element.closest("dialog:modal"))
      catch case _: js.JavaScriptException => None
    val holding = near
      .map(node =>
        if node.nodeType == Passage.elementNode then node else node.parentNode,
      )
      .filter(_ != null)
      .flatMap(node => modal(node.asInstanceOf[dom.Element]))
    val modals =
      try document.querySelectorAll("dialog:modal")
      catch case _: js.JavaScriptException => null
    holding.getOrElse(
      if modals == null || modals.length == 0 then document.documentElement
      else modals(modals.length - 1),
    )

  /**
    * Styles what is within `root` by `sheets`, adopting them where the browser
    * can, and else copying their rules, as where a page's script may not hand
    * them to its document, as an extension's may not in some browsers.
    */
  private def adopt
    (
      root: js.Dynamic,
      document: dom.Document,
      sheets: Seq[dom.CSSStyleSheet],
    )
    : Unit = if sheets.nonEmpty then
    if js.isUndefined(root.adoptedStyleSheets) || !adopting(root, sheets) then
      sheets
        .flatMap(rules)
        .foreach(rules =>
          val style = document.createElement("style")
          style.textContent = rules
          root.appendChild(style),
        )

  /**
    * The rules of `sheet`, if they may be read, as those of a sheet of another
    * origin may not.
    */
  private def rules(sheet: dom.CSSStyleSheet): Option[String] =
    try
      Some(
        (0 until sheet.cssRules.length)
          .map(sheet.cssRules(_).cssText)
          .mkString("\n"),
      )
    catch case _: js.JavaScriptException => None

  /** Whether `root` adopts `sheets`, as the browser may refuse. */
  private def adopting
    (
      root: js.Dynamic,
      sheets: Seq[dom.CSSStyleSheet],
    )
    : Boolean =
    try
      root.adoptedStyleSheets = js.Array(sheets*)
      true
    catch case _: js.JavaScriptException => false

  /**
    * The pointer's events and the keys', which the page is not told of when
    * they happen within the tooltip.
    */
  private val kept = List(
    "keydown",
    "keyup",
    "keypress",
    "beforeinput",
    "input",
    "pointerdown",
    "pointerup",
    "pointermove",
    "mousedown",
    "mouseup",
    "mousemove",
    "click",
    "dblclick",
    "contextmenu",
    "touchstart",
    "touchend",
  )

  /**
    * Keeps the pointer's events within `host` from the page, and keeps a press
    * there from taking the focus from the page, unless on a field, in which one
    * may type.
    */
  private def keep(host: dom.HTMLElement): Unit = kept.foreach(kind =>
    host.addEventListener(
      kind,
      (event: dom.Event) =>
        event.stopPropagation()
        if kind == "mousedown" && !typed(origin(event), host) then
          event.preventDefault(),
    ),
  )

  /** Where `event` happened, within a shadow root as well. */
  private[dom] def origin(event: dom.Event): dom.EventTarget =
    val path = event.asInstanceOf[js.Dynamic].composedPath
    if js.typeOf(path) != "function" then event.target
    else
      event
        .asInstanceOf[js.Dynamic]
        .composedPath()
        .asInstanceOf[js.Array[dom.EventTarget]]
        .headOption
        .getOrElse(event.target)

  /**
    * Whether `target` is a field, in which one may type, or `host` itself, as
    * its scroll bar, which a press must not be kept from.
    */
  private def typed
    (
      target: dom.EventTarget,
      host: dom.HTMLElement,
    )
    : Boolean = (target eq host) ||
    js.typeOf(target.asInstanceOf[js.Dynamic].matches) ==
      "function" && target.asInstanceOf[dom.Element].matches(field)

  /** What takes typing. */
  private val field =
    "input, textarea, select, [contenteditable]:not([contenteditable='false'])"

  /** What takes the focus. */
  private[dom] val focusable =
    "a[href], area[href], button:not([disabled]), input:not([disabled]):not([type='hidden']), select:not([disabled]), textarea:not([disabled]), [contenteditable]:not([contenteditable='false']), [tabindex]:not([tabindex='-1'])"

  /**
    * Its own styles, which reset all that it inherits from the page, keep it
    * within the window, scrolling within itself, and hide it while it is
    * closed, as resetting everything would show it then. Each but `all` and
    * `inset` is `!important`, so that the page's rules for every element do not
    * outweigh it; those two are not, as they would then outweigh where it is
    * placed, which is set on the host itself.
    */
  private def own(interactive: Boolean): String =
    s""":host { all: initial; inset: auto; display: block !important; position: fixed !important; margin: 0 !important; padding: 0 !important; border: 0 !important; background: none !important; box-sizing: border-box !important; max-width: calc(100vw - ${ 2 *
        Placement.margin }px) !important; max-height: calc(100vh - ${ 2 *
        Placement.margin }px) !important; max-height: calc(100dvh - ${ 2 *
        Placement.margin }px) !important; overflow: auto !important;${
        if interactive then "" else " pointer-events: none !important;"
      } }
       |:host([hidden]) { display: none !important; }
       |""".stripMargin

  /**
    * Places `tooltip` again whenever its size changes, where the browser can
    * tell.
    */
  private def resizing
    (host: dom.HTMLElement, tooltip: Tooltip)
    : Option[js.Dynamic] =
    val window = host
      .ownerDocument
      .asInstanceOf[dom.HTMLDocument]
      .defaultView
      .asInstanceOf[js.Dynamic]
    Option.when(!js.isUndefined(window.ResizeObserver)):
      val observer = js
        .Dynamic
        .newInstance(window.ResizeObserver)((_: js.Any, _: js.Any) =>
          tooltip.fit(),
        )
      observer.observe(host)
      observer

  /** The popover of `host`, where the browser has popovers. */
  private def popover(host: dom.HTMLElement): Option[Popover] = Option.when(
    !js.isUndefined(host.asInstanceOf[js.Dynamic].showPopover),
  )(host.asInstanceOf[Popover])

/** An element shown in the top layer, as a popover. */
@js.native
private trait Popover extends js.Object:

  def showPopover(): Unit = js.native

  def hidePopover(): Unit = js.native
