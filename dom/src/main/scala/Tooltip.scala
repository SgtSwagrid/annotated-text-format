package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom
import scala.scalajs.js

/**
  * A tooltip in a page's top layer, where no part of the page can clip or cover
  * it, holding any content in a shadow root, which the page's styles do not
  * reach. It is kept within the window, and content too large for it scrolls
  * within it. Its content may be replaced while it is open, as by an entry that
  * arrives later, and it is placed again as its size changes.
  *
  * Its `host` is the element in the page. It takes the attribute `data-side`,
  * `above` or `below`, as it stands of the line it is for.
  *
  * The pointer's presses, clicks and moves within it, if it is `interactive`,
  * go no further than it, so that the page does not act on them, and a press
  * leaves the focus where it was, but in a field. The page's own listeners on
  * its window or its document that capture events still hear of them first, as
  * nothing within the page can stop them.
  */
final class Tooltip private (
  val host: dom.HTMLElement,
  root: js.Dynamic,
  body: dom.Element,
):

  private var line: Option[Box] = None

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

  /** Shows `content` for `line`, the box of the line of text it is for. */
  def show(content: dom.Node, line: Box): Unit =
    fill(content)
    this.line = Some(line)
    open()
    fit()

  /** Shows `content` in place of what it shows, where it stands. */
  def update(content: dom.Node): Unit =
    fill(content)
    fit()

  /** Closes it, keeping it in the page to open again. */
  def hide(): Unit =
    line = None
    host.setAttribute("hidden", "")
    Tooltip.popover(host).foreach(_.hidePopover())

  /** Takes it out of the page. */
  def remove(): Unit =
    hide()
    Option(host.parentNode).foreach(_.removeChild(host))

  private def fill(content: dom.Node): Unit =
    Passage.children(body).foreach(body.removeChild)
    body.appendChild(content)

  /** Puts it in the top layer, if it is not there. */
  private def open(): Unit =
    if !host.isConnected then
      host.ownerDocument.documentElement.appendChild(host)
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
    * A tooltip for `document`, not yet in it, whose content is styled within
    * its shadow root by the CSS `styles`, as `.card { padding: 8px }`, and by
    * `sheets`, made for `document`, and which the pointer may move onto and
    * use, unless not `interactive`.
    */
  def apply
    (
      document: dom.Document,
      styles: String = "",
      sheets: Seq[dom.CSSStyleSheet] = Nil,
      interactive: Boolean = true,
    )
    : Tooltip =
    val host = document.createElement("div").asInstanceOf[dom.HTMLElement]
    val root = host
      .asInstanceOf[js.Dynamic]
      .attachShadow(js.Dynamic.literal(mode = "open"))
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
    resizing(host, tooltip)
    tooltip

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
      sheets.foreach(sheet =>
        val style = document.createElement("style")
        style.textContent = (0 until sheet.cssRules.length)
          .map(sheet.cssRules(_).cssText)
          .mkString("\n")
        root.appendChild(style),
      )

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
    * The pointer's events, which the page is not told of when they happen
    * within the tooltip.
    */
  private val kept = List(
    "pointerdown",
    "pointerup",
    "pointermove",
    "mousedown",
    "mouseup",
    "mousemove",
    "click",
    "dblclick",
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
  private def origin(event: dom.Event): dom.EventTarget =
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
  private val focusable =
    "a[href], area[href], button:not([disabled]), input:not([disabled]):not([type='hidden']), select:not([disabled]), textarea:not([disabled]), [contenteditable]:not([contenteditable='false']), [tabindex]:not([tabindex='-1'])"

  /**
    * Its own styles, which reset all that it inherits from the page, keep it
    * within the window, scrolling within itself, and hide it while it is
    * closed, as resetting everything would show it then.
    */
  private def own(interactive: Boolean): String =
    s""":host { all: initial; display: block; position: fixed; inset: auto; margin: 0; padding: 0; border: 0; background: none; box-sizing: border-box; max-width: calc(100vw - ${ 2 *
        Placement.margin }px); max-height: calc(100vh - ${ 2 *
        Placement.margin }px); overflow: auto;${
        if interactive then "" else " pointer-events: none;"
      } }
       |:host([hidden]) { display: none; }
       |""".stripMargin

  /**
    * Places `tooltip` again whenever its size changes, where the browser can
    * tell.
    */
  private def resizing(host: dom.HTMLElement, tooltip: Tooltip): Unit =
    val window = host
      .ownerDocument
      .asInstanceOf[dom.HTMLDocument]
      .defaultView
      .asInstanceOf[js.Dynamic]
    if !js.isUndefined(window.ResizeObserver) then
      js.Dynamic
        .newInstance(window.ResizeObserver)((_: js.Any, _: js.Any) =>
          tooltip.fit(),
        )
        .observe(host)

  /** The popover of `host`, where the browser has popovers. */
  private def popover(host: dom.HTMLElement): Option[Popover] = Option.when(
    !js.isUndefined(host.asInstanceOf[js.Dynamic].showPopover),
  )(host.asInstanceOf[Popover])

/** An element shown in the top layer, as a popover. */
@js.native
private trait Popover extends js.Object:

  def showPopover(): Unit = js.native

  def hidePopover(): Unit = js.native
