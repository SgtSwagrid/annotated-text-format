package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom
import scala.scalajs.js

/**
  * Documents of jsdom, from the `node_modules` that `package.json` lists, in
  * the directory the build gives as `NODE_PACKAGES`, or else the one the tests
  * run in, for the tests, which run in Node.js, which has no DOM of its own.
  */
object Jsdom:

  private lazy val jsdom: js.Dynamic = js
    .Dynamic
    .global
    .process
    .getBuiltinModule("node:module")
    .createRequire(s"$packages/")("jsdom")

  private def packages: String =
    val named = js.Dynamic.global.process.env.NODE_PACKAGES
    if js.isUndefined(named) then js.Dynamic.global.process.cwd().toString
    else named.toString

  /** The first element of a body holding the HTML `html`. */
  def element(html: String): dom.Element = js
    .Dynamic
    .newInstance(jsdom.JSDOM)(s"<!DOCTYPE html><body>$html</body>")
    .window
    .document
    .body
    .firstElementChild
    .asInstanceOf[dom.Element]

  /** Sends `element` the event `kind`, as the pointer would. */
  def send(element: dom.Element, kind: String): Unit =
    val window = element.ownerDocument.asInstanceOf[js.Dynamic].defaultView
    element.dispatchEvent(
      js.Dynamic.newInstance(window.MouseEvent)(kind).asInstanceOf[dom.Event],
    )

  /** Moves the pointer onto `node`, at `(x, y)` in the window. */
  def move(node: dom.Node, x: Double, y: Double): Unit =
    val window = node.ownerDocument.asInstanceOf[js.Dynamic].defaultView
    node.dispatchEvent(
      js.Dynamic
        .newInstance(window.MouseEvent)(
          "mousemove",
          js.Dynamic.literal(bubbles = true, clientX = x, clientY = y),
        )
        .asInstanceOf[dom.Event],
    )

  /** Presses the key `key` in `document`. */
  def press(document: dom.Document, key: String): Unit =
    val window = document.asInstanceOf[js.Dynamic].defaultView
    document.dispatchEvent(
      js.Dynamic
        .newInstance(window.KeyboardEvent)(
          "keydown",
          js.Dynamic.literal(bubbles = true, key = key),
        )
        .asInstanceOf[dom.Event],
    )
