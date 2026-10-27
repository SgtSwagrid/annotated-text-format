package com.alecdorrington.annotatedtextformat.dom

import org.scalajs.dom
import scala.scalajs.js

/**
  * Documents of jsdom, from the `node_modules` that `package.json` lists, for
  * the tests, which run in Node.js, which has no DOM of its own.
  */
object Jsdom:

  private lazy val jsdom: js.Dynamic = js
    .Dynamic
    .global
    .process
    .getBuiltinModule("node:module")
    .createRequire(s"${ js.Dynamic.global.process.cwd() }/")("jsdom")

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
