package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.{Script, Span}

/** Where spaces go as annotations take text away or put it in. */
object Spacing:

  /**
    * The space just after `span` in `text`, or else just before it, if either
    * is one, to be taken away with it when it is emptied, so that no two spaces
    * are left together; never a line break, which may be kept as one.
    */
  def beside(text: String, span: Span): Option[Span] =
    List(span.end, span.start - 1)
      .find(at => text.indices.contains(at) && spaces.contains(text(at)))
      .map(at => Span(at, at + 1))

  /**
    * Whether a word put in at `at` in `text` needs a space before it, and one
    * after it, to stand apart from the words around it: none before after an
    * opening bracket or quote, none after before a closing one or a stop, and
    * none beside a script that does not space its words.
    */
  def around(text: String, at: Int): (Boolean, Boolean) = (
    Script
      .before(text, at)
      .exists(char => apart(char) && !among(opening, char)),
    Script.after(text, at).exists(char => apart(char) && !among(closing, char)),
  )

  /** Whether a word beside the code point `char` must be spaced off it. */
  private def apart(char: Int): Boolean = !Character.isWhitespace(char) &&
    Script.spaced(char)

  private def among(chars: String, char: Int): Boolean =
    chars.exists(_.toInt == char)

  /** The spaces that may be taken away: not line breaks. */
  private val spaces = " \t\u00a0"

  private val opening = "([{“‘«¿¡"
  private val closing = ".,;:!?)]}”’»…"
