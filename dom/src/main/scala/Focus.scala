package com.alecdorrington.annotatedtextformat.dom

/**
  * Which annotations of each decoration that a [[Hovering]] covers take the
  * focus, as by Tab, showing their tooltip, and how. An annotation within what
  * takes the focus itself, as a link, never does, as the page's own keys reach
  * it there.
  */
enum Focus[-A]:

  /** No annotation takes the focus. */
  case Off extends Focus[Any]

  /** Each annotation whose value is one `which` holds for, in its turn. */
  case Each[A](which: A => Boolean) extends Focus[A]

  /**
    * Of the annotations of each decoration whose values are those `which` holds
    * for, one at a time, as one stop of Tab: the first, and then the one last
    * focused; the left and right arrow keys move the focus among the rest, so
    * that a passage of many annotations is passed over by one press of Tab.
    */
  case Roving[A](which: A => Boolean) extends Focus[A]

  /** Whether an annotation whose value is `value` takes the focus. */
  def takes(value: A): Boolean = this match
    case Off           => false
    case Each(which)   => which(value)
    case Roving(which) => which(value)

  /** Whether the arrow keys move the focus among a decoration's annotations. */
  def roves: Boolean = this match
    case Roving(_) => true
    case _         => false
