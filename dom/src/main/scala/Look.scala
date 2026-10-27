package com.alecdorrington.annotatedtextformat.dom

/** How annotations whose values are of type `A` look in a page. */
trait Look[-A]:

  /** The colour in which an annotation whose value is `value` is highlighted. */
  def colour(value: A): Colour

  /**
    * What the span at `part` of an annotation whose value is `value` shows in
    * place of its text, if anything else, as a translation: for a point, the
    * text put in there. An empty text empties the span, and takes a space
    * beside it away with it, so that no two are left together.
    */
  def shown(value: A, part: Int): Option[String] = None

  /**
    * The layers drawn beneath the highlight of an element under annotations
    * whose values are `values`, as an application's own shading, asked only of
    * elements under an annotation that is shown.
    */
  def beneath(values: List[A]): List[Style.Layer] = Nil

object Look:

  /**
    * The look that `colour`, `shown` and `beneath` give, as [[Look]] describes
    * them.
    */
  def apply[A]
    (
      colour: A => Colour,
      shown: (A, Int) => Option[String] = (_: A, _: Int) => None,
      beneath: List[A] => List[Style.Layer] = (_: List[A]) => Nil,
    )
    : Look[A] =
    val (colours, shows, layers) = (colour, shown, beneath)
    new Look[A]:
      override def colour(value: A): Colour                   = colours(value)
      override def shown(value: A, part: Int): Option[String] =
        shows(value, part)
      override def beneath(values: List[A]): List[Style.Layer] = layers(values)
