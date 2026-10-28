package com.alecdorrington.annotatedtextformat.dom

import com.alecdorrington.annotatedtextformat.Span
import munit.FunSuite

class SpacingSuite extends FunSuite:

  test(
    "An emptied word takes the space after it, or else the one before it, but never a line break",
  ):
    assertEquals(
      Spacing.beside("I really like it.", Span(2, 8)),
      Some(Span(8, 9)),
    )
    assertEquals(
      Spacing.beside("I like.", Span(2, 6)),
      Some(Span(1, 2)),
    )
    assertEquals(Spacing.beside("a\nb", Span(2, 3)), None)
    assertEquals(
      Spacing.besides("I really like it.", Span(2, 8)),
      List(Span(8, 9), Span(1, 2)),
    )
    assertEquals(
      Spacing.besides("I like.", Span(2, 6)),
      List(Span(1, 2)),
    )

  test(
    "A word put in is spaced off the words beside it, but not in a script that does not space them, nor beside a full-width stop or a character beyond the basic plane",
  ):
    assertEquals(
      Spacing.around("early late", 5),
      (true, false),
    )
    assertEquals(
      Spacing.around("early.", 5),
      (true, false),
    )
    assertEquals(
      Spacing.around("(note)", 1),
      (false, true),
    )
    assertEquals(Spacing.around("我的猫", 1), (false, false))
    assertEquals(
      Spacing.around("我的猫。", 3),
      (false, false),
    )
    assertEquals(
      Spacing.around("名字𠀋好", 4),
      (false, false),
    )
    assertEquals(Spacing.around("猫，狗", 1), (false, false))
