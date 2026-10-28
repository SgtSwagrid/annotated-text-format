package com.alecdorrington.annotatedtextformat.dom

import munit.FunSuite

class PlacementSuite extends FunSuite:

  private val window = Box(0, 0, 1000, 800)

  private val tip = Box(0, 0, 200, 100)

  test(
    "A tooltip stands centred above its line if there is room, or else on the side with more, within the window",
  ):
    assertEquals(
      Placement.place(Box(400, 400, 100, 20), tip, window),
      (Box(350, 294, 200, 100), Side.Above),
    )
    assertEquals(
      Placement.place(Box(400, 20, 100, 20), tip, window),
      (Box(350, 46, 200, 100), Side.Below),
    )
    assertEquals(
      Placement.place(Box(0, 400, 40, 20), tip, window)._1.left,
      8.0,
    )
    assertEquals(
      Placement.place(Box(980, 400, 20, 20), tip, window)._1.right,
      992.0,
    )
    val tall           = Box(0, 0, 200, 900)
    val (placed, side) = Placement.place(Box(400, 100, 100, 20), tall, window)
    assertEquals(side, Side.Below)
    assertEquals(placed.top, 8.0)

  test(
    "The line under the pointer is the one it is in, or else the nearest, and the box holding it and its tooltip spans the gap between them",
  ):
    val lines = List(Box(100, 0, 300, 20), Box(0, 20, 200, 20))
    assertEquals(
      Placement.lineAt(lines, 50, 30),
      Some(lines(1)),
    )
    assertEquals(
      Placement.lineAt(lines, 380, 25),
      Some(lines.head),
    )
    assertEquals(Placement.lineAt(Nil, 0, 0), None)
    val line = Box(400, 400, 100, 20)
    val hull = line.hull(Box(350, 294, 200, 100))
    assert(hull.contains(450, 397))
    assert(!hull.contains(450, 430))
