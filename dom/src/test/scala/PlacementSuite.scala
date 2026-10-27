package com.alecdorrington.annotatedtextformat.dom

import munit.FunSuite

class PlacementSuite extends FunSuite:

  private val window = Box(0, 0, 1000, 800)

  private val tip = Box(0, 0, 200, 100)

  test("A tooltip stands centred above its line, if there is room"):
    assertEquals(
      Placement.place(Box(400, 400, 100, 20), tip, window),
      (Box(350, 294, 200, 100), Side.Above),
    )

  test("A tooltip stands below a line at the top of the window"):
    assertEquals(
      Placement.place(Box(400, 20, 100, 20), tip, window),
      (Box(350, 46, 200, 100), Side.Below),
    )

  test("A tooltip is kept within the window's sides"):
    assertEquals(
      Placement.place(Box(0, 400, 40, 20), tip, window)._1.left,
      8.0,
    )
    assertEquals(
      Placement.place(Box(980, 400, 20, 20), tip, window)._1.right,
      992.0,
    )

  test(
    "A tooltip too tall for either side goes on the side with more room, within the window",
  ):
    val tall           = Box(0, 0, 200, 900)
    val (placed, side) = Placement.place(Box(400, 100, 100, 20), tall, window)
    assertEquals(side, Side.Below)
    assertEquals(placed.top, 8.0)

  test("The line under the pointer is the one it is in, or else the nearest"):
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

  test("The box holding a line and its tooltip spans the gap between them"):
    val line = Box(400, 400, 100, 20)
    val hull = line.hull(Box(350, 294, 200, 100))
    assert(hull.contains(450, 397))
    assert(!hull.contains(450, 430))
