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

  test(
    "A tooltip held on one side of its line stays there, where it may be as tall as the room between the line and the window's edge",
  ):
    val line = Box(400, 150, 100, 20)
    assertEquals(
      Placement.room(line, Side.Above, window),
      136.0,
    )
    assertEquals(
      Placement.room(line, Side.Below, window),
      616.0,
    )
    assertEquals(
      Placement.place(line, Box(0, 0, 200, 300), window)._2,
      Side.Below,
    )
    assertEquals(
      Placement.place(
        line,
        Box(0, 0, 200, 136),
        window,
        Some(Side.Above),
      ),
      (Box(350, 8, 200, 136), Side.Above),
    )
    assertEquals(
      Placement.place(
        Box(400, 600, 100, 20),
        tip,
        window,
        Some(Side.Below),
      ),
      (Box(350, 626, 200, 100), Side.Below),
    )

  test(
    "A tooltip opens on a side with room for what it reserves, though it is placed as tall as it is",
  ):
    val line = Box(400, 150, 100, 20)
    assertEquals(
      Placement.place(line, tip, window, reserve = 136),
      (Box(350, 44, 200, 100), Side.Above),
    )
    assertEquals(
      Placement.place(line, tip, window, reserve = 137),
      (Box(350, 176, 200, 100), Side.Below),
    )
    assertEquals(
      Placement
        .place(
          line,
          tip,
          window,
          Some(Side.Above),
          reserve = 300,
        )
        ._2,
      Side.Above,
    )

  test(
    "The gap between a line and its tooltip runs from the edge of one to the other, as wide as both, unless they meet",
  ):
    val line = Box(400, 400, 100, 20)
    assertEquals(
      Placement.between(line, Box(350, 294, 200, 100)),
      Some(Box(350, 394, 200, 6)),
    )
    assertEquals(
      Placement.between(line, Box(450, 426, 200, 100)),
      Some(Box(400, 420, 250, 6)),
    )
    assertEquals(
      Placement.between(line, Box(350, 8, 200, 900)),
      None,
    )

  test(
    "The hull of a line and its tooltip runs around the corners of each that stand out, holding the wedges beside the line but not the corners of the box around both",
  ):
    val line  = Box(400, 400, 100, 20)
    val above = Box(350, 294, 200, 100)
    val hull  = Placement.around(line, above)
    assertEquals(
      hull,
      List(
        (350.0, 294.0),
        (550.0, 294.0),
        (550.0, 394.0),
        (500.0, 420.0),
        (400.0, 420.0),
        (350.0, 394.0),
      ),
    )
    List(
      (450, 410),
      (450, 397),
      (450, 300),
      (515, 410),
      (385, 410),
      (550, 294),
    ).foreach((x, y) => assert(Placement.within(hull, x, y), (x, y)))
    List(
      (525, 410),
      (370, 410),
      (600, 300),
      (450, 430),
    ).foreach((x, y) => assert(!Placement.within(hull, x, y), (x, y)))
    assert(line.hull(above).contains(525, 410))
    assertEquals(
      Placement
        .around(
          Box(300, 20, 400, 20),
          Box(400, 46, 200, 100),
        )
        .toSet,
      Set(
        (300.0, 20.0),
        (700.0, 20.0),
        (700.0, 40.0),
        (600.0, 146.0),
        (400.0, 146.0),
        (300.0, 40.0),
      ),
    )

  test(
    "The way from a line to its tooltip is covered within the box around both, by the hull but each of them, unless they meet",
  ):
    val line = Box(400, 400, 100, 20)
    assertEquals(
      Placement.crossing(line, Box(350, 294, 200, 100)),
      Some(
        Box(350, 294, 200, 126) ->
          ("M 0 0 L 200 0 L 200 100 L 150 126 L 50 126 L 0 100 Z " +
            "M 0 0 L 200 0 L 200 100 L 0 100 Z " +
            "M 50 106 L 150 106 L 150 126 L 50 126 Z"),
      ),
    )
    assertEquals(
      Placement.crossing(line, Box(350, 8, 200, 900)),
      None,
    )
