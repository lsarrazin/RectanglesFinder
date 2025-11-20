import rectangles._

class RectangleFinderTest extends org.scalatest.FunSuite {

  val losange1 = List(Point(0, 0), Point(2, 2), Point(2, 0), Point(4, 2))
  val trapeze1 = List(Point(8, 0), Point(3, 2), Point(5, 2), Point(0, 0))
  val rectangle1 = List(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1))
  val rectangle2 = List(Point(2, 2), Point(0, 0), Point(0, 2), Point(2, 0))
  val rectangle3 = List(Point(2, 4), Point(0, 0), Point(0, 4), Point(2, 0))
  val blob1 = List(Point(0, 0), Point(8, 0), Point(0, 8), Point(4, 4))

  val sets: List[(String, Boolean, List[Point])] = List(
    ("Losange1", false, losange1),
    ("Trapeze1", false, trapeze1),
    ("Rectangle1", true, rectangle1),
    ("Rectangle2", true, rectangle2),
    ("Rectangle3", true, rectangle3),
    ("Blob1", false, blob1)
  )

  val allpoints: Set[Point] = {
    sets.flatMap(_._3).toSet
  }

  val refPoints: List[Point] = List[(Int, Int)]((0, 0), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (-1, 2), (0, 2),
    (1, 2), (2, 2), (2, 1), (2, 0), (2, -1), (2, -2), (1, -2), (0, -2), (-1, -2), (-2, -2), (-2, -1), (-2, 0), (-2, 1), (-2, 2),
    (-2, 3), (-1, 3), (0, 3), (1, 3), (2, 3), (3, 3), (3, 2), (3, 1)).map(xy => Point(xy._1, xy._2))

  test("CheckRectangles") {

    var ok = sets.length

    sets.foreach(e => {
      val (name, isRect, points) = e
      val pts = points.toArray
      val testRect = RectangleFinder.isRectangle(pts(0), pts(1), pts(2), pts(3))
      println("Testing " + name + ": " + testRect + "/" + isRect)

      if (testRect != isRect) ok = ok-1
    })

    assert(ok == sets.length)
  }

  test("CheckRectangle") {
    assert(!RectangleFinder.isRectangle(Point(-5.0, 0.0),Point(-5.0, 5.0),Point(5.0, -5.0),Point(5.0, 0.0)))
    assert(!RectangleFinder.isRectangle(Point(6.0, 9.0),Point(8.0, 8.0),Point(11.0, 14.0),Point(12.0, 12.0)))
  }

  test("MatchRectangles") {

    var ok = sets.length

    sets.foreach(e => {
      val (name, isRect, points) = e
      val rlist = RectangleFinder.matchRectangles(points)
      val testRect = rlist.size == 1
      println("Testing " + name + ": " + rlist + " - " + testRect + "/" + isRect)

      if (testRect != isRect) ok = ok-1
    })

    assert(ok == sets.length)
  }

  test("MatchRectangles2") {
    println("Within " + allpoints.mkString("{", ", ", "}"))
    val rects = RectangleFinder.matchRectangles(allpoints)
    println(" found " + rects.mkString("{", ", ", "}"))
    println(" -> " + rects.size + " rectangles")
  }

  test("SortPoints") {
    val p1 = Point(0, 0)
    val p2 = Point(0, 1)
    val p3 = Point(1, 0)
    val p4 = Point(1, 1)

    val sorted = RectangleFinder.sortPoints(p4, p2, p3, p1)
    assert(sorted == (p1, p2, p3, p4))
  }

  test("Spiral rotation") {

    var prev: Point = Point(0, 0)
    var max: Point = Point(0, 0)
    var dir: Int = 0

    refPoints.tail.foreach(ref => {
      val pmd = prev.spiralToNext(max, dir)
      prev = pmd._1
      max = pmd._2
      dir = pmd._3
      assert(prev == ref)
    })

  }

  test("Vectors") {
    val up = GVector(Point(2, 2), Point(2, 7))
    val right = GVector(Point(2, 2), Point(7, 2))
    val down = GVector(Point(2, 2), Point(2, -3))
    val left = GVector(Point(2, 2), Point(-3, 2))
    val upright = GVector(Point(2, 2), Point(7, 7))
    val downright = GVector(Point(2, 2), Point(7, -3))
    val downleft = GVector(Point(2, 2), Point(-3, -3))
    val upleft = GVector(Point(2, 2), Point(-3, 7))

    println("U: " + up.canonize)
    println("D: " + down.canonize)
    println("R: " + right.canonize)
    println("L: " + left.canonize)
    println("UR: " + upright.canonize)
    println("DR: " + downright.canonize)
    println("DL: " + downleft.canonize)
    println("UL: " + upleft.canonize)

    assert(left.canonize == right.canonize)
    assert(upleft.canonize == downright.canonize)
    assert(upright.canonize == downleft.canonize)
    assert(up.canonize == down.canonize)

    val v1 = GVector(Point(0, 0), Point(5, 5))
    val v2 = GVector(Point(1, 0), Point(6, 5))
    val v3 = GVector(Point(0, 0), Point(-5, 5))

    println(v1.canonize, v2.canonize, v3.canonize)

    assert(v1.canonize == v2.canonize)
    assert(v1.canonize != v3.canonize)
  }

  test("VectorsToRectangles") {
    var ok = sets.length

    sets.foreach(e => {
      val (name, isRect, points) = e
      println()
      println("Testing " + name + "... should find " + isRect)
      val rlist = GVectorMap.matchRectangles(points)
      val testRect = rlist.size == 1
      println("Tested " + name + ": " + rlist + " - " + testRect + "/" + isRect)

      if (testRect != isRect) ok = ok - 1
    })

    assert(ok == sets.length)
  }

  test("Benchmark") {

    var prev: Point = Point(0, 0)
    var max: Point = Point(0, 0)
    var dir: Int = 0

    val points: List[Point] = prev +: (
      for {
        _ <- 1 to 2048
      } yield {
        val pmd = prev.spiralToNext(max, dir)
        prev = pmd._1
        max = pmd._2
        dir = pmd._3
        prev
      }).toList

    def measure_time[A, B](f: A => B)(p: A): (B, Long) = {
      val startTime: Long = System.currentTimeMillis()
      val res = f(p)
      val estimatedTime: Long = System.currentTimeMillis() - startTime
      (res, estimatedTime)
    }

    println(points)
    for (b <- 1 to 10) {
      val spoints = points.take(b * 50)
      val (rects1, time1) = measure_time[Iterable[Point], Iterable[(Point, Point, Point, Point)]](RectangleFinder.matchRectangles)(spoints)
      val (rects2, time2) = measure_time[Iterable[Point], Iterable[Rectangle]](GVectorMap.matchRectangles)(spoints)

      assert(rects1.size == rects2.size)
      assert(time1 > time2)
      println("With " + (b * 50) + " points, " + rects1.size + " rectangles in " + (time1 / 1000.0) + "s / " + rects2.size + " in " + (time2 / 1000.0) + "s (" + (time1 / time2) + "x acc.)")
    }


  }
}
