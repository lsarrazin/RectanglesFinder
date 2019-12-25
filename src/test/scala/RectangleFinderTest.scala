package RectangleTest

class RectangleFinderTest extends org.scalatest.FunSuite {

  val losange1 = List(Point(0,0), Point(2,2), Point(2,0), Point(4,2))
  val trapeze1 = List(Point(8,0), Point(3,2), Point(5,2), Point(0,0))
  val rectangle1 = List(Point(0,0), Point(1,0), Point(0,1), Point(1,1))
  val rectangle2 = List(Point(2,2), Point(0,0), Point(0,2), Point(2,0))
  val rectangle3 = List(Point(2,4), Point(0,0), Point(0,4), Point(2,0))
  val blob1 = List(Point(0,0), Point(8,0), Point(0,8), Point(4,4))

  val sets : List[(String, Boolean, List[Point])] = List(
    ("Losange1", false, losange1),
    ("Trapeze1", false, trapeze1),
    ("Rectangle1", true, rectangle1),
    ("Rectangle2", true, rectangle2),
    ("Rectangle3", true, rectangle3),
    ("Blob1", false, blob1)
  )

  val allpoints: List[Point] = {
    sets.flatMap(_._3).toSet.toList
  }

  test("CheckRectangles") {

    var ok = sets.length

    sets.foreach(e => {
      val (name, isRect, points) = e
      val testRect = RectangleFinder.isRectangle(points(0), points(1), points(2), points(3))
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
      val testRect = rlist.length == 1
      println("Testing " + name + ": " + rlist + " - " + testRect + "/" + isRect)

      if (testRect != isRect) ok = ok-1
    })

    assert(ok == sets.length)
  }

  test("MatchRectangles2") {
    println("Within " + allpoints.mkString("{", ", ", "}"))
    val rects = RectangleFinder.matchRectangles(allpoints)
    println(" found " + rects.mkString("{", ", ", "}"))
    println(" -> " + rects.length + " rectangles")
  }
}
