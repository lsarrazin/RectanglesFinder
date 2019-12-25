package RectangleTest

object RectangleFinder {

  def isRectangle(au: Point, bu: Point, cu: Point, du: Point): Boolean = {
    val (a, b, c, d) = sortPoints(au, bu, cu, du)
    val ab2 = a.squareDistanceTo(b)
    val bc2 = b.squareDistanceTo(c)
    val cd2 = c.squareDistanceTo(d)
    val da2 = d.squareDistanceTo(a)

    (ab2 == cd2) && (bc2 == da2) && isOrthogonal(a, b, d) && isOrthogonal(b, d, c)
  }

  def isOrthogonal(a: Point, b: Point, c: Point): Boolean = {
    val ab2 = a.squareDistanceTo(b)
    val bc2 = b.squareDistanceTo(c)
    val ca2 = c.squareDistanceTo(a)

    if ((ab2 > bc2) && (ab2 > ca2)) ab2 == bc2 + ca2
    else if ((bc2 > ab2) && (bc2 > ca2)) bc2 == ca2 + ab2
    else ca2 == ab2 + bc2
  }

  def sortPoints(a: Point, b: Point, c: Point, d: Point): (Point, Point, Point, Point) = {
    if ((a.x > b.x) || ((a.x == b.x) && (a.y > b.y))) sortPoints(b, a, c, d)
    else if ((b.x > c.x) || ((b.x == c.x) && (b.y > c.y))) sortPoints(a, c, b, d)
    else if ((c.x > d.x) || ((c.x == d.x) && (c.y > d.y))) sortPoints(a, b, d, c)
    else (a, b, c, d)
  }

  def matchRectangles(points: List[Point]) : List[(Point, Point, Point, Point)] = {
    if (points.length < 4)
      Nil
    else {

      val matches =
      for {
        a <- points
        b <- points.tail if (a != b)
        c <- points.tail.tail if ((a != c) && (b != c) && isOrthogonal(a, b, c))
        d <- points.tail.tail.tail if ((a != d) && (b != d) && (c != d) && isRectangle(a, b, c, d))
      } yield {
        sortPoints(a, b, c, d)
      }
      matches.toSet.toList
    }
  }
}
