package rectangles

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object RectangleFinder {

  /**
   * Find rectangles within a set of points
   *
   * @param points Set of points to consider
   * @return Set of rectangles (Tuple4[Point])
   */
  def matchRectangles(points: Iterable[Point]): Iterable[(Point, Point, Point, Point)] = {
    if (points.size < 4)
      Set.empty[(Point, Point, Point, Point)]
    else {
      val rects =
        for {
          a <- points.par
          b <- points.tail if a != b
          c <- points.tail.tail if (a != c) && (b != c) && isOrthogonal(a, b, c)
          d <- points.tail.tail.tail if (a != d) && (b != d) && (c != d) && isRectangle(a, b, c, d)
        } yield {
          sortPoints(a, b, c, d)
        }
      Set.from(rects.seq)
    }
  }

  /**
   * Check if the angle abc is 90°
   *
   * @param a first point
   * @param b second point
   * @param c third point
   * @return true if abc is 90°
   */
  def isOrthogonal(a: Point, b: Point, c: Point): Boolean = {
    val ab2 = a.squareDistanceTo(b)
    val bc2 = b.squareDistanceTo(c)
    val ca2 = c.squareDistanceTo(a)

    if ((ab2 > bc2) && (ab2 > ca2)) ab2 == bc2 + ca2
    else if ((bc2 > ab2) && (bc2 > ca2)) bc2 == ca2 + ab2
    else ca2 == ab2 + bc2
  }

  /**
   * Order four points using a left most upper most algorithm
   *
   * @param a first point
   * @param b second point
   * @param c third point
   * @param d fourth point
   * @return abcd ordered
   */
  @tailrec
  def sortPoints(a: Point, b: Point, c: Point, d: Point): (Point, Point, Point, Point) = {
    if ((a.x > b.x) || ((a.x == b.x) && (a.y < b.y))) sortPoints(b, a, c, d)
    else if ((b.x > c.x) || ((b.x == c.x) && (b.y < c.y))) sortPoints(a, c, b, d)
    else if ((c.x > d.x) || ((c.x == d.x) && (c.y < d.y))) sortPoints(a, b, d, c)
    else (a, b, c, d)
  }

  def rematchRectangles(newPoint: Point, points: Iterable[Point]): Iterable[(Point, Point, Point, Point)] = {
    if (points.size < 3)
      Set.empty[(Point, Point, Point, Point)]
    else {
      val newRects =
        for {
          b <- points.par if newPoint != b
          c <- points.tail if (newPoint != c) && (b != c) && isOrthogonal(newPoint, b, c)
          d <- points.tail.tail if (newPoint != d) && (b != d) && (c != d) && isRectangle(newPoint, b, c, d)
        } yield
          sortPoints(newPoint, b, c, d)

      newRects.seq
    }
  }

  /**
   * Test if abcd is rectangle
   *
   * @param au first point
   * @param bu second point
   * @param cu third point
   * @param du fourth point
   * @return true is abcd forms a rectangle
   */
  def isRectangle(au: Point, bu: Point, cu: Point, du: Point): Boolean = {
    val (a, b, c, d) = sortPoints(au, bu, cu, du)
    val ab2 = a.squareDistanceTo(b)
    val bc2 = b.squareDistanceTo(c)
    val cd2 = c.squareDistanceTo(d)
    val da2 = d.squareDistanceTo(a)

    (ab2 == cd2) && (bc2 == da2) && isOrthogonal(a, b, c) && isOrthogonal(b, c, d)
  }
}
