package rectangles

import scala.annotation.tailrec

case class Rectangle(a: Point, b: Point, c: Point, d: Point) {
  def toTuple: (Point, Point, Point, Point) = (a, b, c, d)

}

object Rectangle {

  @tailrec
  final def sortPoints(a: Point, b: Point, c: Point, d: Point): (Point, Point, Point, Point) = {
    if ((a.x > b.x) || ((a.x == b.x) && (a.y < b.y))) sortPoints(b, a, c, d)
    else if ((b.x > c.x) || ((b.x == c.x) && (b.y < c.y))) sortPoints(a, c, b, d)
    else if ((c.x > d.x) || ((c.x == d.x) && (c.y < d.y))) sortPoints(a, b, d, c)
    else (a, b, c, d)
  }

  /**
   * Build a rectangle instance from two vectors (if possible)
   *
   * @param v1 First vector
   * @param v2 Second vector
   * @return A rectangle instance from the two vectors, or None if this is not a rectangle
   */
  def rectangleFrom(v1: GVector, v2: GVector): Option[Rectangle] =
    if (!GVector(v1.center, v2.center).isOrthogonalTo(v1))
    // They are in line
      None
    else
      rectangle(v1.a, v1.b, v2.b, v2.a)

  /**
   * Build a rectangle instance from four point, ordering the four points
   *
   * @param _a First point
   * @param _b Second point
   * @param _c Third point
   * @param _d Fourth point
   * @return A rectangle instance from all ordered points, or None if this is not a rectangle
   */
  def rectangle(_a: Point, _b: Point, _c: Point, _d: Point): Option[Rectangle] = {

    val (a: Point, b: Point, c: Point, d: Point) = sortPoints(_a, _b, _c, _d)

    if (a.squareDistanceTo(c) == b.squareDistanceTo(d)) {
      val (p1, p2, p3, p4) = sortPoints(a, b, c, d)
      Some(Rectangle(p1, p2, p3, p4))
    } else
      None
  }

}