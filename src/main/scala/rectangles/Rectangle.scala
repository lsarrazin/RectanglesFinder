package rectangles

case class Rectangle(a: Point, b: Point, c: Point, d: Point) {
  def toTuple: (Point, Point, Point, Point) = (a, b, c, d)
}

object Rectangle {

  /**
   * Sorts four points into a canonical order: primarily by x-coordinate, secondarily by y-coordinate.
   */
  private def sortPoints(p1: Point, p2: Point, p3: Point, p4: Point): (Point, Point, Point, Point) = {
    val sorted = List(p1, p2, p3, p4).sortWith((pA, pB) =>
      if (pA.x != pB.x) pA.x < pB.x else pA.y < pB.y
    )
    (sorted(0), sorted(1), sorted(2), sorted(3))
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