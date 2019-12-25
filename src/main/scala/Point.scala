package RectangleTest

case class Point(x: Double, y: Double) {

  def squareDistanceTo(b: Point) = {
    val dx = b.x - x
    val dy = b.y - y
    (dx * dx) + (dy * dy)
  }

  def scaleDownBy(c: Int): Point =
    Point(x / c, y / c)

  override def toString: String = "(" + x + ", " + y + ")"
}
