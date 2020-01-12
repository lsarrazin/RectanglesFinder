package rectangles

case class Point(x: Double, y: Double) {

  def squareDistanceTo(b: Point): Double = {
    val dx = b.x - x
    val dy = b.y - y
    (dx * dx) + (dy * dy)
  }

  def scaleDownBy(c: Int): Point =
    Point(x / c, y / c)

  // Directions : 0: Up, 1: Right, 2: Down, 3: Left
  def spiralToNext(max: Point, direction: Int = 0): (Point, Point, Int) =
    direction match {
      case 0 => if (y < max.y) (Point(x, y + 1), max, direction)
      else (Point(x, y + 1), Point(max.x, max.y + 1), 1)
      case 1 => if (x < max.x) (Point(x + 1, y), max, 1)
      else (Point(x + 1, y), Point(max.x + 1, max.y), 2)
      case 2 => if (y > -max.y) (Point(x, y - 1), max, direction)
      else (Point(x - 1, y), max, 3)
      case 3 => if (x > -max.x) (Point(x - 1, y), max, 3)
      else (Point(x, y + 1), max, 0)
    }

  override def toString: String = "(" + x + ", " + y + ")"
}
