package rectangles

import scala.annotation.tailrec
import scala.collection.immutable.{MultiDict, Set}
import scala.collection.parallel.CollectionConverters._

case class Direction(x: Double, y: Double) {
  override def toString: String = "<" + x + "," + y + ">"
}

case class GVector(a: Point, b: Point) {
  lazy val canonize: Direction = {
    val cx = x
    val cy = y

    (cx >= 0, cy >= 0) match {
      case (true, true) => Direction(cx, cy)
      case (true, false) => if (cx == 0) Direction(cx, -cy) else Direction(cx, cy)
      case (false, true) => Direction(-cx, -cy)
      case (false, false) => Direction(-cx, -cy)
    }
  }

  lazy val squareNorm: Double = (x * x) + (y * y) // x2 + y2
  val x: Double = b.x - a.x
  val y: Double = b.y - a.y

  def center: Point = Point(a.x + x / 2, a.y + y / 2)

  def isOrthogonalTo(v: GVector): Boolean = scalarProduct(v) == 0.0

  def scalarProduct(v: GVector): Double = x * v.x + y * v.y

  override def toString: String = "[" + a + " -> " + b + "]"
}

/**
 * GVectorMap represents a associative map where
 *
 * @param points Points to build vectors from
 */
case class GVectorMap(points: Iterable[Point]) {

  lazy val innerMap: MultiDict[Direction, GVector] = MultiDict.from[Direction, GVector](vectorsFrom(points).map(v => (v.canonize, v)))

  def vectors: Iterable[GVector] = innerMap.values

  override def toString: String = {
    innerMap.mkString("", "\n", "")
  }

  @tailrec
  private def innerVectors(points: Iterable[Point], vl: Iterable[GVector]): Iterable[GVector] =
    if (points.isEmpty)
      vl
    else {
      val origin = points.head
      innerVectors(points.tail, vl ++ points.tail.map(GVector(origin, _)))
    }

  private def vectorsFrom(points: Iterable[Point]): Iterable[GVector] = innerVectors(points, Nil)
}

object GVectorMap {

  def matchRectangles(points: Iterable[Point]): Iterable[Rectangle] = {
    val vmap = GVectorMap(points).innerMap

    // Fetch list of directions that has more than one entry
    val eligibleDirs: Iterable[Direction] =
      vmap.filterSets { case (_: Direction, vs: Set[GVector]) => vs.size > 1 }.keySet

    // From this list, try all associations leading to rectangles
    eligibleDirs.par.flatMap { d =>
      val vectors = vmap.get(d)
      for {
        v1 <- vectors
        v2 <- vectors.tail if v2 != v1
        r <- Rectangle.rectangleFrom(v1, v2)
      } yield r
    }.seq
  }

}