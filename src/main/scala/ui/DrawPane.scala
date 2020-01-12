package ui

import rectangles.{GVectorMap, Point}
import scalafx.Includes._
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.{Black, Blue, Gray, LightBlue, Navy, Red, Wheat}

import scala.collection.mutable

class DrawPane(width: Int, height: Int) extends Canvas(width, height) {

  val scale = 20

  val gc: GraphicsContext = graphicsContext2D

  val points = mutable.Set.empty[Point]
  val rects = mutable.Set.empty[(Point, Point, Point, Point)]

  def pointToXY(p: Point): (Double, Double) =
    (p.x * scale + width / 2, height / 2 - p.y * scale)

  def pointFromMouse(x: Double, y: Double): Point =
    Point(Math.round((x - width / 2) / scale), Math.round((height / 2 - y) / scale))

  def drawBackground(): Unit = {
    gc.fill = Wheat
    gc.fillRect(0, 0, width, height)

    gc.stroke = LightBlue
    gc.setLineDashes(3.0, 2.0)

    for (x <- ((width / 2) % scale) to width by scale)
      gc.strokeLine(x, 0, x, height)

    for (y <- ((height / 2) % scale) to height by scale)
      gc.strokeLine(0, y, width, y)

    gc.stroke = Blue
    gc.setLineDashes(1.0)

    gc.strokeLine(width / 2, height, width / 2, 0)
    gc.strokeLine(0, height / 2, width, height / 2)
  }

  def drawPoint(p: Point): Unit = {
    val (x, y) = pointToXY(p)

    gc.stroke = Black
    gc.strokeRect(x-1, y-1, 2, 2)
  }

  def drawPoints(): Unit =
    points.foreach(drawPoint)

  def drawRect(rect: (Point, Point, Point, Point), color: Color = Navy, highlight: Boolean = false): Unit = {
    val (a, b, c, d) = rect

    if (highlight) {
      gc.save()
      gc.setGlobalAlpha(0.5)
    }

    gc.beginPath()
    val (ax, ay) = pointToXY(a)
    gc.moveTo(ax, ay)
    val (bx, by) = pointToXY(b)
    gc.lineTo(bx, by)
    val (cx, cy) = pointToXY(d)
    gc.lineTo(cx, cy)
    val (dx, dy) = pointToXY(c)
    gc.lineTo(dx, dy)
    gc.lineTo(ax, ay)
    gc.closePath()

    gc.stroke = color
    if (highlight) {
      gc.fill = color
      gc.fillPath()
      gc.restore()
    } else {
      gc.strokePath()
    }
  }

  def refreshRects(): Unit = {
    rects.clear()
    // rects.addAll(RectangleFinder.matchRectangles(points.toSet))
    rects.addAll(GVectorMap.matchRectangle2(points))
    currentRect = 0
  }

  onMousePressed = (me: MouseEvent) => {
    val mp = pointFromMouse(me.sceneX, me.sceneY)

    if (points.contains(mp)) {
      points.remove(mp)

      refreshRects()

      drawBackground()
      drawPoints()
      drawRects(rects)
    } else {
      points.add(mp)
      drawPoint(mp)

      //val newRects = RectangleFinder.rematchRectangles(mp, points)
      val newRects = GVectorMap.matchRectangle2(points)
      drawRects(newRects)
      //rects.addAll(newRects)
      rects.clear()
      rects.addAll(newRects)
      currentRect = rects.size - 1
    }
  }

  onMouseMoved = (me: MouseEvent) => {
    val mp = pointFromMouse(me.sceneX, me.sceneY)

    gc.stroke = Wheat
    gc.fill = Wheat
    gc.fillRect(width - 80, height - 56, 80, 56)
    gc.stroke = Gray
    gc.strokeText(s"${mp.x}, ${mp.y}", width - 76, height - 40)

  }

  def reset(): Unit = {
    points.clear()
    rects.clear()
    drawBackground()
  }

  def dump(): Unit = {
    println("points = " + points.mkString("[", ", ", "]"))
    println("rects = " + rects.mkString("[", ", ", "]"))
  }

  var currentRect: Int = 0

  def nthRect(n: Int): (Point, Point, Point, Point) = {
    if ((n < 0) || (n >= rects.size))
      (Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0))
    else {
      if (n == 0)
        rects.head
      else
        rects.take(n).last
    }
  }

  def drawRects(rects: Iterable[(Point, Point, Point, Point)]): Unit = {

    rects.foreach(drawRect(_, Black))
    drawPoints()

    val (plen, rlen) = (points.size, this.rects.size)

    gc.stroke = Wheat
    gc.fill = Wheat
    gc.fillRect(0, height - 56, 320, 56)
    gc.stroke = Gray
    gc.strokeText(s"$rlen rectangle(s) / $plen points.", 8, height - 40)
  }

  def rotateRight(): Unit = {
    if (rects.nonEmpty) {
      currentRect -= 1
      if (currentRect < 0) currentRect = rects.size - 1

      drawBackground()
      drawRects(rects)
      drawRect(nthRect(currentRect), Red, highlight = true)
      drawPoints()
    }
  }

  def rotateLeft(): Unit = {
    if (rects.nonEmpty) {
      currentRect += 1
      if (currentRect >= rects.size) currentRect = 0

      drawBackground()
      drawRects(rects.toSet)
      drawRect(nthRect(currentRect), Red, highlight = true)
      drawPoints()
    }
  }

  drawBackground()
  drawRects(rects)

}
