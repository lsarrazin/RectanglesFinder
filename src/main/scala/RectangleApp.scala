import RectangleTest.Point
import RectangleTest.RectangleFinder
import javafx.event.EventHandler
import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Button
import scalafx.scene.{Group, Node, Scene}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Line, Rectangle}

class DrawPane(width: Int, height: Int) extends Canvas(width, height) {

  val scale = 20

  val gc = graphicsContext2D

  val points = scala.collection.mutable.Set[Point]()
  var rects: List[(Point, Point, Point, Point)] = Nil

  def pointToXY(p: Point): (Double, Double) =
    (p.x * scale + width / 2, height / 2 - p.y * scale)

  def pointFromMouse(x: Double, y: Double): Point =
    Point(Math.round((x - width / 2) / scale), Math.round((height / 2 - y) / scale))

  def drawBackground() = {

    gc.fill = Wheat
    gc.fillRect(0, 0, width, height)

    gc.stroke = LightBlue
    gc.setLineDashes(3.0, 2.0)

    for ( x <- ((width / 2) % scale) to width by scale )
      gc.strokeLine(x, 0, x, height)

    for ( y <- ((height/2) % scale) to height by scale )
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

  def drawPoints() =
    points.foreach(drawPoint)

  def drawRect(rect: (Point, Point, Point, Point), color: Color = Navy, highlight: Boolean = false) = {
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

  def drawRects(): Unit = {

    rects.foreach(drawRect(_, Black))
    drawPoints()

    val (plen, rlen) = (points.size, rects.length)

    gc.stroke = Gray
    gc.strokeText(s"$rlen rectangle(s) out of $plen points.", 8, height - 40)
  }

  def refreshRects(): Unit = {
    rects = RectangleFinder.matchRectangles(points.toList)
    currentRect = 0
  }

  onMousePressed = (me: MouseEvent) => {
    val mp = pointFromMouse(me.sceneX, me.sceneY)

    if (points.contains(mp))
      points.remove(mp)
    else
      points.add(mp)

    refreshRects()

    drawBackground()
    drawPoints()
    drawRects()
  }

  onMouseMoved = (me: MouseEvent) => {
    val mp = pointFromMouse(me.sceneX, me.sceneY)

    gc.stroke = Wheat
    gc.fill = Wheat
    gc.fillRect(width - 80, height - 56, 80, 56)
    gc.stroke = Gray
    gc.strokeText(s"${mp.x}, ${mp.y}", width - 76, height - 40)

  }

  def reset: Unit = {
    points.clear()
    rects = Nil

    drawBackground()
  }

  def dump: Unit = {
    println("points = " + points.mkString("[", ", ", "]"))
    println("rects = " + rects.mkString("[", ", ", "]"))
  }

  var currentRect: Int = 0

  def rotateLeft: Unit = {
    if (!rects.isEmpty) {
      drawBackground()
      drawRects()
      drawRect(rects(currentRect), Red, true)
      currentRect += 1
      if (currentRect >= rects.length) currentRect = 0
      drawPoints()
    }
  }

  def rotateRight: Unit = {
    if (!rects.isEmpty) {
      drawBackground()
      drawRects()
      drawRect(rects(currentRect), Red, true)
      currentRect -= 1
      if (currentRect < 0) currentRect = rects.length - 1
      drawPoints()
    }
  }

  drawBackground()
  drawRects()

}

object RectangleApp extends JFXApp {
  val w = 1200
  val h = 900

  stage = new JFXApp.PrimaryStage {
    title.value = "Admission Google"
    width = w
    height = h
    scene = new Scene {
      maxWidth = w
      maxHeight = h
      fill = White

      val panel = new DrawPane(w, h)
      val resetButton = new Button("Réinitialiser") {
        layoutX = 10
        layoutY = 10
        onAction = handle { panel.reset }
      }
      val dumpButton = new Button("Générer...") {
        layoutX = 10
        layoutY = 70
        onAction = handle { panel.dump }
      }
      val highlightLeft = new Button("<<") {
        layoutX = 10
        layoutY = 40
        onAction = handle { panel.rotateLeft }
      }
      val highlightRight = new Button(">>") {
        layoutX = 64
        layoutY = 40
        onAction = handle { panel.rotateRight }
      }
      content = new Group(panel, resetButton, highlightLeft, highlightRight, dumpButton)
    }
  }
}
