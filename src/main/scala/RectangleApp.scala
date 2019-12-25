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

  var points = scala.collection.mutable.Set[Point]()

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

  def pointToXY(p: Point): (Double, Double) = {
    val x = p.x * scale + width / 2
    val y = height / 2 - p.y * scale

    (x, y)
  }

  def drawPoint(p: Point): Unit = {
    val (x, y) = pointToXY(p)

    gc.stroke = Black
    gc.strokeRect(x-1, y-1, 2, 2)
  }

  def pointFromMouse(x: Double, y: Double): Point = {
    val px = Math.round((x - width / 2) / scale)
    val py = Math.round((height / 2 - y) / scale)
    Point(px, py)
  }

  def drawPoints(points: Iterable[Point]) =
    points.foreach(drawPoint)

  def drawRect(rect: (Point, Point, Point, Point)) = {
    val (a, b, c, d) = rect

    gc.stroke = Red
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

    gc.strokePath()
    // gc.closePath()
  }

  def drawRects(points: List[Point]): Unit = {
    drawPoints(points)

    val rects: List[(Point, Point, Point, Point)] = RectangleFinder.matchRectangles(points.toList)
    val (plen, rlen) = (points.length, rects.length)

    rects.foreach(drawRect)

    gc.stroke = Gray
    gc.strokeText(s"$rlen rectangle(s) out of $plen points.", 8, height - 40)
  }

  onMousePressed = (me: MouseEvent) => {
    val mp = pointFromMouse(me.sceneX, me.sceneY)

    if (points.contains(mp))
      points.remove(mp)
    else
      points.add(mp)

    drawBackground()
    drawPoints(points)
    drawRects(points.toList)
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
    points = scala.collection.mutable.Set[Point]()

    drawBackground()
    drawRects(points.toList)
  }

  drawBackground()
  drawRects(points.toList)

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
      val resetButton = new Button("Réinitialiser")
      resetButton.setLayoutX(10)
      resetButton.setLayoutY(10)
      resetButton.onAction = handle  {
          panel.reset
      }
      content = new Group(panel, resetButton)
    }
  }
}
