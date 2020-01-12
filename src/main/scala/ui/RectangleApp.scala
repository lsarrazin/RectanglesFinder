package ui

import scalafx.Includes.handle
import scalafx.application.JFXApp
import scalafx.scene.control.Button
import scalafx.scene.paint.Color.White
import scalafx.scene.{Group, Scene}

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
      val resetButton: Button = new Button("Réinitialiser") {
        layoutX = 10
        layoutY = 10
        onAction = handle {
          panel.reset()
        }
      }
      val dumpButton: Button = new Button("Générer...") {
        layoutX = 10
        layoutY = 70
        onAction = handle {
          panel.dump()
        }
      }
      val highlightLeft: Button = new Button("<<") {
        layoutX = 10
        layoutY = 40
        onAction = handle {
          panel.rotateLeft()
        }
      }
      val highlightRight: Button = new Button(">>") {
        layoutX = 64
        layoutY = 40
        onAction = handle {
          panel.rotateRight()
        }
      }
      content = new Group(panel, resetButton, highlightLeft, highlightRight, dumpButton)
    }
  }
}
