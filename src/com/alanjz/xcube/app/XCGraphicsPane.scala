package com.alanjz.xcube.app

import java.awt.{Font, Graphics2D, Graphics, Dimension}
import java.awt.RenderingHints._
import java.awt.Color._
import javax.swing.JPanel

class XCGraphicsPane {
  private val panel = new JPanel {
    override def paint(g : Graphics) = _paint(g)
  }
  private val font = new Font(Font.MONOSPACED, Font.BOLD, 18)

  this.setPreferredSize(new Dimension(500, 500))
  this.setFocusable(true)
  
  def _paint(g : Graphics) : Unit = {
    val g2d = g.asInstanceOf[Graphics2D]

    setRenderingHints(g2d)
    g2d.setColor(black)
    g2d.fillRect(0, 0, this.getWidth, this.getHeight)

    g2d.setColor(white)
    g2d.setFont(font)
    g2d.drawString(s"FPS: ${XCApp.frame.getFPS}", g2d.getFontMetrics.charWidth(' '), g2d.getFontMetrics.getHeight)
    XCApp.cube.paint(g2d)
  }

  private def setRenderingHints(g2d : Graphics2D) : Unit = {
    g2d.setRenderingHint(KEY_ALPHA_INTERPOLATION, VALUE_ALPHA_INTERPOLATION_QUALITY)
    g2d.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(KEY_COLOR_RENDERING, VALUE_COLOR_RENDER_QUALITY)
    g2d.setRenderingHint(KEY_DITHERING, VALUE_DITHER_ENABLE)
    g2d.setRenderingHint(KEY_FRACTIONALMETRICS, VALUE_FRACTIONALMETRICS_ON)
    g2d.setRenderingHint(KEY_INTERPOLATION, VALUE_INTERPOLATION_BICUBIC)
    g2d.setRenderingHint(KEY_RENDERING, VALUE_RENDER_QUALITY)
    g2d.setRenderingHint(KEY_STROKE_CONTROL, VALUE_STROKE_PURE)
    g2d.setRenderingHint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)
  }

  def toJPanel = panel
}

object XCGraphicsPane {
  implicit def toJPanel(lhs : XCGraphicsPane) : JPanel = lhs.toJPanel
}
