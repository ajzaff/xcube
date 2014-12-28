package com.alanjz.xcube.app

import javax.swing.{SwingUtilities, JFrame}

class XCFrame extends Runnable {
  private val frame = new JFrame("XCube\t[Puzzle Cube Explorer]")
  private val that = this
  private val th = new Thread(this)
  val fps = 100
  private var _reportedFPS = 0

  SwingUtilities.invokeLater(new Runnable {
    override def run() = {
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.add(new XCGraphicsPane)
      frame.setResizable(false)
      frame.setVisible(true)
      frame.pack()
      frame.setLocationRelativeTo(null)
      that.start()
    }
  })

  def getFPS = _reportedFPS

  def start() = {
    th.start()
  }

  def stop() = System.exit(0)

  override def run() = {
    val delta = 1e9 / fps
    while(true) {
      val start = System.nanoTime()
      this.repaint()
      while(System.nanoTime() - start < delta)
        Thread sleep (0, 100)
      _reportedFPS = (fps * (System.nanoTime() - start) / delta).toInt
    }
  }

  def toJFrame = frame
}

object XCFrame {
  implicit def toJFrame(lhs : XCFrame) : JFrame = lhs.toJFrame
}