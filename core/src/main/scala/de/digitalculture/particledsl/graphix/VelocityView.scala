package de.digitalculture.particledsl.graphix

import java.awt.Dimension
import de.digitalculture.math._
import de.digitalculture.particledsl._
import ij.gui.Line

class VelocityView(val width: Int, val height: Int, val zoom: Int, name: String = "") extends Visualization(new Dimension(width * zoom, height * zoom), s"Velocities - $name") {

  def update(pl: ParticleList) {
    val a = Array.fill(proc.getWidth * proc.getHeight)(255.asInstanceOf[Byte])
    proc.setPixels(a)
    val rois = pl.map {
      p =>
        val (pos, vel) = p
        new Line(pos._1 * zoom, pos._2 * zoom, (pos + vel)._1 * zoom, (pos + vel)._2 * zoom)
    }
    rois.foreach { roi =>
      roi.drawPixels(proc)
    }

    img.updateAndDraw

  }
}