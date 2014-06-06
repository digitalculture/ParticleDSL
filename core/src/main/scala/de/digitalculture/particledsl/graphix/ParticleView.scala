package de.digitalculture.particledsl.graphix

import ij.ImagePlus
import java.awt.Dimension
import de.digitalculture.particledsl._

class ParticleView(val width: Int, val height: Int, val zoom: Int, title: String = "") extends Visualization(new Dimension(width * zoom, height * zoom), "Particles - %s".format(title)) {

  def update(pl: ParticleList) {
    //start with a white(255) plane and paint a black(0) dot where the particles are
    val a = Array.fill(proc.getWidth * proc.getHeight)(255.asInstanceOf[Byte])
    pl._1.map {
      pos =>
        a(ix((pos._1 * zoom).toInt, (pos._2 * zoom).toInt)) = 0
        if (pos._1 > 0) a(ix((pos._1 * zoom - 1).toInt, (pos._2 * zoom).toInt)) = 0
        if (pos._2 > 0) a(ix((pos._1 * zoom).toInt, (pos._2 * zoom - 1).toInt)) = 0
        if (pos._1 < proc.getWidth) a(ix((pos._1 * zoom + 1).toInt, (pos._2 * zoom).toInt)) = 0
        if (pos._2 < proc.getWidth) a(ix((pos._1 * zoom).toInt, (pos._2 * zoom + 1).toInt)) = 0
    }
    updatePixels(a)

  }
}