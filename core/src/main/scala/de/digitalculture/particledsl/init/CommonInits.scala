package de.digitalculture.particledsl.init

import de.digitalculture.math._
import de.digitalculture.particledsl._
import de.digitalculture.image.ImageHelper

object CommonInits {

  def withImage(ressource: String, width: Int, height: Int, h: Double, hmin: Double, hmax: Double): ParticleList = {
    val b: Byte = 0
    //    val mask = Seq.fill(width) { Seq.fill(height) { b } }
    val mask = ImageHelper.loadMask(ressource, width, height)
    val positions = PoissonDiskInitialization.fillPDS(mask, h * hmin, h * hmax, Seq.empty[Vector], 20)
    println("%d Particles Initialized!".format(positions.length))
    val velocities = Seq.fill(positions.length)((0.0, 0.0))
    (positions, velocities)
  }

}