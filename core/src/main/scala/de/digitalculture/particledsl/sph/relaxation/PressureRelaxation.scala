package de.digitalculture.particledsl.sph.relaxation

import de.digitalculture.particledsl.sph.pressure.PressureImpulse
import de.digitalculture.particledsl.sph.density.DensityEstimation
import de.digitalculture.particledsl._
import de.digitalculture.math._
import scala.annotation.tailrec

class PressureRelaxation(impulseEst: PressureImpulse,
    densEst: DensityEstimation,
    minv: Vector,
    maxv: Vector,
    dt: Double,
    val rhoMax: Double,
    val maxIter: Int) {

  def relaxImpulse(particles: ParticleList, acc: VectorList) = {
    iter(particles, acc, 0)
  }

  @tailrec
  private def iter(particles: ParticleList, acc: VectorList, n: Int): VectorList = {
    val posFwd = particles.addImpulse(acc).estimatePositions(minv, maxv, dt)._1
    val index = SpatialIndex.build(posFwd, densEst.h)
    val densities = densEst.estimateDensities(posFwd, index)
    if (n >= maxIter || densities.max <= rhoMax) {
      println(s"$n iterations")
      acc
    } else {
      val pressImp = impulseEst.pressureImpulse(densities, posFwd, index)
      iter(particles, acc + pressImp, n + 1)
    }
  }

}