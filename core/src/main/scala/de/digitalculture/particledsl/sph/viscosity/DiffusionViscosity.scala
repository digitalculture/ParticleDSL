package de.digitalculture.particledsl.sph.viscosity

import de.digitalculture.math._
import de.digitalculture.particledsl._
import de.digitalculture.particledsl.sph.density.DensityEstimation

class DiffusionViscosity(kernel: Kernel, mu: Double) {
  val nullvector: Vector = (0.0, 0.0)

  def viscousImpulse(particles: ParticleList, h: Double) = {
    val (pos, vel) = particles.unzip
    val index = SpatialIndex.build(pos, h)
    val dens = new DensityEstimation(kernel, h).estimateDensities(pos, index)
    particles.indices.map {
      p =>
        index.nbIndices(pos(p)).map {
          nb =>
            (vel(nb) * kernel.laplace2D(pos(p), pos(nb), h) / dens(nb) -
              vel(p) * kernel.laplace2D(pos(nb), pos(p), h) / dens(p)) / 2.0 * mu
        }.fold(nullvector)(_ + _)
    }
  }

}