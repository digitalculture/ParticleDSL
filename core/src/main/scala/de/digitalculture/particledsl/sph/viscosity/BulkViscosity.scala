package de.digitalculture.particledsl.sph.viscosity

import de.digitalculture.math._
import de.digitalculture.math.Kernel
import de.digitalculture.particledsl._
import de.digitalculture.particledsl.sph.density.DensityEstimation

class BulkViscosity(c: Double, alpha: Double = 1.0, beta: Double = 2.0) {

  def viscousImpulse(particles: ParticleList, h: Double) = {
    val (pos, vel) = particles.unzip
    val etasq: Double = 0.01 * h * h
    def muij(i: Int, j: Int) = {
      (ij(vel(i), vel(j)) * h).dot(ij(pos(i), pos(j))) /
        (ij(pos(i), pos(j)).dot(ij(pos(i), pos(j))) + etasq)
    }

    val index = SpatialIndex.build(pos, h)
    val densest = new DensityEstimation(GaussKernel, h)
    val rho = densest.estimateDensities(pos, index)
    particles.indices.map {
      i =>

        index.nbIndices(pos(i)).map {
          nb =>
            if (pos(i).distanceTo(pos(nb)) > 0 &&
              pos(i).distanceTo(pos(nb)) < h &&
              ij(vel(i), vel(nb)).dot(ij(pos(i), pos(nb))) < 0) {
              val mu = muij(i, nb)
              val piij = densest.kernel.nabla2D(pos(i), pos(nb), h) *
                (-alpha * c * mu + beta * mu * mu) /
                (barij(rho(i), rho(nb)) * barij(rho(i), rho(nb)))
              piij
            } else { (0.0, 0.0) }
        }.foldLeft((0.0, 0.0))((v1, v2) => v1 + v2)
    }
  }

  def ij(i: Double, j: Double) = i - j
  def ij(i: Vector, j: Vector): Vector = i - j
  def barij(i: Double, j: Double) = (i + j) / 2.0
  def barij(i: Vector, j: Vector): Vector = (i + j) / 2.0

}