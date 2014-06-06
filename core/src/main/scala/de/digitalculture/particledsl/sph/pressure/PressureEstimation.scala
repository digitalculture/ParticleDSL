package de.digitalculture.particledsl.sph.pressure

import de.digitalculture.math._
import de.digitalculture.particledsl._

class PressureEstimation(val rho0: Double, val k: Double) {
  def estimatePressures(densities: Seq[Double]) = densities.map { rho => k * (rho - rho0) }
}

class PressureGradient(val kernel: Kernel, val h: Double, pressureEstimation: PressureEstimation) {
  def pressureGradient(densities: Seq[Double], positions: VectorList, index: SpatialIndex): Seq[Vector] = {
    val pressures = pressureEstimation.estimatePressures(densities)

    def firstTerm(i: Int)(nb: Int) = kernel.nabla2D(positions(i), positions(nb), h)

    def secondTerm(i: Int)(nb: Int) = kernel.nabla2D(positions(i), positions(nb), h) * pressures(i) / ((densities(nb) * densities(nb)))

    for (i <- positions.indices)
      yield sigma(index.nbIndices(positions(i)).filter(nb => positions(i).distanceTo(positions(nb)) < h),
      firstTerm(i)_) * (pressures(i)) / (densities(i) / densities(i)) +
      sigma(index.nbIndices(positions(i)).filter(nb => positions(i).distanceTo(positions(nb)) < h),
        secondTerm(i)_)
  }
}

class PressureImpulse(grad: PressureGradient) {

  def pressureImpulse(densities: Seq[Double], positions: VectorList, index: SpatialIndex): Seq[Vector] =
    grad.pressureGradient(densities, positions, index).map { v => -v }

}