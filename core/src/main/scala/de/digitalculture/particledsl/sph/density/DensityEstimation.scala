package de.digitalculture.particledsl.sph.density

import de.digitalculture.math._
import de.digitalculture.particledsl._

class DensityEstimation(val kernel: Kernel, val h: Double) {

  def estimateDensities(positions: VectorList, index: SpatialIndex) = {
    index.mapReduceWithNBs[Double](positions,
      (ps1, ps2) => kernel.kernel2D(ps1, ps2, h),
      (w1, w2) => w1 + w2)
  }

}