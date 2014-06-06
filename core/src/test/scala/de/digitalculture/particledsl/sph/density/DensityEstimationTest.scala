package de.digitalculture.particledsl.sph.density

import org.junit.Test
import org.junit.Assert._
import de.digitalculture.particledsl.SpatialIndex

import de.digitalculture.math._

class DensityEstimationTest {

  val delta = 0.00001

  @Test def densityEstimationTest() {
    val h = 2.0
    val kernel = GaussKernel
    val positions = Seq((0.0, 0.0), (1.0, 1.0), (10.0, 10.0))
    val index = SpatialIndex.build(positions, h)
    val densEstimation = new DensityEstimation(kernel, h)
    val densities = densEstimation.estimateDensities(positions, index)

    assertEquals("Should have length 3!", densities.length, positions.length)
    assertEquals("Should be the same!", densities(0), densities(1), delta)
    assertEquals("Density value not right!", densities(0),
      kernel.kernel2D(positions(0), positions(1), h) +
        kernel.kernel2D(positions(0), positions(0), h), delta)
    assertEquals("Density Value not right!", densities(2),
      kernel.kernel2D(positions(2), positions(2), h), delta)
  }

}