package de.digitalculture.io.gnuplot

import de.digitalculture.math._
import de.digitalculture.particledsl._
import de.digitalculture.particledsl.sph.density.DensityEstimation
import java.io.FileWriter

object MeanQuantSplot {

  def plot(file: String, particles: ParticleList, quant: Seq[Double], h: Double, width: Int, height: Int) = {
    import de.digitalculture.math.weightedMean
    val kernel = GaussKernel
    val (pos, vel) = particles.unzip
    val index = SpatialIndex.build(pos, h)
    //val dens = new DensityEstimation(kernel, h).estimateDensities(pos, index)
    val actDens = (p: (Double, Double)) =>
      sigma(index.nbIndices(p), (i: Int) => kernel.kernel2D(p, pos(i), h))
    val writer = new FileWriter(file, false)
    (0 to width).foreach { w =>
      (0 to height).foreach { h =>
        val a = if (index.nbIndices((w, h)).length > 0) {
          val weights = index.nbIndices((w, h)).map { i =>
            kernel.kernel2D((w, h), pos(i), h)
          }
          weightedMean(quant, weights)
        } else { 0.0 }

        writer.write("%d %d %f\n".format(w, h, a))
      }
    }
  }
}