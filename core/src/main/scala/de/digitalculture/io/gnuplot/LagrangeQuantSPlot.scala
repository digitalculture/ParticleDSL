package de.digitalculture.io.gnuplot

import de.digitalculture.math._
import de.digitalculture.particledsl._
import java.io.FileWriter
import de.digitalculture.particledsl.sph.density.DensityEstimation

object LagrangeQuantSPlot {

  def plot(file: String, particles: ParticleList, quant: Seq[Double], h: Double, width: Int, height: Int) = {
    val kernel = GaussKernel
    val (pos, vel) = particles.unzip
    val index = SpatialIndex.build(pos, h)
    val dens = new DensityEstimation(kernel, h).estimateDensities(pos, index)
    val writer = new FileWriter(file, false)

    (0 to width).foreach { w =>
      (0 to height).foreach { h =>
        val quantNbs = (i: Int) => {
          quant(i) / dens(i) * kernel.kernel2D((w, h), pos(i), h)
        }
        val a = sigma(index.nbIndices((w, h)), quantNbs)
        writer.write("%d %d %f\n".format(w, h, a))
      }
      writer.write("\n")
    }
    writer.flush()
    writer.close()
  }

}