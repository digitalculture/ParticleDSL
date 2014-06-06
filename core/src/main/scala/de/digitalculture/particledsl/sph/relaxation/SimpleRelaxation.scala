package de.digitalculture.particledsl.sph.relaxation

import de.digitalculture.particledsl.sph.density.DensityEstimation
import de.digitalculture.particledsl._
import de.digitalculture.math._
import scala.annotation.tailrec
import scala.collection.mutable.ArraySeq
import de.digitalculture.particledsl.sph.NeighborModel

class SimpleRelaxation(densEst: DensityEstimation,
    minv: Vector,
    maxv: Vector,
    dt: Double,
    k: Double,
    val rhoMax: Double,
    val maxIter: Int) {

  def relaxImpulse(particles: ParticleList) = {
    iter(particles._1, Seq.fill(particles.length) { (0.0, 0.0) }, 0)
  }

  @tailrec
  private def iter(positions: VectorList, acc: VectorList, n: Int): VectorList = {
    import ParticleFlowOperations.advancePositions
    val posFwd = advancePositions(positions, acc, minv, maxv, 1)
    //particles.addImpulse(acc).estimatePositions(minv, maxv, dt)._1
    val index = SpatialIndex.build(posFwd, densEst.h)
    val densities = densEst.estimateDensities(posFwd, index)
    println("%d: %f <= %f (%b)".format(n, densities.max, rhoMax, densities.max <= rhoMax))
    if (n >= maxIter || densities.max <= rhoMax) {
      println(s"$n iterations")
      println(acc.max)
      acc
    } else {
      val D = ArraySeq.fill(positions.length)((0.0, 0.0))
      posFwd.indices.filter(i => densities(i) > rhoMax).map { i =>
        index.nbIndices(posFwd(i)).filter(nb => posFwd(i).distanceTo(posFwd(nb)) > 0 &&
          posFwd(i).distanceTo(posFwd(nb)) < densEst.h).map { nb =>
          val imp = (posFwd(i).diffTo(posFwd(nb)).unit) * densEst.kernel.kernel2D(posFwd(i), posFwd(nb), densEst.h) * -k
          if (posFwd(i).diffTo(posFwd(nb)).unit.dot(imp) < 0) {
            D(i) += imp / 2.0
            D(nb) -= imp / 2.0
          }
        }
      }
      iter(positions, acc + D, n + 1)
    }
  }

}