package de.digitalculture.particledsl.sph.relaxation

import de.digitalculture.math._
import de.digitalculture.particledsl._
import scala.annotation.tailrec
import scala.collection.mutable.ArraySeq

class DistanceRelaxation(
    minv: Vector,
    maxv: Vector,
    k: Double,
    val ips: Double,
    val maxIter: Int) {

  val kernel = GaussKernel

  def relaxImpulse(particles: ParticleList) = {
    iter(particles._1, Seq.fill(particles.length) { (0.0, 0.0) }, 0)
  }

  @tailrec
  private def iter(positions: VectorList, acc: VectorList, n: Int): VectorList = {
    import ParticleFlowOperations.advancePositions
    val posFwd = advancePositions(positions, acc, minv, maxv, 1)
    //particles.addImpulse(acc).estimatePositions(minv, maxv, dt)._1
    val index = SpatialIndex.build(posFwd, ips)
    if (n >= maxIter) {
      println(s"$n iterations")
      println(acc.max)
      acc
    } else {
      val D = ArraySeq.fill(positions.length)((0.0, 0.0))
      var change = false
      posFwd.indices.foreach {
        i =>
          index.nbIndices(posFwd(i)).foreach {
            j =>
              val distance = posFwd(i).distanceTo(posFwd(j))
              if (distance > 0 && distance < ips) {
                change = true
                val imp = (posFwd(i).diffTo(posFwd(j)).unit) * kernel.kernel2D(posFwd(i), posFwd(j), ips) * -k
                D(i) += imp / 2.0
                D(j) -= imp / 2.0
              }
          }
      }
      if (!change) {
        println(s"$n iterations")
        println(acc.max)
        acc
      } else {
        iter(positions, acc + D, n + 1)
      }
    }
  }

}