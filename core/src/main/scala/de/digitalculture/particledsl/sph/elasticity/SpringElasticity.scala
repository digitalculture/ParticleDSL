package de.digitalculture.particledsl.sph.elasticity

import de.digitalculture.particledsl._
import de.digitalculture.math._

class SpringElasticity(gamma: Double, restLength: Double, kspring: Double) {
  val ripLength = restLength * (gamma + 1)

  def springs(particles: ParticleList): Seq[(Int, Int)] = {
    val (pos, vel) = particles.unzip
    val index = SpatialIndex.build(pos, ripLength)
    pos.indices.map {
      ri =>
        index.nbIndices(pos(ri)).filter(nbi => nbi > ri && pos(ri).distanceTo(pos(nbi)) < ripLength).map {
          nbi => (ri, nbi)
        }
    }.flatten
  }

  def impulse(particles: ParticleList) = {
    val sprs = springs(particles).groupBy(s => s._1)
    val (pos, vel) = particles.unzip
    val d = Array.fill(particles.length)((0.0, 0.0))
    particles.indices.foreach {
      i =>
        if (sprs.contains(i)) sprs(i).foreach {
          case (_, j) =>
            val imp = pos(i).diffTo(pos(j)).unit * (pos(i).distanceTo(pos(j)) - restLength) * kspring / sprs(i).length
            d(i) += imp * 0.5
            d(j) -= imp * 0.5
        }
    }
    d.toSeq
  }

}

object SpringElasticity {
  def main(args: Array[String]) {
    import ParticleFlowOperations.advancePositions
    val se = new SpringElasticity(0.5, 2.1, 1.0)
    val particles = Seq(((0.0, 0.0), (0.0, 0.0)),
      ((2.0, 0.0), (0.0, 0.0)),
      ((4.0, 0.0), (0.0, 0.0)),
      ((3.0, 0.0), (0.0, 0.0)))
    val springs = se.springs(particles)
    val imp = se.impulse(particles)
    val pos2 = advancePositions(particles._1, imp, (-1.0, -10.0), (10.0, 10.0), 1.0)
    println("pos1: " + particles._1)
    println("springs: " + springs)
    println("imp: " + imp)
    println("pos2: " + pos2)
  }
}