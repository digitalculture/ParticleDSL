package de.digitalculture.particledsl

import scala.util.Random
import scala.math.{ min, max }

import de.digitalculture.math._

class ParticleFlowOperations(pl: ParticleList) {

  private val r: Random = new Random

  def move(displacement: VectorList,
    minv: Vector, maxv: Vector,
    multiplier: Double): ParticleList = {
    import ParticleFlowOperations.advancePositions

    (advancePositions(pl._1, displacement, minv, maxv, multiplier), pl._2)
  }

  def addImpulse(impulse: VectorList): ParticleList =
    (pl._1,
      pl._2.zip(impulse).map(v => v._1 + v._2))

  def estimatePositions(minv: Vector, maxv: Vector, dt: Double) =
    pl.move(pl._2, minv, maxv, dt)

  def estimateVelocities(old: ParticleList, dt: Double): ParticleList =
    (pl._1,
      pl._1.zip(old._1).map {
        pos => (pos._1 - pos._2) / dt
      })

}

object ParticleFlowOperations {
  private val r: Random = new Random

  def advancePositions(pos: VectorList, vel: VectorList, minv: Vector, maxv: Vector, mult: Double) = {
    pos.zip(vel).map { p =>
      val (po, ve) = p
      (po + ve * mult).project(minv, maxv, r.nextDouble / 100.0)
    }
  }

  def advanceParticle(pos: Vector, vel: Vector, minv: Vector, maxv: Vector, mult: Double) = {
    (pos + vel * mult).project(minv, maxv, r.nextDouble / 100.0)
  }
}