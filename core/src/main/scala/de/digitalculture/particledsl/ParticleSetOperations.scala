package de.digitalculture.particledsl

import scala.math.{ floor }

import de.digitalculture.math._

class ParticleSetOperations(pl: ParticleList) {

  def :+(p: Particle) = (pl._1 :+ p)

  def :++(pss: ParticleList) = pl._1 ++ pss
}