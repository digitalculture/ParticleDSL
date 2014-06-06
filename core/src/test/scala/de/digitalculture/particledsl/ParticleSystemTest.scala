package de.digitalculture.particledsl

import org.junit.Assert._
import org.junit.Test

import de.digitalculture.math._
import de.digitalculture.particledsl._
import scala.util.Random

class ParticleSystemTest {

  @Test def move() {
    val r = new Random
    def randomPoint = (r.nextDouble, r.nextDouble)
    def randomVelocity = ((r.nextDouble - 0.5) * 2, (r.nextDouble - 0.5) * 2)

    val minv = (0.0, 0.0)
    val maxv = (2.0, 2.0)
    val dt = 1.0

    val positions: VectorList = Seq.fill(100) { randomPoint }
    val velocities: VectorList = Seq.fill(100) { randomVelocity }

    val pl: ParticleList = (positions, velocities)
    val pl2: ParticleList = pl.move(velocities, minv, maxv, dt)

    (positions, velocities).zip(pl2).foreach {
      ps =>
        val (p1, p2) = ps
        val (pos, vel) = p2
        assertTrue(pos.greaterThan(minv))
        assertTrue(maxv.greaterThan(pos))
        val est = p1._1 + p1._2
        if (est.greaterThan(minv) && maxv.greaterThan(est))
          assertEquals(est, pos)
    }
  }

  @Test def impulse() {
    val r = new Random
    def randomPoint = (r.nextDouble, r.nextDouble)
    def randomVelocity = ((r.nextDouble - 0.5) * 2, (r.nextDouble - 0.5) * 2)

    val minv = (0.0, 0.0)
    val maxv = (2.0, 2.0)
    val dt = 1.0

    val positions: VectorList = Seq.fill(100) { randomPoint }
    val velocities: VectorList = Seq.fill(100) { randomVelocity }
    val impulse: VectorList = Seq.fill(100) { randomVelocity }

    val pl: ParticleList = (positions, velocities)
    val pl2: ParticleList = pl.addImpulse(impulse)
    velocities.zip(pl2._2).zip(impulse).map {
      case ((v1, v2), i) => assertEquals(v2, v1 + i)
    }
  }

}