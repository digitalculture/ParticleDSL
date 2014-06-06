package de.digitalculture.particledsl.init

import org.junit.Test
import org.junit.Assert._

import de.digitalculture.math._

class PoissonDiskInitializationTest {

  @Test def pdsTest() {
    val b: Byte = 0
    val width = 80
    val height = 60
    val mask = Seq.fill(width) { Seq.fill(height) { b } }
    val hmin = 1.0
    val hmax = 1.5
    val seeds = Seq.empty[Vector]
    val maxTries = 10

    val positions = PoissonDiskInitialization.fillPDS(mask, hmin, hmax, seeds, maxTries);
    assertTrue(testInvariant(positions, hmin))
  }

  private def testInvariant(positions: Seq[Vector], h: Double) = {
    positions.map { p1 =>
      positions.filterNot(check => check == p1).map { p2 =>
        p1.distanceTo(p2) >= h
      }
    }.flatten.reduce((b1, b2) => b1 && b2)
  }

}