package de.digitalculture.particledsl

import org.junit.Test
import org.junit.Assert._
import scala.util.Random
import scala.math.{ sqrt }

import de.digitalculture.math._

class SpatialIndexTest {

  val r: Random = new Random

  def randomPoint = (r.nextDouble, r.nextDouble)

  @Test def nbs() {
    val h = 0.1
    val somePoints = Seq.fill(1000) { randomPoint }
    val index = SpatialIndex.build(somePoints, h)
    for (aPoint <- somePoints) {
      val nbs = index.nbs(aPoint, somePoints)
      val realNbs = somePoints.filter(p => aPoint.distanceTo(p) < h)
      //assume all real nbs are contained in nbs
      for (nb <- realNbs) {
        assume(nbs.contains(nb), "%s should be a neighbor of %s!".format(nb, aPoint))
      }

      //assume the distance of indexed Nbs to be smaller than 2 * sqrt(2 * h * h)
      for (cnb <- nbs) {
        assume(cnb.distanceTo(aPoint) < 2 * sqrt(2 * h * h),
          "%s was retrieved as neighbor of %s, but the distance is bigger than %f (%f)".
            format(cnb, aPoint, 2 * sqrt(2 * h * h), cnb.distanceTo(aPoint)))
      }
    }
  }
}