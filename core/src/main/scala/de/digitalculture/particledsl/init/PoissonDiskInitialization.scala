package de.digitalculture.particledsl.init

import de.digitalculture.math._
import de.digitalculture.particledsl._
import scala.util.Random
import scala.math.{ cos, sin, floor }
import scala.annotation.tailrec
import de.digitalculture.particledsl.graphix.ParticleView

/**
 * Robert Bridson - Fast Poisson Disk Sampling in Arbitrary Dimensions
 * SIGGRAPH 07
 */
object PoissonDiskInitialization {
  val rand = new Random

  def fillPDS(mask: Seq[Seq[Byte]], hmin: Double, hmax: Double, seeds: Seq[Vector], maxTries: Int) = {
    val actives = seeds match {
      case Nil => Seq(generateSeed(mask))
      case list => list
    }
    val samples = seeds match {
      case Nil => actives
      case _ => Seq.empty[Vector]
    }
    val index = SpatialIndex.build((seeds ++ samples), hmin)

    def sample(p: Vector, index: SpatialIndex, positions: VectorList): Option[Vector] = {

      def valid(p: Vector, index: SpatialIndex, positions: VectorList): Boolean = {
        def distance = index.nbs(p, positions) match {
          case Nil => true
          case ds => ds.map(nb => p.distanceTo(nb)).
            reduce((l1, l2) => if (l1 < l2) l1 else l2) > hmin
        }
        def bounded = p._1 > 0 && p._2 > 0 && p._1 < mask.length && p._2 < mask(0).length
        def inMask = mask(floor(p._1).toInt & 0xff)(floor(p._2).toInt & 0xff) == 0

        bounded && distance && inMask
      }

      randomPointsStream(p, hmin, hmax).zipWithIndex.dropWhile { pi =>
        val (p, i) = pi
        i < maxTries && !valid(p, index, positions)
      }.head match {
        case (p, i) if (i < maxTries) => Some(p)
        case _ => None
      }
    }

    @tailrec
    def iter(activeList: Seq[Vector], index: SpatialIndex, samples: Seq[Vector]): Seq[Vector] = {
      if (activeList.isEmpty) samples else {
        val element = activeList(rand.nextInt(activeList.length))
        sample(element, index, seeds ++ samples) match {
          case Some(v) => iter(activeList :+ v, index :+ (v, (seeds ++ samples).length), samples :+ v)
          case None => iter(removeFromList(element, activeList), index, samples)
        }
      }
    }
    iter(actives, index, samples)
  }

  private def randomPointsStream(point: Vector, hmin: Double, hmax: Double): Stream[Vector] = {
    def polarToRect(r: Double, a: Double): Vector = (r * cos(a), r * sin(a))
    def nextPoint(point: Vector, hmin: Double, hmax: Double) =
      point + polarToRect(rand.nextDouble * (hmax - hmin) + hmin, rand.nextDouble * 360)

    Stream.continually(nextPoint(point, hmin, hmax))
  }

  private def removeFromList[T](element: T, list: Seq[T]): Seq[T] = {
    val (head, _ :: tail) = list.span(element != _)
    head ++ tail
  }

  private def generateSeed(mask: Seq[Seq[Byte]]): Vector = {
    val randInd = rand.shuffle(zeroIndices(mask)).head
    (randInd._1.toDouble, randInd._2.toDouble) + (rand.nextDouble, rand.nextDouble)
  }

  private def zeroIndices(mask: Seq[Seq[Byte]]): Seq[(Int, Int)] = {
    for (i <- mask.indices; j <- mask(0).indices; if (mask(i)(j) == 0.toByte)) yield (i, j)
  }
}