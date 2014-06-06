package de.digitalculture

import scala.collection.TraversableOnce
package object math {

  type Vector = (Double, Double)
  type VectorList = Seq[Vector]

  implicit def double2Vector(x: Double): Vector = (x, x)

  implicit def int2Vector(x: Int): Vector = (x, x)

  implicit def dtuple2VectorOps(x: (Double, Double)): VectorOps = new VectorOps(x)

  implicit def ituple2VectorOps(x: (Int, Int)): VectorOps = new VectorOps((x._1.toDouble, x._2.toDouble))

  implicit def vector2D2Tuple(v: Vector): Tuple2[Double, Double] = (v._1, v._2)

  implicit def vectorList2Ops(vl: VectorList): VectorListOps = new VectorListOps(vl)

  def sigma(ind: TraversableOnce[Int], fun: Int => Double): Double = {
    ind.map { i => fun(i) }.sum
  }

  def sigma(ind: TraversableOnce[Int], fun: Int => Vector): Vector = {
    ind.map { i => fun(i) }.reduce { _ + _ }
  }

  def weightedMean(data: Seq[Double], weights: Seq[Double]) = {
    val sum = if (weights.sum != 0.0) { weights.sum } else 1.0
    data.zip(weights).map {
      case (d, w) => d * w
    }.sum / sum
  }

  def weightedMean(data: Seq[Vector], weights: Seq[Double]) = {
    data.zip(weights).map {
      case (d, w) => d * w
    }.foldLeft((0.0, 0.0)) { (v1, v2) => v1 + v2 } / weights.sum
  }
}