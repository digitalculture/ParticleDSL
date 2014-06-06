package de.digitalculture.math

import scala.math._
import de.digitalculture.math._

class VectorOps(v: Vector) {

  def +(other: Vector): Vector = (v._1 + other._1, v._2 + other._2)

  def *(other: Vector): Vector = (v._1 * other._1, v._2 * other._2)

  def dot(other: Vector): Double = v._1 * other._1 + v._2 * other._2

  def *(scalar: Double): Vector = (v._1 * scalar, v._2 * scalar)

  def /(other: Vector): Vector = (v._1 / other._1, v._2 / other._2)

  def /(scalar: Double): Vector = v * (1 / scalar)

  def -(other: Vector): Vector = (v._1 - other._1, v._2 - other._2)

  def unary_- : Vector = (-v._1, -v._2)

  def unary_+ : Vector = v

  def length: Double = sqrt(v._1 * v._1 + v._2 * v._2)

  def diffTo(other: Vector): Vector = new VectorOps(other) + (-v)

  def distanceTo(other: Vector): Double = abs((v.diffTo(other)).length)

  def unit: Vector = this / this.length

  def equals(other: Vector): Boolean = v._1 == other._1 && v._2 == other._2

  def mirror(dim: Int, pos: Double): Vector = (dim, pos) match {
    case (1, pos) if (v._1 < pos) => (pos + pos - v._1, v._2)
    case (1, pos) if (v._1 > pos) => (pos - (v._1 - pos), v._2)
    case (2, pos) if (v._2 < pos) => (v._1, pos + pos - v._2)
    case (2, pos) if (v._2 > pos) => (v._1, pos - (pos - v._2))
    case _ => v
  }

  def greaterThan(other: Vector): Boolean = v._1 > other._1 && v._2 > other._2
  def smallerThan(other: Vector): Boolean = v._1 < other._1 && v._2 < other._2

  def project(minV: Vector, maxV: Vector, eps: Double): Vector = {
    if (greaterThan(minV) && smallerThan(maxV)) v else
      (max(min(v._1, maxV._1 - eps), minV._1 + eps), max(min(v._2, maxV._2 - eps), minV._2 + eps))
  }

  override def toString = "Vector(" + v._1 + ", " + v._2 + ")"

  //for scala.Numeric
  //  override def compare(x: Vector, y: Vector) = compare(x.length, y.length)
  //  override def fromInt(x: Int) = (x.toDouble, x.toDouble)
  //  override def minus(x: Vector, y: Vector) = new VectorOps(x).-(y)
  //  override def negate(x: Vector) = new VectorOps(x).unary_-
  //  override def plus(x: Vector, y: Vector) = new VectorOps(x).+(y)
  //  override def times(x: Vector, y: Vector) = new VectorOps(x).*(y)
  //  override def toDouble(x: Vector) = x.length
  //  override def toFloat(x: Vector) = x.length.toFloat
  //  override def toInt(x: Vector) = x.length.toInt
  //  override def toLong(x: Vector) = x.length.toLong
}

object VectorOps {
  def apply(x: Double, y: Double) = (x, y)
  def apply(x: (Double, Double)) = (x._1, x._2)

  def main(args: Array[String]) {
    testGT
  }

  def testGT {
    val vmin = (0.1, 0.1)
    val vmax = (10.0, 10.0)
    val v4 = (0.1, 0.0)
    val v5 = (0.0, 0.4)
    val v6 = (1.3, 4.5)
    val v7 = (12.4, 3.4)
    println("Alls false:")
    println(vmin.greaterThan(v4))
    println(vmin.greaterThan(v5))
    println(vmin.greaterThan(v6))
    println(v7.greaterThan(vmax))
    println(v6.greaterThan(vmax))
    println("All true")
    println(vmax.greaterThan(v4))
    println(v6.greaterThan(v4))
    println(vmin.smallerThan(vmax))

    println("All within (0.1,0.1) and (10.0,10.0)")
    println(v7.project(vmin, vmax, 0.01))
    println(v6.project(vmin, vmax, 0.01))
    println(v5.project(vmin, vmax, 0.03))
    println(v4.project(vmin, vmax, 0.03))
  }

}