package de.digitalculture.math

import scala.math.Pi
import scala.math.exp
import scala.math.sqrt

import de.digitalculture.math._

import sun.reflect.generics.reflectiveObjects.NotImplementedException

trait Kernel {
  def kappa: Double
  def kernel2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Double
  def nabla2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Vector
  def laplace2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Double
  def toString: String
}

object GaussKernel extends Kernel {
  def kappa = 3.0
  def kernel2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double) = {
    val a = 1 / (Pi * h * h)
    val r = ri.diffTo(rj).length
    val rh = r / h
    if (rh / kappa < 1) {
      a * exp(-(rh * rh))
    } else {
      0.0
    }
  }
  def nabla2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Vector = {
    val dx = rj._1 - ri._1
    val dy = rj._2 - ri._2
    val d = sqrt(dx * dx + dy * dy)
    val a2 = 1 / (Pi * h * h)
    d / (h * kappa) match {
      case v if v < 1 =>
        (-(2 * dx) * exp(-(dx * dx / h * h)) / (h * sqrt(Pi)) * exp(-(dy * dy / h * h)) / (h * sqrt(Pi)),
          -(2 * dy) * exp(-(dx * dx / h * h)) / (h * sqrt(Pi)) * exp(-(dy * dy / h * h)) / (h * sqrt(Pi)))
      case _ => (0.0, 0.0)
    }
  }
  def laplace2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double) = {
    val d = ri.distanceTo(rj)
    val a2 = 1 / (Pi * h * h)
    d / (h * kappa) match {
      case v if v < 1 => a2 * (4 * d * d - 2 * h * h) / (h * h * h * h) * exp(-(d / h * h))
      case _ => 0.0
    }
  }
  override def toString = "GaussKernel"
}

object minXSqKernel extends Kernel {
  def kappa: Double = 2.0
  def kernel2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Double = {
    val r = ri.distanceTo(rj)
    val rh = r / h
    val kh = kappa * h
    val ci = (1 / 3.0) / (h * h)
    if (rh / kappa < 1) {
      (ci / 2) * (rh - 2) * (rh - 2)
    } else {
      0.0
    }
  }
  def nabla2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Vector = {
    val r = ri.distanceTo(rj)
    val rh = r / h
    val kh = kappa * h
    val ci = (1 / 3.0) / (h * h)
    if (r / kh < 1 && r > 0) {
      val rij = rj.diffTo(ri).unit
      rij * ci * (r / (h * r) * (rh - 2))
    } else {
      (0.0, 0.0)
    }
  }
  def laplace2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Double = {
    throw new NotImplementedException
  }
  override def toString: String = "minXSqKernel"
}

object linearKernel extends Kernel {
  def kappa: Double = 2.0
  def kernel2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Double = {
    val r = ri.diffTo(rj).length
    val rh = r / h
    if (rh <= 1) {
      1 - rh
    } else {
      0.0
    }
  }
  def nabla2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Vector = {
    throw new NotImplementedException
  }
  def laplace2D(ri: Tuple2[Double, Double], rj: Tuple2[Double, Double], h: Double): Double = {
    throw new NotImplementedException
  }
  override def toString: String = "linearKernel"
} //else {
