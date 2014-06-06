package de.digitalculture.math

class VectorListOps(vl: VectorList) {
  def +(other: VectorList) = vl.zip(other).map { vs => vs._1 + vs._2 }
  def -(other: VectorList) = vl + (-other)
  def *(mult: Double) = vl.map { v => v * mult }
  def /(div: Double) = vl * (1 / div)

  def unary_- = vl.map { v => -v }
  def unary_+ = vl
}