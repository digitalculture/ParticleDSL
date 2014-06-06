package de.digitalculture

import de.digitalculture.math._

package object particledsl {

  type Particle = (Vector, Vector)
  type ParticleList = Seq[Particle]

  type Box = (Int, Int)
  type Index = (Map[Box, Seq[Int]], Double)

  implicit def VecLi2ParLi(vecs: (VectorList, VectorList)): ParticleList = vecs._1.zip(vecs._2)

  implicit def ParLi2VecLi(pl: ParticleList): (VectorList, VectorList) = pl.unzip

  implicit def ParLi2FlowOps(pl: ParticleList) = new ParticleFlowOperations(pl)
}