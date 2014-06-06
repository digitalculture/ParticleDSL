package de.digitalculture.image.warp

import de.digitalculture.particledsl._
import de.digitalculture.math._
import ij.process.ImageProcessor
import ij.process.ByteProcessor

class RevertWarp {

  def warp(img: ImageProcessor, particles: ParticleList, particles0: ParticleList, h: Double) = {
    val nimg = new ByteProcessor(img.getWidth, img.getHeight)
    val (pos, vel) = particles.unzip
    val (pos0, vel0) = particles0.unzip
    val index = SpatialIndex.build(pos, h)
    for (x <- 0 until img.getWidth; y <- 0 until img.getHeight) {
      val nbs = index.nbIndices((x, y)).filter(i => pos(i).distanceTo((x, y)) < h)
      if (!nbs.isEmpty) {
        //        val nearNb = nbs.reduce((i1, i2) =>
        //          if (pos(i1).distanceTo((x, y)) > pos(i2).distanceTo((x, y))) i1 else i2)
        //        val diff = pos(nearNb).diffTo((x, y))
        val colors = nbs.map { i =>
          val diff = pos(i).diffTo((x, y))
          img.getPixelInterpolated(pos0(i)._1 + diff._1, pos0(i)._2 + diff._2)
        }
        val color = colors.sum / colors.length
        nimg.set(x, y, color)
      }
    }
    nimg
  }

}