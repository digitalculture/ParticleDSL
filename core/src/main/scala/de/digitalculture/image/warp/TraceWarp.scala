package de.digitalculture.image.warp

import de.digitalculture.particledsl._
import de.digitalculture.math._
import ij.process.ImageProcessor
import ij.process.ByteProcessor
import java.util.Arrays

class TraceWarp {

  val steps = 5
  val bgcol = 255.toByte

  def warp(img: ImageProcessor, particles: Seq[ParticleList], indices: Seq[SpatialIndex], dt: Double, h: Double, kernel: Kernel, min: Vector, max: Vector) = {
    img.setInterpolationMethod(ImageProcessor.BICUBIC)
    val nimg = new ByteProcessor(img.getWidth, img.getHeight, Array.fill(img.getWidth * img.getHeight)(bgcol))
    nimg.setInterpolationMethod(ImageProcessor.BILINEAR)
    val imgGrid: Seq[(Int, Int)] = (for (x <- 0 until img.getWidth; y <- 0 until img.getHeight) yield (x, y)) //.filter(pos => indices.last.nbIndices(pos._1, pos._2).filter(
    //  nb => particles.last(nb)._1.distanceTo(pos._1, pos._2) < h).length > 0)
    val imgGridCorrected = imgGrid.map { _ + (0.0, 0.0) }
    val pos0 = particles.zip(indices).reverse.foldLeft(imgGridCorrected)((imgGrid, plind) => {
      val (particles, index) = plind
      val vel = imgGrid.map {
        gridPoint => interpolateVelocity(gridPoint._1, gridPoint._2, particles, index, h, kernel)
      }
      System.out.print(".")
      traceBack(vel, imgGrid, min, max, dt)
    })
    println(" ")
    imgGrid.zip(pos0).foreach {
      case (gridPoint, gridPoint0) =>
        nimg.set(gridPoint._1, gridPoint._2,
          img.getPixelInterpolated(gridPoint0._1, gridPoint0._2))
    }
    println(" ")
    nimg
  }

  def traceBack(vel: VectorList, pos: VectorList, min: (Double, Double), max: (Double, Double), dt: Double) = {
    import de.digitalculture.particledsl.ParticleFlowOperations.advancePositions
    advancePositions(pos, vel, min, max, -dt)
  }

  def interpolateVelocity(x: Double, y: Double, particles: ParticleList, index: SpatialIndex, h: Double, kernel: Kernel) = {
    import de.digitalculture.math.weightedMean
    val (pos, vel) = particles.unzip
    val realNbs = index.nbIndices((x, y)).filter(i => pos(i).distanceTo((x, y)) < h)
    if (realNbs.length > 0) {
      val nbsVels = realNbs.map { i => vel(i) }
      val weights = realNbs.map { i => kernel.kernel2D((x, y), pos(i), h) }
      weightedMean(nbsVels, weights)
    } else { (0.0, 0.0) }
  }

  def nuwarp(img: ImageProcessor, particles: Seq[ParticleList], dt: Double, h: Double, kernel: Kernel, min: Vector, max: Vector) = {
    val indices = particles.map { pl => SpatialIndex.build(pl._1, h) }
    img.setInterpolationMethod(ImageProcessor.BICUBIC)
    val nimg = new ByteProcessor(img.getWidth, img.getHeight)
    nimg.setInterpolationMethod(ImageProcessor.BILINEAR)
    val imgGrid: Seq[(Int, Int)] = for (x <- 0 until img.getWidth; y <- 0 until img.getHeight) yield (x, y)
    val imgGridCorrected = imgGrid.map { _ + (0.0, 0.0) }
    val colors = imgGridCorrected.par.map {
      case (i, j) =>
        if (indices.last.nbIndices((i, j)).length > 0) {
          val (x, y) = traceBack((i, j), particles, indices, h, kernel, min, max, dt)
          img.getPixelInterpolated(x, y)
        } else { bgcol }
    }
    imgGrid.zip(colors).map {
      case ((i, j), c) => nimg.set(i.toInt, j.toInt, c)
    }
    nimg
  }

  def traceBack(pos: Vector, particles: Seq[ParticleList], indices: Seq[SpatialIndex], h: Double, kernel: Kernel, min: (Double, Double), max: (Double, Double), dt: Double) = {
    import de.digitalculture.particledsl.ParticleFlowOperations.advanceParticle
    particles.zip(indices).foldRight(pos) {
      case ((plist, index), p) =>
        (1 to steps).foldLeft(pos) { (p, i) =>
          val vel = interpolateVelocity(p._1, p._2, plist, index, h, kernel)
          advanceParticle(p, vel, min, max, dt / steps)
        }
    }
  }
}