package de.digitalculture.image

import ij.ImagePlus
import java.awt.Toolkit
import ij.process.ImageConverter
import ij.process.ByteProcessor
import ij.process.ImageProcessor
import javax.imageio.ImageIO
import java.io.File

object ImageHelper {

  val kernelDX = Seq(0.0, 0, 0, 1, 0, -1, 0, 0, 0).grouped(3).toSeq
  val kernelDY = Seq(0.0, -1, 0, 0, 0, 0, 0, 1, 0).grouped(3).toSeq
  val kernelgauss = Seq.fill(9)(1.0).grouped(3).toSeq

  def getImagePlusFromRessources(title: String, url: String) = {
    val imgurl = getClass().getResource(url)
    new ImagePlus(title, Toolkit.getDefaultToolkit().getImage(imgurl))
  }

  def loadMask(url: String, width: Int, height: Int): Seq[Seq[Byte]] = {
    val img = getImagePlusFromRessources("", url)
    new ImageConverter(img).convertToGray8
    img.getProcessor.resize(width, height).asInstanceOf[ByteProcessor].
      getPixels.asInstanceOf[Array[Byte]].toSeq.grouped(width).toSeq.map { line => line.reverse }.transpose.reverse
  }

  def getImagePlusFromFile(title: String, file: String) = {
    val img = new ImagePlus(title, ImageIO.read(new File(file)))
    new ImageConverter(img).convertToGray8
    img
  }

  //  def div(proc: ByteProcessor): Seq[Seq[(Byte, Byte)]] = {
  //    val width = proc.getWidth
  //    val height = proc.getHeight
  //    val dx = convolute(proc, kernelDX)
  //    val dy = convolute(proc, kernelDY)
  //    dx.getPixels.asInstanceOf[Array[Byte]].toSeq.zip(dy.getPixels.asInstanceOf[Array[Byte]].toSeq).grouped(width).toSeq
  //  }

  def blur(imgData: Seq[Seq[Double]], background: Double) = {
    convolute(imgData, kernelgauss, background)
  }

  def convolute(proc: ByteProcessor, kernel: Array[Int]) = {

    val nproc = cloneProcessor(proc)
    nproc.convolve3x3(kernel)
    val x = new ImagePlus
    x.setProcessor(nproc)
    x.show
    nproc
  }

  def convolute(imgData: Seq[Seq[Double]], kernel: Seq[Seq[Double]], background: Double) = {
    import scala.math.{ floor }
    import de.digitalculture.math.{ weightedMean }
    val shift = floor(kernel.length / 2.0).toInt
    val imgWidth = imgData.length
    val imgHeight = imgData(0).length
    imgData.zipWithIndex.map {
      case (row, r) => row.zipWithIndex.map {
        case (v, c) =>
          val dataVals = kernel.zipWithIndex.map {
            case (kernelRow, kr) => kernelRow.zipWithIndex.map {
              case (kV, kc) =>
                val rPos = r - shift + kr
                val cPos = c - shift + kc
                if (rPos > 0 && cPos > 0 && rPos < imgWidth && cPos < imgHeight) {
                  imgData(rPos)(cPos)
                } else { background }
            }
          }.flatten
          weightedMean(dataVals, kernel.flatten)
      }
    }
  }

  def cloneProcessor(proc: ByteProcessor) = {
    val pixels = proc.getPixelsCopy.asInstanceOf[Array[Byte]]
    new ByteProcessor(proc.getWidth, proc.getHeight, pixels)
  }

  def main(args: Array[String]) {
    val img = Seq(Seq(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), Seq(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), Seq(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), Seq(0.0, 0.1, 0.2, 0.3, 0.4, 0.5))
    val kernel = Seq(Seq(0.0, -1.0, 0.0), Seq(0.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0))
    val convolution = convolute(img, kernel, 0.0)
    convolution.foreach { r => println(r) }
  }

}