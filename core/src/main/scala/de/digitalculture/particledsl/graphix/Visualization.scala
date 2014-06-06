package de.digitalculture.particledsl.graphix

import ij.gui.NewImage
import ij.io.FileSaver
import ij.process.ByteProcessor
import ij.process.ImageConverter

import java.awt.Dimension

class Visualization(dim: Dimension, title: String) {
  protected val img = NewImage.createByteImage(title, dim.width + 1, dim.height + 1, 1, NewImage.FILL_BLACK)
  new ImageConverter(img).convertToGray8
  protected val proc: ByteProcessor = img.getProcessor.asInstanceOf[ByteProcessor]

  println("Visualization initialized! (%d, %d) - %d bytes".format(proc.getWidth, proc.getHeight, proc.getPixelCount))

  val saver: FileSaver = new FileSaver(img)

  protected def updatePixels(pixels: Array[Byte]) {
    //Typechecking?? Check bounds? Maybe just try/catch?
    proc.setPixels(pixels)
    img.updateAndDraw
  }

  def show = {
    img.show
  }

  def save(path: String) {
    saver.saveAsJpeg(path)
  }

  def ix(x: Int, y: Int) = x + y * proc.getWidth
}