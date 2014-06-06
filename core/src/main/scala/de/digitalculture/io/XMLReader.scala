package de.digitalculture.io

import scala.xml.XML
import scala.xml.Node
import de.digitalculture.particledsl._
import de.digitalculture.particledsl.graphix.ParticleView
import java.io.File

object XMLReader {

  def readFrame(file: String): (ParticleList, Int, Int, Double) = {
    val xmlNode = XML.loadFile(file)
    val width = (xmlNode \ "@width").text.toDouble.toInt
    val height = (xmlNode \ "@height").text.toDouble.toInt
    val h = (xmlNode \ "@h").text.toDouble
    (
      (xmlNode \\ "particle").map { pNode => unmarshallParticle(pNode) },
      width, height, h)
  }

  private def unmarshallParticle(pNode: Node): Particle = {
    (((pNode \ "position" \ "@x").text.toDouble,
      (pNode \ "position" \ "@y").text.toDouble),
      ((pNode \ "velocity" \ "@x").text.toDouble,
        (pNode \ "velocity" \ "@y").text.toDouble))
  }

  def main(args: Array[String]) {

    val path = "/home/punkx/workspace_scala/SPHDambreak/out/simplerelax/1/"
    val namePattern = "%d.xml"
    def filename(i: Int) = path + namePattern.format(i)
    val (pl, width, height, h) = readFrame(filename(1))
    val view = new ParticleView(width, height, 6)
    view.update(pl)
    view.show
    Stream.from(1).takeWhile(i => new File(filename(i)).exists).map { i => readFrame(filename(i))._1 }.foreach { pl => view.update(pl) }
  }
}