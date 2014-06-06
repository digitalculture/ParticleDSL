package de.digitalculture.io

import scala.xml.XML
import de.digitalculture.math._
import de.digitalculture.particledsl._
import de.digitalculture.particledsl.sph.NeighborModel
import de.digitalculture.particledsl.sph.density.DensityEstimation

object XMLWriter {
  def write(file: String, particles: ParticleList, kernel: Kernel, width: Double, height: Double, h: Double) {
    val ips = h
    val rho0 = NeighborModel.rho0(kernel, ips, h)
    val (positions: VectorList, velocities: VectorList) = particles.unzip
    val index = SpatialIndex.build(positions, h)
    val densities = new DensityEstimation(kernel, h).estimateDensities(positions, index)
    val pxml =
      <frame width={ width.toString } height={ height.toString } h={ h.toString } rho0={ rho0.toString }>
        {
          for (
            data <- particles.indices.map {
              i =>
                (i, positions(i), velocities(i), densities(i),
                  if (false) { "border" } else { "liquid" })
            }
          ) yield <particle type={ data._5 }>
                    <position x={ data._2._1.toString } y={ data._2._2.toString }/>
                    <velocity x={ data._3._1.toString } y={ data._3._2.toString }/>
                    <density>{ data._4 }</density>
                  </particle>
        }
      </frame>
    XML.save(file, pxml, "UTF8", true, null)
  }
}