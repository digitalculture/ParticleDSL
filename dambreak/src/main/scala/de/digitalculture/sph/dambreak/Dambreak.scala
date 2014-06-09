package de.digitalculture.sph.dambreak

import de.digitalculture.math._
import de.digitalculture.particledsl._
import de.digitalculture.particledsl.graphix.ParticleView
import de.digitalculture.particledsl.sph.density.DensityEstimation
import de.digitalculture.particledsl.init.PoissonDiskInitialization
import de.digitalculture.particledsl.sph.pressure.PressureImpulse
import de.digitalculture.particledsl.sph.pressure.PressureEstimation
import de.digitalculture.image.ImageHelper
import de.digitalculture.particledsl.sph.pressure.PressureGradient
import de.digitalculture.particledsl.sph.NeighborModel
import de.digitalculture.particledsl.sph.relaxation.PressureRelaxation
import de.digitalculture.io.XMLWriter
import de.digitalculture.particledsl.sph.relaxation.SimpleRelaxation
import de.digitalculture.particledsl.init.CommonInits
import de.digitalculture.particledsl.sph.viscosity.DiffusionViscosity

object Dambreak {
  val width: Int = 50
  val height: Int = 50
  val minv = (0.0, 0.0)
  val maxv = (width.toDouble, height.toDouble)

  val dt = 0.05
  val h = 1.4
  val ips = 0.4 * h
  val dens = new DensityEstimation(minXSqKernel, h)
  val rho0ip = NeighborModel.rho0(minXSqKernel, ips, h)
  val k = 0.5
  val presest = new PressureEstimation(rho0ip, k)
  val pgrad = new PressureGradient(minXSqKernel, h, presest)
  val pimpulse = new PressureImpulse(pgrad)
  val maxit = 100
  val rhotol = 1.4 * rho0ip
  val krelax = 0.8
  val simpRelax = new SimpleRelaxation(dens, minv, maxv, dt, krelax, rhotol, maxit)
  val visc = new DiffusionViscosity(GaussKernel, 0.8)

  val imgpath = "/img/watercolumn.jpg"
  var particles = CommonInits.withImage(imgpath, width, height, h, ips, ips + 0.1)
  val pv = createView
  val pv2 = createView
  val outpath = "/home/punkx/workspace_scala/SPHDambreak/out/simplerelax/1/"

  def createView = {
    new ParticleView(width, height, 10)
  }

  def gravity(n: Int) = Seq.fill(n) { (0.0, 9.8) }

  def forwardEuler(particles: ParticleList, acc1: VectorList, dt: Double): ParticleList = {
    import ParticleFlowOperations.advancePositions
    val particlesForce = particles.addImpulse(acc1)
    val index = SpatialIndex.build(particlesForce._1, h)
    val densities = dens.estimateDensities(particlesForce._1, index)
    val pressImpulse = pimpulse.pressureImpulse(densities, particlesForce._1, index)
    val particlesPress = particlesForce.addImpulse(pressImpulse)
    val viscImpulse = visc.viscousImpulse(particlesPress, h)
    val particlesVisc = particlesForce.addImpulse(viscImpulse)
    val nullimpulse = Seq.fill(particles.length) { (0.0, 0.0) }
    val (r1: VectorList, v1: VectorList) = particlesVisc.unzip
    val r2star: VectorList = advancePositions(r1, v1, minv, maxv, dt)
    val relax2: VectorList = simpRelax.relaxImpulse((r2star, nullimpulse))
    val r2: VectorList = advancePositions(r2star, relax2, minv, maxv, 1.0)
    val v2: VectorList = (r2 - r1)/dt
    (r2, v2)
  }

  def heunsMethod(particles: ParticleList): ParticleList = {
    import ParticleFlowOperations.advancePositions
    val nullimpulse = Seq.fill(particles.length) { (0.0, 0.0) }
    val particles2 = forwardEuler(particles, gravity(particles.length), dt)
    val particles3 = forwardEuler(particles2, gravity(particles.length), dt)
    val r2star = (particles2._1 + particles3._1) / 2.0
    val relax2 = simpRelax.relaxImpulse((r2star, nullimpulse))
    val r2 = advancePositions(r2star, relax2, minv, maxv, 1.0)
    val v2 = r2 - particles._1
    (r2, v2)
  }

  def main(args: Array[String]) {
    pv.update(particles)
    pv.show

    (1 to 1000).foreach {
      i =>
        particles = forwardEuler(particles, gravity(particles.length), dt)
        //        particles = heunsMethod(particles)
        pv.update(particles)
    }
  }

}
