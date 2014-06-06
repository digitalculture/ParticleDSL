package de.digitalculture.particledsl.sph

import de.digitalculture.math.Kernel

object NeighborModel {
  def rho0(kernel: Kernel, ips: Double, h: Double) = {
    kernel.kernel2D((0.0, 0.0), (0.0, ips), h) * 6 + kernel.kernel2D((0.0, 0.0), (0.0, 0.0), h)
  }

  def rho0(nbs: Int, kernel: Kernel, ips: Double, h: Double) = {
    import scala.math.{ min, max }
    kernel.kernel2D((0.0, 0.0), (0.0, ips), h) * min(nbs, 6) + kernel.kernel2D((0.0, 0.0), (0.0, 0.0), h)
  }
}