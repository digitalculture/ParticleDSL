//package de.digitalculture.particledsl
//
//import de.digitalculture.math._
//
//class ParticleSystem(val particles: ParticleList = Seq.empty[Particle]) {
//
//  override def toString: String = "[" +
//    particles.map { p => "(" + p._1.toString + "," + p._2.toString + ")" }.reduceLeft(_ + "," + _) + "]"
//
//  def positions = particles._1
//  def velocities = particles._2
//}
//
//object ParticleSystem {
//  //  implicit def psToSetOp(ps: ParticleSystem) = new ParticleSetOperations(ps)
//  //  implicit def psToFlowOp(ps: ParticleSystem) = new ParticleFlowOperations(ps)
//
//  def main(args: Array[String]) {
//    val ps = new ParticleSystem
//    println(ps :+ (((1.0, 1.0), (0.0, 0.0))) :+ (((2.0, 1.0), (0.0, 0.0))) :+ (((2.0, 2.0), (0.0, 0.0))))
//
//    val poss = (1 to 10).map { i => (i.asInstanceOf[Double], i.asInstanceOf[Double] + 1) }
//    val vels = (1 to 10).map { _ => (0.0, 0.0) }
//
//    val ps2 = new ParticleSystem((poss, vels))
//    println(ps2)
//
//    val smooth = 2.0
//    val ind = SpatialIndex.build(ps2.particles, smooth, (p: Particle) => p._1)
//    println(ind)
//
//    val sumofdifferences = ind.mapReduceWithNBs(ps2.particles,
//      ps => ps._1._1.distanceTo(ps._2._1),
//      (d1: Double, d2: Double) => d1 + d2)
//    println(sumofdifferences)
//
//    //    val (sumofdifferences2, ind2) = ps2.mapWithNBs(smooth,
//    //      (in) => (in._2.map { _.length }).reduceLeft(_ + _))
//    //
//    //    println(ind2)
//    //    println(sumofdifferences2.sorted)
//  }
//}