package de.digitalculture.particledsl

import de.digitalculture.math._
import scala.math.{ floor }
import scala.collection.TraversableOnce

class SpatialIndex(index: Index) {
  val (maps, h) = index

  private def nbs(b: Box): Seq[Int] =
    (for (i <- b._1 - 1 to b._1 + 1; j <- b._2 - 1 to b._2 + 1)
      yield maps.getOrElse((i, j), Seq.empty[Int])).reduce((l1, l2) => l1 ++ l2)

  def nbs[T](pos: Vector, elements: Seq[T]): Seq[T] = filterIndices(elements, nbs(getBox(pos)))

  def nbIndices(pos: Vector) = nbs(getBox(pos))

  def mapWithNBs[B, T](poss: VectorList, elts: Seq[T], mapFun: (T, T) => B) =
    poss.zip(elts).map {
      case (pos, el) => nbs(pos, elts).map(nbEl => mapFun(el, nbEl))
    }

  def mapWithNBs[B](poss: VectorList, mapFun: (Vector, Vector) => B): Seq[Seq[B]] =
    mapWithNBs(poss, poss, mapFun)

  def mapReduceWithNBs[T, B](poss: VectorList, elts: Seq[T], mapFun: (T, T) => B, reduceFun: (B, B) => B): Seq[B] =
    poss.zip(elts).map {
      case (pos, el) => nbs[T](pos, elts).map(nbEl => mapFun(el, nbEl)).reduce(reduceFun)
    }

  def mapReduceWithNBs[B](poss: VectorList, mapFun: (Vector, Vector) => B, reduceFun: (B, B) => B): Seq[B] =
    mapReduceWithNBs(poss, poss, mapFun, reduceFun)

  private def getBox(p: Vector): Box = SpatialIndex.getBox(p, h)

  private def filterIndices[T](list: Seq[T], indices: Seq[Int]): Seq[T] =
    for (i <- indices) yield list(i)

  //  def mapWithNBs[T, B](listOfTs: Seq[T], fun: ((T, T)) => B): Seq[Seq[B]] =
  //    listOfTs.map { t: T => nbs(t).map { otherT: T => fun(t, otherT) } }

  //  def mapReduceWithNBs[B](listOfTs: Seq[T], mapFun: ((T, T)) => B, reduceFun: (B, B) => B): Seq[B] =
  //    listOfTs.map { t: T => nbs(t).map { otherT: T => mapFun(t, otherT) }.reduce(reduceFun) }

  def :+(posInd: (Vector, Int)): SpatialIndex = {
    val (pos, ind) = posInd
    new SpatialIndex(maps.updated(getBox(pos), maps.getOrElse(getBox(pos), Seq.empty[Int]) :+ ind), h)
  }

  def :++(ts: TraversableOnce[(Vector, Int)]): SpatialIndex =
    ts./:(this)((ind, t) => ind :+ t)

}

object SpatialIndex {
  def build[T](positions: VectorList, h: Double): SpatialIndex =
    new SpatialIndex(positions.indices.groupBy(ind => getBox(positions(ind), h)), h)

  def getBox(p: Vector, h: Double): Box =
    (floor(p._1 / h).toInt, floor(p._2 / h).toInt)
}