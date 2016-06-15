package state

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object State {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleMap: Rand[Double] =
    map(nonNegativeInt) { n =>
      n.toDouble / Int.MaxValue
    }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)

    ((n, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r2) = intDouble(rng)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def intsTail(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, l: ArrayBuffer[Int])(rng: RNG): (List[Int], RNG) = count match {
      case 0 =>
        (l.toList, rng)
      case n =>
        val (i, r1) = rng.nextInt
        go(count - 1, l += i)(r1)
    }
    go(count, ArrayBuffer[Int]())(rng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 =>
      (List[Int](), rng)
    case n =>
      val (i, r1) = rng.nextInt
      val (l, r2) = ints(count - 1)(r1)
      (i :: l, r2)
  }

}

