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


object RNG {
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

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def map2FlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { r1 =>
      map(rb) { r2 =>
        f(r1,r2)
      }
    }

  def mapInFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { r =>
      unit(f(r))
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) {
        unit(mod)
      } else nonNegativeLessThan(n)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsR(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt))

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

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S,List[A]](List[A]())){(s, acc) => s.map2(acc)(_ :: _)}

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap { r => unit(f(r)) }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap {a => sb map { b => f(a, b) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}
