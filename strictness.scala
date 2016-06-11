package strictness

import scala.annotation.tailrec
import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => t()
    case Cons(h, t) => t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(_, _) => Empty
  }


  def toList: List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, =>B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOptionFR: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def mapFR[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => cons(f(h()), t().map(f))
    case Empty => Empty
  }

  def takeWhileFR(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if(p(a)) cons(a, b) else b)
  }

  def toListTR: List[A] = {
    @tailrec
    def go(l: List[A], s: Stream[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => go(h() :: l, t())
    }
    go(List[A](), this)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n +1))
  }

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(a => cons(a._1, unfold(a._2)(f))).getOrElse(empty)
  }

  def constantUnfold[A](a: A): Stream[A] = {
    unfold(a)(s => Some(s, a))
  }

  def fromUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s+1))
  }

  def fibUnfold: Stream[Int] = {
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
