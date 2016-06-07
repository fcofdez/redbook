package errors

import scala.{Option => _, Either => _, _}

object exercise {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None}

def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  Try(a.foldRight(List[A]())((a: Option[A], l: List[A]) => a.get :: l))
}

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
}

def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = {
  traverse(a)(identity)
}

def simplerSequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case Nil => Some(Nil)
  case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
}

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a flatMap(aa => b.map(bb => f(aa, bb)))
}

def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  case (Some(a), Some(b)) => Some(f(a, b))
  case _ => None
}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def ident[A](x: A) = x

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case None => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
