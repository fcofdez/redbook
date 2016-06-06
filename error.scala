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
