package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def tail[A](l: List[A]): Option[List[A]] = {
    l match {
      case Cons(_, tail) => Some(tail)
      case Nil => None
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, Cons(t, Nil)) => Cons(h, Nil)
      case Cons(h, tail) => Cons(h, init(tail))
      case Nil => Nil
    }
  }

  def foldRight[A, B](as: List[A], x: B)(f: (A, B) => B): B = {
    as match {
      case Nil => x
      case Cons(head, tail) =>
        f(head, foldRight(tail, x)(f))
    }
  }

  def transformL(l: List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => Cons(head + 1, transformL(tail))
    }
  }

  // def transformL(l: List[Int]): List[String] = {
  //   l match {
  //     case Nil => Nil
  //     case Cons(head, tail) => Cons(head.toString, transformL(tail))
  //   }
  // }


  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
      case Nil => Nil
    }
  }


  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    flatten(map(l)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => Cons(head, filter(tail)(f))
      case Cons(_, tail) => filter(tail)(f)
    }
  }

  def foldLeft[A, B](as: List[A], x: B)(f: (A, B) => B): B = {
    as match {
      case Nil => x
      case Cons(head, tail) => foldLeft(tail, f(head, x))(f)
    }
  }

  def append[A](l: List[A], elem: A): List[A] = {
    foldRight(l, Cons(elem, Nil))(Cons(_, _))
  }

  def appendList[A](l: List[A], b: List[A]): List[A] = {
    foldRight(l, b)(Cons(_, _))
  }


  def flatten[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(appendList)
  }

  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def suz(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (Cons(head, tail), Cons(rhead, rtail)) => Cons(head + rhead, suz(tail, rtail))
      case (Nil, Cons(rhead, rtail)) => r
      case (Cons(head, tail), Nil) => l
      case (Nil, Nil) => Nil
    }

  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (Cons(head, tail), Cons(rhead, rtail)) => Cons(f(head, rhead), zipWith(tail, rtail)(f))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)( (_, y) =>  y + 1)
  }

  def sum(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def product(as: List[Int]): Int = {
    foldLeft(as, 1)(_ * _)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])(Cons(_, _))
  }


  def setHead[A](l: List[A], head: A): List[A] = {
    l match {
      case Cons(_, tail) => Cons(head, tail)
      case Nil => Nil
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case Nil => Nil
      case _ => l
      //case x @ Cons(_, _) => x
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Cons(_, tail) if n > 0 => drop(tail, n - 1)
      case Nil => Nil
      case _ => l
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
