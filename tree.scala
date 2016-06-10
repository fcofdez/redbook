package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(a) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}

//                     A
//                    / \
//                   b   c
//                      / \
//                     d   e
//
//

