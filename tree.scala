package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//Size -> Number nodes
//Maximum -> returns max element in Tree[Int]
//Depth
//Map
//Fold

def size[A](tree: Tree[A]): Int = tree match {
  case Leaf(_) => 1
  case Branch(l, r) => size(l) + size(r) + 1
}

def max[Int](tree: Tree[Int]): Int = tree match {
  case Leaf(a) => a
  case Branch(l, r) => max(l) max max(r)
}

def depth[A](tree: Tree[A]): Int = tree match {
  case Leaf(a) => 1
  case Branch(l, r) => (depth(l) max depth(r)) + 1
}

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
  case Leaf(a) => Leaf(f(a))
  case Branch(l, r) => Branch(map(l)(f), map(r)(f))
}
