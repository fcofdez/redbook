package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//Size -> Number nodes
//Maximum -> returns max element in Tree[Int]
//Depth
//Map
//Fold
