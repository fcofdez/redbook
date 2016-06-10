package errors

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h) flatMap(hh => traverse(t)(f) map (hh :: _))
  }
}
