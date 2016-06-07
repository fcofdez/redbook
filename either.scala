package errors
trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(_) => this
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, b](f: A => Either[EE, B]): Either[EE, B] = {
    map(f) getOrElse None
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {

  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]
