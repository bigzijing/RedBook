package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  /**
   * Exercise 4.6
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case right => right
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- b
    } yield f(a, b)
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: tail => f(h).flatMap(head => traverse(tail)(f).map(t => head :: t))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
}

object EitherSimpleTest {

  def main(args: Array[String]): Unit = {
    val right: Either[String, Int] = Right(10)
    val left: Either[String, Int] = Left("error")

    assert(right.map(_ + 90) == Right(100) && left.map(_ + 90) == left)
    assert(right.flatMap(a => Right(a + 90)) == Right(100) && left.flatMap(a => Right(a + 90)) == left)
    assert(right.orElse(Right(1000)) == right && left.orElse(right) == right)
    assert(right.map2(right)(_ + _) == Right(20) && right.map2(left)(_ + _) == left && left.map2(right)(_ + _) == left)
  }
}