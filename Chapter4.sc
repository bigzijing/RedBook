object Option {
  sealed trait Option[+A] {
    /**
     * Exercise 4.1
     */
    def map[B](f: A => B): Option[B] = this match {
      case Some(get) => Some(f(get))
      case None => None
    }
    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(get) => get
      case None => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(ob)
    def filter(f: A => Boolean): Option[A] =
      if (map(f).getOrElse(false)) this
      else None
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
   * Exercise 4.2
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  /**
   * Exercise 4.3
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  def insuranceRateQuote(a: Int, b: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  /**
   * Exercise 4.4
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case headOpt :: tail => headOpt.flatMap(head => sequence(tail).map(tail => head :: tail))
  }

  /**
   * Exercise 4.5
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => f(head).flatMap { value =>
      traverse(tail)(f).map { fTail =>
        value :: fTail
      }
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(a => a)

  def map2_book[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { aa =>
      b.map { bb =>
        f(aa, bb)
      }
    }
}

object Either {
  sealed trait Either[+E, +A] {
    /**
     * Exercise 4.6
     */
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(v) => Right(f(v))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this.map(f) match {
        case Left(e) => Left(e)
        case Right(v) => v
      }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap { aa =>
        b.map { bb =>
          f(aa, bb)
        }
      }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case head :: tail => f(head).flatMap { value =>
      traverse(tail)(f).map { fTail =>
        value :: fTail
      }
    }
  }
}

import Option._
println(sequence[Int](List(Some(1), Some(2), Some(3))))
println(sequence[Int](List(Some(1), Some(2), None, Some(3))))
println(sequence2[Int](List(Some(1), Some(2), Some(3))))
println(sequence2[Int](List(Some(1), Some(2), None, Some(3))))
println(traverse[Int, Int](a = List(1,2,3,4,5))(f = (x: Int) => if (x % 3 == 0) Some(x) else None))
println(traverse[Int, Int](a = List(3,6,9,12,15))(f = (x: Int) => if (x % 3 == 0) Some(x) else None))