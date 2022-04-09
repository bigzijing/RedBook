object Option {
  sealed trait Option[+A] {
    // 4.1
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case _ => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case _ => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
      if (map(f).getOrElse(false)) this
      else None

  }

  object Option {

    def lift[A, B](f: A => B): Option[A] => Option[B] =
      (opt: Option[A]) => opt.map(f)

    // 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap { aVal =>
        b.map { bVal =>
          f(aVal, bVal)
        }
      }

    // 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case headOpt :: tail => headOpt.flatMap(head => sequence(tail).map(tail => head :: tail))
      case Nil => Some(Nil)
    }

    // 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case head :: tail => f(head).flatMap(h => traverse(tail)(f).map (t => h :: t))
      case Nil => Some(Nil)
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(a => a)
  }

  case class Some[A](a: A) extends Option[A]
  case object None extends Option[Nothing]

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(seq: Seq[Double]): Option[Double] =
      if (seq.isEmpty) None
      else Some(seq.sum / seq.length)

    mean(xs).flatMap { m =>
      mean(xs.map(v => math.pow(v - m, 2)))
    }
  }

}

object Either {

  sealed trait Either[+E, +A] {
    // 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      map(f) match {
        case Left(e) => Left(e)
        case Right(b) => b
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case right @ _ => right
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap { aa =>
        b.map { bb =>
          f(aa, bb)
        }
      }
  }

  sealed case class Left[E](error: E) extends Either[E, Nothing]
  sealed case class Right[A](a: A) extends Either[Nothing, A]

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case Nil => Right(Nil)
      case Right(a) :: tail => sequence(tail).map(bb => a :: bb)
      case Left(e) :: _ => Left(e)
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      sequence(as.map(f))

    def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case head :: tail => f(head).flatMap { a =>
        traverse2(tail)(f).map { b =>
          a :: b
        }
      }
    }

    def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse2(es)(either => either)
  }
}

import Option._
import Option.Option.{ lift, map2 }
import Either._
import Either.Either.sequence

val absO: Option[Double] => Option[Double] = lift(math.abs)

def Try[A](a: => A): Option[A] = {
  try Some(a)
  catch { case e: Exception => None }
}

def insuranceRateQuote(a: Int, b: Int): Double = ???

def parseInsuranceRateQuote(
                           age: String,
                           numberOfSpeedingTickets: String
                           ): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

  map2(optAge, optTickets)(insuranceRateQuote)
}

def parseInsuranceRateQuoteEither(
                                 age: String,
                                 numberOfSpeedingTickets: String
                                 ): Either[Exception, Double] = {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  for {
    a <- Try { age.toInt }
    b <- Try { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuote(a, b)
}
