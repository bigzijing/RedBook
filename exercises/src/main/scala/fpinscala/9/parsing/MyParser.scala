package fpinscala.parsing

import fpinscala.parsing.Reference.{Failure, MyParser, Success}

import scala.util.matching.Regex

object Reference {
  type MyParser[+A] = Location => Result[A]

  sealed trait Result[+A] {

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, b) => Failure(f(e), b)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}

object MyParsers extends Parsers[MyParser] {
  /**
   * Exercise 9.15
   */
  override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] =
    p(Location(input, 0)) match {
      case Success(get, _) => Right(get)
      case Failure(get, _) => Left(get)
    }

  /**
   * Exercise 9.13
   */
  override def succeed[A](a: A): MyParser[A] =
    _ =>
      Success(a, 0)

  /**
   * Exercise 9.14
   */
  override implicit def string(s: String): MyParser[String] =
    (location: Location) =>
      if (location.input.startsWith(s))
        Success(s, s.length)
      else
        Failure(location.toError("Expected: " + s).label("Some error"), false)

  override implicit def regex(r: Regex): MyParser[String] =
    (location: Location) =>
      r.findPrefixOf(location.input) match {
        case Some(m) => Success(m, m.length)
        case _ => Failure(location.toError("Regex " + r), false)
      }

  override def slice[A](p: MyParser[A]): MyParser[String] =
    location => p(location) match {
      case Success(_, n) => Success(location.input.substring(location.offset, location.offset + n), n)
      case f@Failure(_, _) => f
    }

  override def or[A](p1: MyParser[A], p2: MyParser[A]): MyParser[A] =
    location => p1(location) match {
      case Failure(e, false) => p2(location)
      case r => r
    }

  override def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] =
    location => p(location) match {
      case Success(a, n) => f(a)(location.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)

      case e@Failure(_, _) => e
    }

  override def label[A](msg: String)(p: MyParser[A]): MyParser[A] =
    location => p(location).mapError(_.label(msg))

  override def scope[A](msg: String)(p: MyParser[A]): MyParser[A] =
    location => p(location).mapError(_.push(location, msg))

  override def attempt[A](p: MyParser[A]): MyParser[A] =
    location => p(location).uncommit

}
