package fpinscala.parsing

import fpinscala.testing.Prop.forAll
import fpinscala.testing.{Gen, Prop}

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  /**
   * Exercise 9.4 (hard)
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  /**
   * Exercise 9.3 (hard)
   */
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  /**
   * Exercise 9.7
   */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  def map2FlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => flatMap(p2)(b => f(a, b)))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f.andThen(succeed))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
   * Exercise 9.1
   */
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many[A](p: Parser[A]): Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    /**
     * Exercise 9.2 (hard)
     * `product` should be associative (standard mathematical law)
     * However, with how `product` is implemented, the difference is that the results is nested slightly differently, i.e.
     *
     * (a ** b) ** c will be [((A, B), C)] and
     * a ** (b ** c) will be [(A, (B, C))]
     *
     * These are technically equal, but pedantically not; perhaps a better solution is to include an unbias method on **, as such:
     *
     * def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
     * def unbiasR[A, B, C](p: (A, (B, C)): (A, B, C) = (p._1, p._2._1, p._2._2)
     *
     * This proves the associativity of product:
     * ((a ** b) ** c).map(unbiasL) == (a ** (b ** c)).map(unbiasR)
     */
  }
}


case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next()
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
