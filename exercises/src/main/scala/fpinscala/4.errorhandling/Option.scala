package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  /**
   * Exercise 4.1
   */
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    if (map(f).getOrElse(false)) this
    else None
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
   * Exercise 4.2
   */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  /**
   * Exercise 4.3
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

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

object OptionSimpleTest {
  import Option._

  def main(args: Array[String]): Unit = {
    val some = Some(10)
    val none: Option[Int] = None

    assert(some.map(_ + 10) == Some(20) && none.map(_ + 10) == None)
    assert(some.getOrElse(100) == 10 && none.getOrElse(100) == 100)
    assert(some.flatMap(_ => Some(5)) == Some(5) && none.flatMap(_ => Some(5)) == None)
    assert(some.orElse(Some(10)) == Some(10) && none.orElse(Some(50)) == Some(50))
    assert(some.filter(_ > 100) == None && none.filter(_ => true) == None)
    assert(map2[Int, Int, Int](some, some)(_ + _) == Some(20))
    assert(sequence(List(some, none)) == None && sequence(List(some, some, some)) == Some(List(10, 10, 10)))
    assert(sequence2(List(some, none)) == None && sequence2(List(some, some, some)) == Some(List(10, 10, 10)))
    assert(traverse(List(10, 10, 10))(a => if (a == 10) Some(10) else None) == Some(List(10, 10, 10)) && traverse(List(10, 5))(a => if (a == 10) Some(10) else None) == None)
  }
}