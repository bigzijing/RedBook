package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /**
   * Exercise 3.2
   */
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /**
   * Exercise 3.3
   */
  def setHead[A](ls: List[A], newHead: A): List[A] = ls match {
    case Nil => Nil
    case Cons(_, tail) => Cons(newHead, tail)
  }

  /**
   * Exercise 3.4
   */
  @annotation.tailrec
  def drop[A](ls: List[A], n: Int): List[A] = ls match {
    case Nil => Nil
    case Cons(_, tail) if n >= 1 => drop(tail, n - 1)
    case _ => ls
  }

  /**
   * Exercise 3.5
   */
  @annotation.tailrec
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => ls
  }

  /**
   * Exercise 3.6
   */
  def init[A](ls: List[A]): List[A] = ls match {
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
    case Nil => Nil
  }

  /**
   * Exercise 3.9
   */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, count) => count + 1)

  /**
   * Exercise 3.10
   */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  /**
   * Exercise 3.11
   */
  def sum3(ls: List[Int]): Int =
    foldLeft(ls, 0)(_ + _)

  def product3(ls: List[Double]): Double =
    foldLeft(ls, 1.0)(_ * _)

  def length2[A](ls: List[A]): Int =
    foldLeft(ls, 0)((count, _) => count + 1)

  /**
   * Exercise 3.12
   */
  def reverse[A](ls: List[A]): List[A] =
    foldLeft(ls, Nil: List[A])((ls, elem) => Cons(elem, ls))

  /**
   * Exercise 3.13 (hard)
   */
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  /**
   * Exercise 3.14
   */
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)((a, b) => Cons(a, b))

  /**
   * Exercise 3.15 (hard)
   */
  def concatenateLists[A](ls: List[List[A]]): List[A] =
    foldRight2(ls, Nil: List[A])((a, b) => append2(a, b))

  /**
   * Exercise 3.16
   */
  def addOne(ls: List[Int]): List[Int] =
    foldRight2(ls, Nil: List[Int])((a, b) => Cons(a + 1, b))

  /**
   * Exercise 3.17
   */
  def doubleLsToString(ls: List[Double]): List[String] =
    foldRight2(ls, Nil: List[String])((a, b) => Cons(a.toString, b))

  /**
   * Exercise 3.18
   */
  def map[A, B](ls: List[A])(f: A => B): List[B] =
    foldRight2(ls, Nil: List[B])((a, b) => Cons(f(a), b))


  /**
   * Exercise 3.19
   */
  def filter[A](ls: List[A])(f: A => Boolean): List[A] =
    foldRight2(ls, Nil: List[A]) { (a, b) =>
      if (f(a)) Cons(a, b)
      else b
    }

  /**
   * Exercise 3.20
   */
  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] =
    concatenateLists(map(ls)(f))

  /**
   * Exercise 3.21
   */
  def filter2[A](ls: List[A])(f: A => Boolean): List[A] =
    flatMap(ls) { a =>
      if (f(a)) List(a)
      else Nil
    }

  /**
   * Exercise 3.22
   */
  def mergeAddLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (a, Nil) => a
    case (Nil, b) => b
    case (Cons(a, aTail), Cons(b, bTail)) => Cons(a + b, mergeAddLists(aTail, bTail))
  }

  /**
   * Exercise 3.23
   */
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (a, Nil) => a
    case (Nil, b) => b
    case (Cons(a, aTail), Cons(b, bTail)) => Cons(f(a, b), zipWith(aTail, bTail)(f))
  }

  /**
   * Exercise 3.24 (hard)
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    if (length2(sub) > length2(sup)) false
    else {
      def checkSub(supLs: List[A], subLs: List[A]): Boolean = (supLs, subLs) match {
        case (_, Nil) => true
        case (Cons(a, aTail), Cons(b, bTail)) if a == b => checkSub(aTail, bTail)
        case _ => false
      }

      def iterateSup(ls: List[A]): Boolean = ls match {
        case Nil => false
        case Cons(_, tail) =>
          if (checkSub(ls, sub)) true
          else iterateSup(tail)
      }

      iterateSup(sup)
    }
}
