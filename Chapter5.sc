import Stream._
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /**
   * Exercise 5.1
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
   * Exercise 5.2
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * Exercise 5.3
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) =>
      cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
   * Exercise 5.4
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
   * Exercise 5.5
   */
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) =>
      if (p(a)) cons(a, b)
      else empty)

  /**
   * Exercise 5.6 (hard)
   */
  def headOption2: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  /**
   * Exercise 5.7
   */
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) =>
    if (p(a)) cons(a, b)
    else b.filter(p))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((a, b) => f(a).append(b))

  /**
   * Exercise 5.13
   */
  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h()), (t(), n - 1))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
      case (_, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
      case _ => None
    }

  /**
   * Exercise 5.14 (hard)
   */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll {
      case (thisHead, sHead) => thisHead == sHead
    }

  /**
   * Exercise 5.15
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(_, t) => Some(s, t())
      case _ => None
    }.append(Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))

  /**
   * Exercise 5.16 (hard)
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val bNext = b
      val b2 = f(a, b._1)
      (b2, cons(b2, b._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
   * Exercise 5.8
   */
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  /**
   * Exercise 5.9
   */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /**
   * Exercise 5.10
   */
  def fibs: Stream[Int] = {
    def recurse(a: Int, b: Int): Stream[Int] =
      cons(a, recurse(b, a + b))

    recurse(0, 1)
  }

  /**
   * Exercise 5.11
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, b)) => cons(a, unfold(b)(f))
      case _ => empty
    }

  /**
   * Exercise 5.12
   */
  def fibs2: Stream[Int] =
    unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }

  def from2(n: Int): Stream[Int] =
    unfold(n)(next => Some((next, next + 1)))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))
}

val s = Stream(1, 2, 3, 4, 5, 6, 7, 8)
val ones: Stream[Int] = cons(1, ones)

println(ones.take(5).toList)
println(ones.exists(_ % 2 != 0))
println(ones.map(_ + 1).exists(_ % 2 == 0))
println(ones.takeWhile(_ == 1))
println(ones.forAll(_ != 1))
println(fibs.take(8).toList)
println(fibs2.takeWhile3(_ < 13).toList)
println(fibs2.zipWith(s)(_ + _).toList)
println(fibs2.take(10).zipAll(s.take(5)).toList)
println(fibs2.startsWith(Stream(0, 1, 1, 2, 3, 5, 8, 13)))
println(fibs2.startsWith(Stream(0, 1, 1, 2, 3, 1231)))
println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
