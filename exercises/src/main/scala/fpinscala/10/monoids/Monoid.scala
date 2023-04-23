package fpinscala.monoids

import fpinscala.parallelism.Nonblocking.Par.toParOps
import fpinscala.parallelism.Nonblocking._
import fpinscala.testing.{Gen, Prop}

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero                       = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero                         = Nil
  }

  /** Exercise 10.1
    */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero                 = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero                 = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {

    def op(a1: Boolean, a2: Boolean): Boolean =
      a1 || a2

    def zero: Boolean = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {

    def op(a1: Boolean, a2: Boolean): Boolean =
      a1 && a2

    def zero: Boolean = false
  }

  /** Exercise 10.2
    */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1.orElse(a2)

    def zero: Option[A] = None
  }

  /** Exercise 10.3
    */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    def op(a1: A => A, a2: A => A): A => A =
      (a: A) => a1(a2(a))

    def zero: A => A = identity
  }

  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)

    def zero: A = m.zero
  }

  /** Exercise 10.4
    */
  import fpinscala.testing.Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)
    ) { case (x, y, z) =>
      m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    } &&
      forAll(gen)(x => m.op(x, m.zero) == x)

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  /** Exercise 10.5
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  /** Exercise 10.6 (hard)
    */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val m = endoMonoid[B]

    foldMap(as, m)(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val m = new Monoid[B => B] {
      override def op(a1: B => B, a2: B => B): B => B =
        (b: B) => a2(a1(b))

      override def zero: B => B = identity
    }

    foldMap(as, m)(a => b => f(b, a))(z)
  }

  /** Exercise 10.7
    */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as(0))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  /** Exercise 10.9 (hard)
    */
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        println(a1, a2)
        (a1, a2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1.min(x2), y1.max(y2), q && p && x2 >= y1))
          case (x, None) => x
          case (None, x) => x
        }
      }

      def zero: Option[(Int, Int, Boolean)] = None
    }
    foldMapV(ints, m)(int => Some((int, int, true))).forall(_._3)
  }

  sealed trait WC
  case class Stub(chars: String)                            extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /** Exercise 10.8 (hard)
    */
  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {

      def op(a1: Par[A], a2: Par[A]): Par[A] =
        a1.map2(a2)(m.op)

      def zero: Par[A] = Par.unit(m.zero)
    }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  /** Exercise 10.10
    */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b))                    => Stub(a + b)
      case (Stub(a), Part(a2, int, b))           => Part(a + a2, int, b)
      case (Part(a, int, b), Stub(b2))           => Part(a, int, b + b2)
      case (Part(a, int, b), Part(a2, int2, b2)) => Part(a, int + int2 + (if ((b + a2).isEmpty) 1 else 0), b2)
    }

    def zero: WC = Stub("")
  }

  /** Exercise 10.11
    */
  def count(s: String): Int = {
    def countStub(s: String) = if (s.isEmpty) 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid) { char =>
      if (char.isWhitespace) Part("", 0, "")
      else Stub(char.toString)
    } match {
      case Stub(s)         => countStub(s)
      case Part(l, int, r) => int + countStub(l) + countStub(r)
    }
  }

  /** Exercise 10.16
    */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      def zero: (A, B) = (A.zero, B.zero)
    }

  /** Exercise 10.17
    */
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {

      def op(a1: A => B, a2: A => B): A => B =
        a => B.op(a1(a), a2(a))

      def zero: A => B = _ => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {

      def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
        }

      def zero: Map[K, V] = Map.empty
    }

  /** Exercise 10.18
    */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}

/** Exercise 10.12
  */
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  /** Exercise 10.15
    */
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/** Exercise 10.13
  */
object TreeFoldable extends Foldable[Tree] {

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(v)      => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(v)      => f(v, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

/** Exercise 10.14
  */
object OptionFoldable extends Foldable[Option] {

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(v) => f(v)
      case None    => mb.zero
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case Some(v) => f(z, v)
      case None    => z
    }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case Some(v) => f(v, z)
      case None    => z
    }
}
