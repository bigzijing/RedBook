package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.{Par, parMap}
import Gen._
import Prop._

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/**
 * Exercise 8.1
 * - summing a list of the same element == list size * element
 * - summing an empty list == 0
 */

/**
 * Exercise 8.2
 * - max of a list and its reverse should be the same
 * - max of empty list should be None
 * - max of list with single element should be that element
 * - max of list should be >= all elements in list
 */

/**
 * Exercise 8.18
 * - `ls.takeWhile(f) ++ l.dropWhile(f) == l`
 */

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case object Proved extends Result {
  def isFalsified = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  /**
   * Exercise 8.3
   */
  //  def check: Boolean = ???
  //  def &&(other: Prop): Boolean =
  //    this.check && other.check


  /**
   * Exercise 8.9
   */
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case failed => failed
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case passed => passed
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case passed => passed
      }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  def check(p: => Boolean): Prop =
    Prop { (_, _, _) => if (p) Passed else Falsified("()", 0) }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)


  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack tracek:\n ${e.getStackTrace.mkString("\n")}"

}

object Gen {
  /**
   * Exercise 8.4
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  /**
   * Exercise 8.5
   */
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /**
   * Exercise 8.7
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(bool => if (bool) g1 else g2)

  /**
   * Exercise 8.8
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }

  /**
   * Exercise 8.12
   */
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  val smallInt = Gen.choose(-10, 10)

  val maxProp = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  /**
   * Exercise 8.13
   */
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n.max(1)))

  val maxProp1 = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  /**
   * Exercise 8.14
   */
  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || l.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }

  /**
   * Exercise 8.16 (hard)
   */
  val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map { l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      })
  }

  /**
   * Exercise 8.17
   */
  val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)).tag("fork")
}

case class Gen[+A](sample: State[RNG, A]) {
  /**
   * Exercise 8.6
   */
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))

  /**
   * Exercise 8.10
   */
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)((_, _))

}

case class SGen[+A](forSize: Int => Gen[A]) {
  /**
   * Exercise 8.11
   */
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n).flatMap(f(_).forSize(n))
    }

    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}

