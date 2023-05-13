package fpinscala
package monads

import parallelism._
import state._
import parallelism.Par._
import language.higherKinds
import fpinscala.testing.Gen
import fpinscala.parsing.Parsers
import fpinscala.laziness.Stream
import fpinscala.state.State

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /** Exercise 11.3
    */
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A])) { case (next, acc) =>
      map2(next, acc)(_ :: _)
    }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B])) { case (next, acc) =>
      map2(f(next), acc)(_ :: _)
    }

  /** Exercise 11.4
    */
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  /** Exercise 11.5
    * `replicateM` basically takes whatever is inside the M, put n of it inside a list and wrap it inside a Monad
    *
    * If the Monad is a List, it will return a List[List[A]], where the length of the outer list would be n,
    * and the length of the inner lists would be the same as they are repeated
    *
    * If the Monad is an Option, it will return an Option[List[A]] -- if it's a None, obviously it would return None,
    * and if it's a Some, then likewise, there'd be the element a repeated n times
    */

  /** Exercise 11.6
    */
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case head :: tail =>
      flatMap(f(head)) { bool =>
        if (bool) map(filterM(tail)(f))(head :: _)
        else filterM(tail)(f)
      }
    case Nil => unit(Nil)
  }

  /** Exercise 11.7
    */
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(b => g(b))

  /**
   * Exercise 11.8
   */
  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  /**
   * Exercise 11.9
   * compose(compose(f, g), h) == compose(f, compose(g, h))
   *
   * From 11.7:
   * a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g, h))
   * a => flatMap(b => flatMap(f(b))(g))(a)(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
   * a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
   *
   * Sub x for f(a):
   * a => flatMap(flatMap(x)(g))(h) == a => flatMap(x)(b => flatMap(g(b))(h))
   * flatMap(flatMap(x)(g))(h) == flatMap(x)(a => flatMap(g(a))(h))
   */

  /**
   * Exercise 11.10
   * 1.
   * compose(f, unit)(x) == f(x)
   * a => flatMap(f(a)(unit))(x) == f(x)
   * flatMap(f(x))(unit) == f(x)
   *
   * 2.
   * compose(unit, f)(x) == f(x)
   * flatMap(unit(x))(f) == f(x)
   */

  /**
   * Exercise 11.11
   *
   */


  /**
   * Exercise 11.12
   */
  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  /**
   * Exercise 11.13
   */
  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {

  /** Exercise 11.1
    */
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMapViaJoin(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  /** Exercise 11.2
    */
  def stateMonad[S] = {
    type StateMonad[A] = State[S, A]

    new Monad[StateMonad] {
      override def unit[A](a: => A): StateMonad[A] = State(s => (a, s))

      override def flatMap[A, B](ma: StateMonad[A])(f: A => StateMonad[B]): StateMonad[B] = ma.flatMap(f)
    }
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  /**
   * Exercise 11.17
   */
  def map[B](f: A => B): Id[B]         = this.copy(value = f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {

  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A]                                               = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(st.run(r)).run(r))
  }
}
