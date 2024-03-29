package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions
import scala.::

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) =>
    UnitFuture(
      a
    ) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /** Exercise 7.1
    */
  def map2[A, B, C](
    a: Par[A],
    b: Par[B]
  )(
    f: (A, B) => C
  ): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(
        f(af.get, bf.get)
      ) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  /** Exercise 7.3 (hard)
    */
  def map2Hard[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Hard(af, bf, f)
    }

  case class Map2Hard[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def cancel(mayInterruptIfRunning: Boolean) =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def isCancelled = a.isCancelled || b.isCancelled

    override def isDone = a.isDone && b.isDone

    override def get() = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit) = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(value) => value
      case None =>
        val start = System.nanoTime()
        val ar    = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop  = System.nanoTime()
        val aTime = stop - start
        val br    = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret   = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  def fork[A](
    a: => Par[A]
  ): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Exercise 7.4
    */
  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parMap[A, B](ps: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = fork {
    val fbs: IndexedSeq[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /** Exercise 7.5 (hard)
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def sequence[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if (ps.isEmpty) unit(Vector())
    else if (ps.length == 1) map(ps.head)(p => Vector(p))
    else {
      val (left, right) = ps.splitAt(ps.length / 2)
      map2(sequence(left), sequence(right))(_ ++ _)
    }
  }

  /** Exercise 7.6
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val filtered = as.filter(f)
    sequence(filtered.map(asyncF(a => a)))
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  /** Exercise 7.11
    */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }

  /** Exercise 7.12
    */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  /** Exercise 7.13
    */
  def choose[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(pa).get
      run(es)(choices(k))
    }

  def choice2[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choose(a)(a => if (a) ifTrue else ifFalse)

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    choose(n)(n => choices(n))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /** Exercise 7.14
    */
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMapViaJoin(a)(a => a)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B]                     = Par.map(p)(f)
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
    def zip[B](b: Par[B]): Par[(A, B)]                = p.map2(b)((_, _))
  }
}

object Examples {

  import Par._

  def sum(
    ints: IndexedSeq[Int]
  ): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
