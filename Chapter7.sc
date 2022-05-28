import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /**
   * Exercise 7.1
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  /**
   * Exercise 7.3 (hard)
   */
  def map2Hard[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Hard(af, bf, f)
    }

  case class Map2Hard[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def cancel(mayInterruptIfRunning: Boolean) = a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def isCancelled = a.isCancelled || b.isCancelled

    override def isDone = a.isDone && b.isDone

    override def get() = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit) = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(value) => value
      case None =>
        val start = System.nanoTime()
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime()
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  /**
   * Exercise 7.4
   */
  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
   * Exercise 7.5 (hard)
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  /**
   * Exercise 7.6
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val filtered = as.filter(f)
    sequence(filtered.map(asyncF(a => a)))
  }

  /**
   * Exercise 7.11
   */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  /**
   * Exercise 7.12
   */
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  /**
   * Exercise 7.13
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

  /**
   * Exercise 7.14
   */
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMapViaJoin(a)(a => a)
}

def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size <= 1)
    Par.unit(ints.headOption.getOrElse(0))
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }

def parGeneric[A](as: IndexedSeq[A], fallback: => A)(f: (A, A) => A): Par[A] =
  if (as.size <= 1)
    Par.unit(as.headOption.getOrElse(fallback))
  else {
    val (l, r) = as.splitAt(as.length / 2)
    Par.map2(Par.fork(parGeneric(l, fallback)(f)), Par.fork(parGeneric(r, fallback)(f)))(f)
  }

def maxInList(ints: IndexedSeq[Int]): Par[Int] =
  parGeneric[Int](ints, 0)((a, b) => if (a > b) a else b)

def maxWordsInPara(para: List[String]): Par[Int] =
  parGeneric[Int](para.map(_.length).toIndexedSeq, 0)(_ + _)