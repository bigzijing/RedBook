package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
   * Exercise 6.1
   * Int.MaxValue =  2147483647
   * Int.MinValue = -2147483648
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRNG) = rng.nextInt
    if (int < 0) (-(int + 1), nextRNG) else (int, nextRNG)
  }

  /**
   * Exercise 6.2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (int, nextRNG) = nonNegativeInt(rng)
    (int / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  /**
   * Exercise 6.3
   */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng2) = nonNegativeInt(rng)
    val (dbl, rng3) = double(rng2)

    ((int, dbl), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, dbl), nextRNG) = intDouble(rng)

    ((dbl, int), nextRNG)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (dbl, rng2) = double(rng)
    val (dbl2, rng3) = double(rng2)
    val (dbl3, rng4) = double(rng3)

    ((dbl, dbl2, dbl3), rng4)
  }

  /**
   * Exercise 6.4
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (List.empty[Int], rng)
    else {
      val (int, rng2) = rng.nextInt
      val (xs, rng3) = ints(count - 1)(rng2)
      (int :: xs, rng3)
    }

  /**
   * Exercise 6.5
   */
  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  /**
   * Exercise 6.6
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  /**
   * Exercise 6.7 (hard)
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /**
   * Exercise 6.8
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, nextRNG) = f(rng)
    g(a)(nextRNG)
  }

  /**
   * Exercise 6.9
   */
  def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map_2(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapGeneric[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) =
    s => {
      val (a2, nextS) = a(s)
      (f(a2), nextS)
    }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, nextS) = run(s)
      (f(a), nextS)
    })

  def map2[B,C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => s2.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, nextS) = run(s)
      f(a).run(nextS)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def update(input: Input): Machine = (input, this) match {
    case (_, Machine(_, candies, _)) if candies <= 0 => this
    case (Turn, Machine(true, _, _)) => this
    case (Coin, Machine(false, _, _)) => this
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
  }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List.empty[A]))((s, acc) => s.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  /**
   * Exercise 6.11 (hard)
   */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(inputs.map(input => State.modify[Machine](_.update(input))))
      s <- get
    } yield (s.coins, s.candies)
  }
}


//package fpinscala.state
//
//
//trait RNG {
//  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
//}
//
//object RNG {
//  // NB - this was called SimpleRNG in the book text
//
//  case class Simple(seed: Long) extends RNG {
//    def nextInt: (Int, RNG) = {
//      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
//      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
//      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
//      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
//    }
//  }
//
//  type Rand[+A] = RNG => (A, RNG)
//
//  val int: Rand[Int] = _.nextInt
//
//  def unit[A](a: A): Rand[A] =
//    rng => (a, rng)
//
//  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
//
//  /**
//   * Exercise 6.1
//   * Int.MaxValue =  2147483647
//   * Int.MinValue = -2147483648
//   */
//  def nonNegativeInt(rng: RNG): (Int, RNG) = {
//    val (int, nextRNG) = rng.nextInt
//    if (int < 0) (-(int + 1), nextRNG) else (int, nextRNG)
//  }
//
//  /**
//   * Exercise 6.2
//   */
//  def double(rng: RNG): (Double, RNG) = {
//    val (int, nextRNG) = nonNegativeInt(rng)
//    (int / (Int.MaxValue.toDouble + 1), nextRNG)
//  }
//
//  /**
//   * Exercise 6.3
//   */
//  def intDouble(rng: RNG): ((Int,Double), RNG) = {
//    val (int, rng2) = nonNegativeInt(rng)
//    val (dbl, rng3) = double(rng2)
//
//    ((int, dbl), rng3)
//  }
//
//  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
//    val ((int, dbl), nextRNG) = intDouble(rng)
//
//    ((dbl, int), nextRNG)
//  }
//
//  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
//    val (dbl, rng2) = double(rng)
//    val (dbl2, rng3) = double(rng2)
//    val (dbl3, rng4) = double(rng3)
//
//    ((dbl, dbl2, dbl3), rng4)
//  }
//
//  /**
//   * Exercise 6.4
//   */
//  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
//    if (count <= 0) (List.empty[Int], rng)
//    else {
//      val (int, rng2) = rng.nextInt
//      val (xs, rng3) = ints(count - 1)(rng2)
//      (int :: xs, rng3)
//    }
//
//  /**
//   * Exercise 6.5
//   */
//  def double2: Rand[Double] =
//    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
//
//  /**
//   * Exercise 6.6
//   */
//  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//    rng => {
//      val (a, rng2) = ra(rng)
//      val (b, rng3) = rb(rng2)
//
//      (f(a, b), rng3)
//    }
//
//  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
//    map2(ra, rb)((_, _))
//
//  /**
//   * Exercise 6.7 (hard)
//   */
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
//    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))
//
//  def ints2(count: Int): Rand[List[Int]] =
//    sequence(List.fill(count)(int))
//
//  /**
//   * Exercise 6.8
//   */
//  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
//    val (a, nextRNG) = f(rng)
//    g(a)(nextRNG)
//  }
//
//  /**
//   * Exercise 6.9
//   */
//  def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    flatMap(s)(a => unit(f(a)))
//
//  def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//    flatMap(ra)(a => map_2(rb)(b => f(a, b)))
//
//  def nonNegativeLessThan(n: Int): Rand[Int] = {
//    flatMap(nonNegativeInt) { i =>
//      val mod = i % n
//      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
//    }
//  }
//
//  def mapGeneric[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) =
//    s => {
//      val (a2, nextS) = a(s)
//      (f(a2), nextS)
//    }
//}
//
//case class State[S,+A](run: S => (A, S)) {
//
//  def map[B](f: A => B): State[S, B] =
//    State(s => {
//      val (a, nextS) = run(s)
//      (f(a), nextS)
//    })
//
//  def map2[B,C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
//    flatMap(a => s2.map(b => f(a, b)))
//
//  def flatMap[B](f: A => State[S, B]): State[S, B] =
//    State(s => {
//      val (a, nextS) = run(s)
//      f(a).run(nextS)
//    })
//}
//
//sealed trait Input
//case object Coin extends Input
//case object Turn extends Input
//
//case class Machine(locked: Boolean, candies: Int, coins: Int) {
//  def update(input: Input): Machine = (input, this) match {
//    case (_, Machine(_, candies, _)) if candies <= 0 => this
//    case (Turn, Machine(true, _, _)) => this
//    case (Coin, Machine(false, _, _)) => this
//    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
//    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
//  }
//}
//
//object State {
//  type Rand[A] = State[RNG, A]
//
//  def unit[S, A](a: A): State[S, A] =
//    State(s => (a, s))
//
//  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
//    sas.foldRight(unit[S, List[A]](List.empty[A]))((s, acc) => s.map2(acc)(_ :: _))
//
//  def get[S]: State[S, S] = State(s => (s, s))
//
//  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
//
//  def modify[S](f: S => S): State[S, Unit] = for {
//    s <- get
//    _ <- set(f(s))
//  } yield ()
//
//  /**
//   * Exercise 6.11 (hard)
//   */
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    for {
//      _ <- State.sequence(inputs.map(input => State.modify[Machine](_.update(input))))
//      s <- get
//    } yield (s.coins, s.candies)
//  }
//}
