object Chapter2 {

  import Module._

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatResults("absolute value", 42, abs))
    println(formatResults("factorial", 7, fact))
  }

  object Module {
    def abs(n: Int): Int = 
      if (n < 0) -n
      else n

    def fact(n: Int): Int = {
      @annotation.tailrec
      def factRecursive(x: Int, acc: Int): Int = 
        if (x <= 1) acc
        else factRecursive(x - 1, acc * x)

      factRecursive(n, 1)
    }

    def formatAbs(x: Int): String = {
      val msg = "The absolute value of %d is %d."
      msg.format(x, abs(x))
    }

    def formatResults(name: String, n: Int, f: Int => Int): String = {
      val msg = "The %s of %d is %d."
      msg.format(name, n, f(n))
    }

    def findFirst(ss: Array[String], key: String): Int = {
      @annotation.tailrec
      def loop(n: Int): Int = 
        if (n >= ss.length) -1
        else if (ss(n) == key) n
        else loop(n + 1)

      loop(0)
    }

    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
      @annotation.tailrec
      def loop(n: Int): Int = 
        if (n >= as.length) -1
        else if (p(as(n))) n
        else loop(n + 1)

      loop(0)
    }
  }

  object Exercises {
    def fib(n: Int): Int = {
      @annotation.tailrec
      def fibRec(x: Int, curr: Int, next: Int): Int = {
        if (x == 0) curr
        else fibRec(x - 1, next, next + curr)
      }

      fibRec(n, 0, 1)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @annotation.tailrec
      def arrRecursive(in: Int): Boolean = {
        if (in + 1 >= as.length) true
        else {
          if (ordered(as(in), as(in + 1))) arrRecursive(in + 1) else false
        }
      }

      arrRecursive(0)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      (a: A) => {
        (b: B) => f(a, b)
      }
    }

    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }

    def compose[A, B, C](f: B => C, g: A => B): A => C =
      (a: A) => f(g(a))
  }

}
