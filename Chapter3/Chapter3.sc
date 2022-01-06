object Chapter3 {

  val x = scala.collection.immutable.List(1, 2, 3, 4, 5) match {
    case ::(x, ::(2, ::(4, _))) => x
    case scala.collection.immutable.Nil => 42
    case ::(x, ::(y, ::(3, ::(4, _)))) => x + y
    case _ => 101
  }

  def main(args: Array[String]): Unit = 
    println(x)
    println(scala.collection.immutable.List(1, 2, 3, 4, 5))

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

    def product(ints: List[Double]): Double = ints match {
      case Nil => 1.0
      case Cons(head, tail) => head * product(tail)
    }

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

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(head, tail) => Cons(head, append(tail, a2))
    }

    /**
      * Exercise 3.6
      */
    def init[A](ls: List[A]): List[A] = ls match {
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
      case Nil => Nil
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(head, tail) => f(head, foldRight(tail, z)(f))
      }

    def sum2(ls: List[Int]): Int =
      foldRight(ls, 0)(_ + _)

    def product2(ls: List[Double]): Double =
      foldRight(ls, 1.0)(_ * _)

    /**
      * Exercise 3.9
      */
    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, count) => count + 1)

    /**
      * Exercise 3.10
      */
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
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
      foldRight2(ls, Nil: List[A]){ (a, b) =>
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
}

/**
  * Test Suite from the official FP in Scala GitHub repo
  * https://github.com/philipschwarz/fpinscala/blob/master/exercises/src/main/scala/fpinscala/datastructures/List.scala#L91
  */
object Tests {
  import Chapter3._
  import Chapter3.List._

  def test_sum(sum: List[Int] => Int): Unit =
  {
    assert( sum(           Nil ) ==  0, "sum of empty list should be 0")
    assert( sum(       List(5) ) ==  5, "sum of single-element list should be the element" )
    assert( sum( List(1,2,3,4) ) == 10, "sum of list should be sum of its elements" )
  }

  def test_sum2(): Unit = test_sum(sum2)
  def test_sum3(): Unit = test_sum(sum3)

  def test_product(product: List[Double] => Double): Unit =
  {
    assert( product( Nil)                       ==  1.0,  "product of empty list should be 1.0" )
    assert( product( List(7.0))                 ==  7.0,  "product of single-element list should be the element" )
    assert( product( List(1.0, 2.0, 3.0, 4.0) ) == 24.0,  "product of list should be product of its elements" )
    assert( product( List(1.0, 2.0, 0.0, 4.0) ) ==  0.0,  "product of list containing zero should be zero" )
  }

  def test_product2(): Unit = test_product(product2)
  def test_product3(): Unit = test_product(product3)

  def test_append(append: (List[Int], List[Int]) => List[Int]): Unit =
  {
    assert( append( Nil,             Nil ) ==           Nil, "append of two empty lists should be empty list")
    assert( append( Nil,         List(3) ) ==       List(3), "append of empty list to a list should be list")
    assert( append( List(3),         Nil ) ==       List(3), "append of list to empty list should be list")
    assert( append( List(1,2),   List(3) ) ==   List(1,2,3), "append of list to one-element list should be concatenation of lists")
    assert( append( List(1),   List(2,3) ) ==   List(1,2,3), "append of one-element list to list should be concatenation of lists")
    assert( append( List(1,2), List(3,4) ) == List(1,2,3,4), "append of two lists should be concatenation of lists")
  }

  def test_tail(): Unit =
  {
    assert( List.tail(         Nil ) ==       Nil, "tail of Nil should be Nil")
    assert( List.tail(     List(3) ) ==       Nil, "tail of single-element list should be Nil")
    assert( List.tail( List(1,2,3) ) == List(2,3), "tail of list should be rest")
  }

  def test_setHead(): Unit =
  {
    assert( setHead(       Nil, 1 ) ==       Nil, "setHead of empty list should be empty list")
    assert( setHead(   List(2), 1 ) ==   List(1), "setHead of single-element list should be two-element list")
    assert( setHead( List(3,2), 1 ) == List(1,2), "setHead of two-element list should be three-element list")
  }

  def test_drop(): Unit =
  {
    assert( drop( Nil,          0) ==         Nil, "drop of zero elements from empty list is empty list")
    assert( drop( Nil,          1) ==         Nil, "drop of one element from empty list is empty list")
    assert( drop( Nil,         10) ==         Nil, "drop of many elements from empty list is empty list")
    assert( drop( List(3),      0) ==     List(3), "drop of zero elements from single-element list is the list")
    assert( drop( List(3),      1) ==         Nil, "drop of one element from single-element list is empty list")
    assert( drop( List(3),     10) ==         Nil, "drop of many elements from single-element list is empty list")
    assert( drop( List(1,2,3),  0) == List(1,2,3), "drop of zero elements from list is list")
    assert( drop( List(1,2,3),  1) ==   List(2,3), "drop of one elements from list is list without 1st element")
    assert( drop( List(1,2,3),  2) ==     List(3), "drop of n elements from list is list without 1st n elements")
    assert( drop( List(1,2,3), 10) ==         Nil, "drop of too many elements from list is empty list")
  }

  def test_dropWhile(): Unit =
  {
    val positive = (x: Int) => x > 0
    assert( dropWhile(                  Nil, positive ) ==                  Nil, "dropWhile of empty list should be empty list")
    assert( dropWhile(              List(1), positive ) ==                  Nil, "dropWhile of list with single valid element should be empty list")
    assert( dropWhile( List( 1,  2,  3,  4), positive ) ==                  Nil, "dropWhile of list with only valid elements should be empty list")
    assert( dropWhile( List( 1,  2, -3,  4), positive ) ==          List(-3, 4), "dropWhile of list with two leading valid elements should be list without leading elements")
    assert( dropWhile( List( 1, -2, -3,  4), positive ) ==      List(-2, -3, 4), "dropWhile of list with one leading valid element should be list without leading element")
    assert( dropWhile( List(-1, -2, -3,  4), positive ) ==  List(-1, -2, -3, 4), "dropWhile of list with no leading valid elements should be same list")
    assert( dropWhile( List(-1, -2, -3, -4), positive ) == List(-1, -2, -3, -4), "dropWhile of list with no valid elements should be Nil")
  }

  def test_init(): Unit =
  {
    assert( init(         Nil ) ==       Nil, "init of empty list should be empty list")
    assert( init(     List(3) ) ==       Nil, "init of single-element-list should be empty list")
    assert( init( List(1,2,3) ) == List(1,2), "init of list should not have last element")
  }

  def test_length(length: List[Int] => Int): Unit =
  {
    assert( length(         Nil ) == 0, "length of empty list is zero")
    assert( length(     List(1) ) == 1, "length of single-element list is one")
    assert( length( List(1,2,3) ) == 3, "length of n-element list is n")
  }

  def test_foldLeft(): Unit =
  {
    assert( foldLeft(  List(1, 2, 3, 4, 5), 0) (_ + _) ==
      foldRight( List(1, 2, 3, 4, 5), 0) (_ + _) &&
      foldLeft2(  List(1, 2, 3, 4, 5), 0) (_ + _) ==
        foldRight2( List(1, 2, 3, 4, 5), 0) (_ + _) &&
      foldLeft2(  List(1, 2, 3, 4, 5), 0) (_ + _) ==
        foldLeft( List(1, 2, 3, 4, 5), 0) (_ + _),
      "foldLeft should compute the same sum value as foldRight")

    assert( foldLeft(  List(1, 2, 3, 4, 5), 1) (_ * _) ==
      foldRight( List(1, 2, 3, 4, 5), 1) (_ * _) &&
      foldLeft2(  List(1, 2, 3, 4, 5), 1) (_ * _) ==
      foldRight2( List(1, 2, 3, 4, 5), 1) (_ * _) &&
      foldLeft(  List(1, 2, 3, 4, 5), 1) (_ * _) ==
      foldRight2( List(1, 2, 3, 4, 5), 1) (_ * _),
      "foldLeft should compute the same product value as foldRight")

    assert( foldLeft(  List("a", "b", "c"), "") (_ + _) ==
      foldRight( List("a", "b", "c"), "") (_ + _) &&
      foldLeft2(  List("a", "b", "c"), "") (_ + _) ==
      foldRight2( List("a", "b", "c"), "") (_ + _) &&
      foldLeft(  List("a", "b", "c"), "") (_ + _) ==
      foldRight2( List("a", "b", "c"), "") (_ + _),
      "foldLeft should compute the same concatenation value as foldRight")
  }

  def test(): Unit = {
    test_sum(sum)
    test_sum2
    test_sum3
    test_product(product)
    test_product2
    test_product3
    test_append(append)
    test_append(append2)
    test_tail
    test_setHead
    test_drop
    test_dropWhile
    test_init
    test_length(length)
    test_length(length2)
    test_foldLeft
  }
}

object Tree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    /**
      * Exercise 3.25
      */
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    /**
      * Exercise 3.26
      */
    def maximum(tree: Tree[Int]): Int = {
      def recurse(tree: Tree[Int], acc: Int): Int = tree match {
        case Leaf(value) => value.max(acc)
        case Branch(left, right) => recurse(left, acc).max(recurse(right, acc))
      }

      recurse(tree, -1)
    }

    /**
      * Exercise 3.27
      */
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    /**
      * Exercise 3.28
      */
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    /**
      * Exercise 3.29
      */
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

    def size2[A](tree: Tree[A]): Int =
      fold(tree)(_ => 1)(_ + _ + 1)

    def maximum2(tree: Tree[Int]): Int =
      fold(tree)(x => x)((l, r) => l max r)

    def depth2[A](tree: Tree[A]): Int =
      fold(tree)(_ => 1)((l, r) => 1 + (l max r))

    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold(tree)(value => Leaf(f(value)): Tree[B])((l, r) => Branch(l, r))
  }

  val testTree = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
}

println("Starting test")
Tests.test()
println("Test ended")

import Tree.testTree
import Tree.Tree._
println(size(testTree))
println(size2(testTree))
println(maximum(testTree))
println(maximum2(testTree))
println(depth(testTree))
println(depth2(testTree))
println(map(testTree)(_ * 10))
println(map2(testTree)(_ * 10))