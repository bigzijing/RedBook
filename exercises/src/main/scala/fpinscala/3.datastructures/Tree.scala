package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /*
  We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.

  Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise.
  */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /*
  Again, note how similar the implementation is to `size` and `maximum`.
  */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /*
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
  accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
  this function to implement just about any recursive function that would otherwise be defined by pattern matching.
  */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _ max _)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    val tree = Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(Leaf(3), Leaf(4))
    )


    val stringTree = Branch(
      Branch(Leaf("1"), Leaf("2")),
      Branch(Leaf("3"), Leaf("4"))
    )

    assert(size(tree) == 7)
    assert(maximum(tree) == 4)
    assert(depth(tree) == 2)
    assert(map(tree)(_.toString) == stringTree)
    assert(fold(tree)(identity)(_ + _) == 10)
    assert(sizeViaFold(tree) == 7)
    assert(maximumViaFold(tree) == 4)
    assert(depthViaFold(tree) == 2)
    assert(mapViaFold(tree)(_.toString) == stringTree)
  }
}