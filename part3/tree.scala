sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/*
 * Naive version
 *
 * For the first exercises I am going to do simple, non-stack-safe
 * functional solutions, because safe ones would require a fold
 * I will put the final, safe-fold-based implementation in a separate file
 */

object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = {
    def go(i: Int, node: Tree[A]): Int = {
      val d = i + 1
      node match {
        case Leaf(i) => d
        case Branch(l, r) => go(d,l) max go(d,r)
      }
    }
    go(0, t)
  }

  // Exercise 3.28
  def map[A,B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

}
