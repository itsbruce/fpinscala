package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
     case Nil => 1.0
     case Cons(0.0, _) => 0.0
     case Cons(x, xs) => x * product(xs)
   }

   def apply[A](as: A*): List[A] = 
     if (as.isEmpty) Nil
     else Cons(as.head, apply(as.tail: _*))

   def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
     as match {
       case Nil => z
       case Cons(x, xs) => f(x, foldRight(xs, z)(f))
     }

   def sum2(ns: List[Int]) =
     foldRight(ns, 0)(_ + _)

   def product2(ns: List[Double]) =
     foldRight(ns, 1.0)(_ + _)

  // Exercise 3.2
   def tail[A](xs: List[A]): List[A] = xs match {
     case Nil => sys.error("Empty list")
     case Cons(_, ys) => ys
   }

  // Exercise 3.3
   def setHead[A](xs: List[A], x: A): List[A] = xs match {
     case Nil => apply(x)
     case Cons(_, ys) => Cons(x, ys)
   }

   // Exercise 3.4
   @annotation.tailrec
   def drop[A](l: List[A], n: Int): List[A] = l match {
     case _ if n < 1 => l
     case Nil => Nil
     case Cons(_, xs) => drop(xs, n - 1)
   }

   // Exercise 3.5
   @annotation.tailrec
   def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
     case Nil => Nil
     case Cons(x, xs) if f(x) => dropWhile(xs, f)
     case _ => l
   }

   // Exercise 3.6
   def init[A](l: List[A]): List[A] = {
     @annotation.tailrec
     def go(xs: List[A], acc: List[A]): List[A] = xs match {
       case Nil => sys.error("Unsupported operation: empty.init")
       case Cons(_, Nil) => acc
       case Cons(y, ys) => go(ys, Cons(y, acc))
     }
     go(l, Nil)
   }

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, n) => n + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11
  def sumLeft(is: List[Int]): Int = foldLeft(is, 0)(_ + _)

  def productLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def size[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))

  // Exercise 3.13
  def foldr[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A,B => B](as, identity)((g, a) => g compose (f(a, _: B))) (z)

  // Exercise 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] = 
    foldLeft(a1, identity[List[A]] _)((f, a) => f compose (Cons(a, _))) (a2)

  // Exercise 3.15
  def flatten[A](xs: List[List[A]]): List[A] =
    foldLeft(xs, identity[List[A]] _)((f, a) => f compose (append(a, _))) (Nil)

}
