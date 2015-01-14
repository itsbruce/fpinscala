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

}
