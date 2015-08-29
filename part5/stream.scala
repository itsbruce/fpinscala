// Part 5 - Streams

sealed trait Stream[+A] {
  import Stream._
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // 5.1 - stack safe
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], f: List[A] => List[A]): List[A] = s match {
      case Empty => f(Nil)
      case Cons(h, t) => go(t(), f compose { h() :: _ })
    }
    go(this, identity)
  }

  // 5.2 - no stack safety worries because lazy
  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, _) if n == 1 => Cons(h, () => empty)
    case Cons(h, t) => Cons(h, () => {t().take(n - 1)})
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => { 
      lazy val head = h()
      if (p(head)) cons(head, t() takeWhile p) else Empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case (Cons(h,t)) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)
    { (a, b) => p(a) || b }

  // 5.4
  def forAll(p: A => Boolean): Boolean = ! exists(! p(_))

  // 5.5
  def takeWhileF(p: A => Boolean): Stream[A] = foldRight(empty[A])
    { (a, bs) => if (! p(a)) empty else cons(a, bs) }

  // 5.6 - Why is this listed as *hard* ?
  def headOptionF: Option[A] = foldRight(None: Option[A]) { (a, _) => Some(a) }

  // 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])
    { (a, bs) => cons(f(a), bs) }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])
    { (a, bs) => if (p(a)) cons(a, bs) else bs }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])
    { (a, bs) => f(a) append bs }

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)
    { (a, bs) => cons(a, bs) }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
