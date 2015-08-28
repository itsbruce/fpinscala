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

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, _) if n == 1 => Cons(h, () => empty)
    case Cons(h, t) => Cons(h, () => {t().take(n - 1)})
  }
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
