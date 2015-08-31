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

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case _ if n < 1 => this
    case Cons(_, t) => t().drop(n - 1)
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

  // Section 5.3 example
  def find(p: A => Boolean): Option[A] = filter(p).headOption

  // Section 5.13
  def mapU[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }

  def takeU(n: Int): Stream[A] = unfold(this, n) {
      case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i - 1)))
      case _ => None
  }

  def takeWhileU(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, that) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _ => None
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this, that) {
      case (Empty, Empty) => None
      case (as, bs) => Some((as.headOption, bs.headOption), (as.drop(1), bs.drop(1)))
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

  // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = cons(a, c)
    c
  }

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  def fibs: Stream[Long] = {
    def fibSum(a: Long, b: Long): Stream[Long] = {
      val ab = a + b
      cons(ab, fibSum(b, ab))
    }
    cons(0, cons(1, fibSum(0, 1)))
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map { case (a, s) => cons(a, unfold(s)(f)) } getOrElse empty

  // 5.12
  def fibsU: Stream[Long] = unfold(0, 1){case (x, y) => Some(x, (y, x + y))}

  def fromU(n: Int): Stream[Int] = unfold(n){x => Some(x, x + 1)}

  def constantU[A](a: A): Stream[A] = unfold(a){x => Some(x, x)}

  def onesU: Stream[Int] = unfold(1){ _ => Some(1, 1) }

}
