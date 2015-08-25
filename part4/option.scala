// Exercise 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def orElse[B >:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
final case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    val n = xs.size
    (if (n > 0) Some(xs.head) else None) flatMap
      (x => Some(xs.tail.foldLeft(x)(_ + _) / n)) map
        (m => xs.foldLeft(0.0)((sum, x) => sum + Math.pow(x - m, 2)) / n)
  }

  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (x => b map (f(x, _)))

  /* Exercise 4.4.
   * Stops as soon as None is found, but is not stack safe
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case x :: xs => map2(x, sequence(xs))(_ :: _)
    case Nil => Some(Nil)
  }

  // Using flatmap rather than map2.  Not stack-safe
  def sequenceF[A](a: List[Option[A]]): Option[List[A]] = a match {
    case x :: xs => x flatMap { y => sequenceF(xs) map { y :: _ } }
    case Nil => Some(Nil)
  }

  /* foldRight implementation.  Traverses entire list no matter where None may
   * be.  But stack safe on recent Scala versions
   */
  def sequenceR[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((x, oxs) => map2(x, oxs)(_ :: _))

  // Stack-safe short-circuit version
  def sequenceSafe[A](a: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def go(os: List[Option[A]], xs: List[A]): Option[List[A]] = os match {
      case Nil => Some(xs)
      case None :: _ => None
      case Some(x) :: rest => go(rest, x :: xs)
    }
  go(a, Nil) map { _.reverse }
  }

  /* Exercise 4.5
   * Stack-safe and short-circuiting
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def go(as: List[A], bs: Option[List[B]]): Option[List[B]] = (as, bs) match {
      case (Nil, _) => bs
      case (_, None) => None
      case (a :: rest, _) => go(rest, f(a) flatMap { aa => bs map { aa :: _ } })
    }
    go(a, Some(Nil)) map { _.reverse }
  }
  
  // Sequence using traverse
  def sequenceT[A](a: List[Option[A]]): Option[List[A]] = 
    traverse(a)(identity)
  
}
