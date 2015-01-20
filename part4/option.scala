// Exercise 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse None
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }
  def orElse[B >:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] =
    /*I did filter before flatMap
    if (this map f getOrElse false) this else None
     *But using flatMap is more common.
     */
    this flatMap (a => if (f(a)) this else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

// Exercise 4.2
def variance(xs: Seq[Double]): Option[Double] = {
  val n = xs.size
  (if (n > 0) Some(xs.head) else None) flatMap
    (x => Some(xs.tail.foldLeft(x)(_ + _) / n)) flatMap
      (m => Some(xs.foldLeft(0.0)((sum, x) => sum + Math.pow(x - m, 2)) / n))
}
