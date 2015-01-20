// Exercise 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }
  //def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }
  def orElse[B >:A](ob: => Option[B]): Option[B] = Some(this) getOrElse ob
  //def filter(f: A => Boolean): Option[A]
}
case class Some[+A](get: A) extends Option[A]
// Exercise work
case object None extends Option[Nothing]
// Exercise work
