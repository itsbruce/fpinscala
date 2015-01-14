def fib(n: Int): Int = {
  @annotation.tailrec
  def go(last: Int, current: Int, i: Int): Int = {
    if (i == n) current else go(current, current + last, i + 1)
  }
  n match {
    case 0 => 0
    case x if x > 0 => go(0, 1, 1)
    case e => sys.error("Invalid input: " + e)
  }
}
