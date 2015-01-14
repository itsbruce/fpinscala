def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean =
    if (n + 1 < as.size) {
      if (ordered(as(n), as(n + 1))) go(n + 1) else false
    } else {
      true
    }
  go(0)
}
