// Chapter 6, Functional Programming n Scala

trait RNG {
  def nextInt: (Int, RNG)
}

objerct RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // 6.1
  @annotation.tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i != Int.MinValue) (math.abs(i), r)
    else nonNegativeInt(r)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    @annotation.tailrec
    def go(ir: (Int, RNG)): (Double, RNG) = ir match {
      case (i, r) =>
        if  (i == Int.MaxValue) go(nonNegativeInt(r))
        else (i.toDouble / Int.MaxValue, r)
    }
    go(nonNegativeInt(rng))
  }

}
