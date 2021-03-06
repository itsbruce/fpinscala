// Chapter 6, Functional Programming n Scala

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
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
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = nonNegativeInt(r)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, is: List[Int], r: RNG): (List[Int], RNG) =
      if (n < 1 )
        (is, r)
      else {
        val (i, r2) = nonNegativeInt(r)
        go(n - 1, i :: is, r2)
      }
   go(count, Nil, rng)
 }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // 6.5
  def randDouble: Rand[Double] = map(nonNegativeInt)
    {_ / (Int.MaxValue.toDouble + 1)}

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())){ (r, rs) => map2(r, rs)(_ :: _) }

  def randInts(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  // 6.9
  def mapF[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2F[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a => mapF(rb)(b => f(a, b))}

}

case class State[S, +A](run: S => (A, S)) {
  import State._

  // 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map[B](f: A => B): State[S, B]  =
    flatMap(a => unit(f(a)))

  def map2[B,C](that: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap {a => that map (b => f(a, b))}
}

object State {
  type Rand[A] = State[RNG, A]

  // 6.10
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](Nil)){ (s, acc) => s.map2(acc)(_ :: _) }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}

// Problem 6.11
object Candy {
  import State._

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  /* Public interface to the machine.  I have modified the case class
   * slightly from the example in problem 6.11 to make it more robust
   * and to simplify Input processing - polymorphism beats pattern
   * matching in this case.  The problem itself is not changed.  Note
   * "locked" is not used in my solution but would be necessary in 
   * general usage and is there in the example so it stays
   */
  sealed trait Machine {
    val candies: Int
    val coins: Int
    def locked: Boolean
    def input(i: Input): Machine
  }
  /* Private machine methods, hidden to prevent unsafe operations.
   * sealed is probably redundant given private, I guess
   */
  private sealed trait MachineOps extends Machine {
    final def input(i: Input): Machine = 
      if (candies < 1) this
      else i match {
        case Coin => addCoin
        case Turn => turnKnob
      }
    def addCoin: Machine
    def turnKnob: Machine
  }
  /* Case classes kept private to prevent initialisation with
   * invalid state (e.g. negative numbers of candies or coins)
   * and because I don't like ADT type constructors leaking where
   * it can be avoided
   */
  private case class Locked(candies: Int, coins: Int) extends MachineOps {
    def locked = true
    def addCoin: Machine = Unlocked(candies, coins + 1)
    def turnKnob: Machine = this
  }
  private case class Unlocked(candies: Int, coins: Int) extends MachineOps {
    def locked = false
    def addCoin: Machine = this
    def turnKnob: Machine = Locked(candies - 1, coins)
  }

  object Machine {
    /* Caller can only create valid, locked machines.  Possibly
     * an exception for negative candies/coins would be more
     * appropriate for anything other than an exercise
     */
    def apply(candies: Int, coins: Int): Machine =
      Locked(math.max(0, candies), math.max(0, coins))
    // Pattern matching
    def unapply(m: Machine): Option[(Boolean,Int,Int)] =
      Some(m.locked, m.candies, m.coins)

  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map { i => modify[Machine](_.input(i)) })
    m <- get
  } yield (m.coins, m.candies)

}
