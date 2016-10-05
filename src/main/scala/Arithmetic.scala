/**
  * Created by Psycho7 on 10/5/16.
  */
object Arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    // P31
    def isPrime: Boolean =
      (start > 1) && (2 to Math.sqrt(start).toInt).forall{ start % _ != 0 }

    // P33
    def isCoprimeTo(that: S99Int): Boolean = gcd(this, that) == 1

    // P34
    def totient: Int = (1 to this.start) count { _ isCoprimeTo this}

    // P35
    def primeFactors: List[Int] = {
      def nextPrime(p: Int) = {
        def next(start: Int): Stream[Int] = start #:: next(start + 1)
        next(p + 1).filter(_.isPrime).head
      }

      def loop(n: Int, p: Int, result: List[Int]): List[Int] = n match {
        case x if x < p => result.reverse
        case x if x % p == 0 => loop(n / p, p, p :: result)
        case x if x % p != 0 => loop(n, nextPrime(p), result)
      }

      loop(this, 2, Nil)
    }

    // P36
    def primeFactorMultiplicity: Map[Int, Int] = {
      def encode(now: Int, count: Int, ls: List[Int], result: List[(Int, Int)]): List[(Int, Int)] =
        ls match {
          case Nil => ((now, count) :: result).reverse.tail
          case x :: xs =>
            if (x == now) encode(now, count + 1, xs, result)
            else encode(x, 1, xs, (now, count) :: result)
        }

      val primes = primeFactors
      Map[Int, Int](encode(0, 0, primes, Nil): _*)
    }

    // P37
    def totient2: Int = (1 /: primeFactorMultiplicity) {
      case (a, (p ,r)) => a * (p - 1) * Math.pow(p, r - 1).toInt
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    implicit def s99toInt(i: S99Int): Int = i.start

    // P32
    def gcd(x: Int, y: Int): Int = {
      val r = x % y
      if (r == 0) y
      else gcd(y, r)
    }
  }

}