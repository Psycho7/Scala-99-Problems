/**
  * Created by Psycho7 on 10/5/16.
  */
import Arithmetic.S99Int
import org.scalatest._

class ArithmeticTest extends FlatSpec {
  import S99Int._

  "isPrime method" should "determine whether a integer is prime" in {
    assert(2.isPrime)
    assert(3.isPrime)
    assert(!4.isPrime)
  }

  "gcd method" should "determine the greatest common divisor of two positive integer numbers" in {
    assert(gcd(2, 4) == 2)
    assert(gcd(22, 121) == 11)
    assert(gcd(121, 22) == 11)
    assert(gcd(3, 5) == 1)
    assert(gcd(5, 3) == 1)
  }

  "isCoprimeTo method" should "determine whether two positive integer numbers are coprime" in {
    assert(2.isCoprimeTo(3))
    assert(2.isCoprimeTo(5))
    assert(5.isCoprimeTo(4))
    assert(!2.isCoprimeTo(4))
    assert(!121.isCoprimeTo(33))
  }

  "totient method" should "calculate Euler's totient function phi(m)" in {
    assert(10.totient == 4)
  }

  "primeFactors method" should "determine the prime factors of a given positive integer" in {
    assert(315.primeFactors == List(3, 3, 5, 7))
    assert(1.primeFactors == Nil)
    assert(2.primeFactors == 2 :: Nil)
    assert(6.primeFactors == List(2, 3))
  }

  "primeFactorMultiplicity method" should "determine the prime factors of a given positive integer" in {
    assert(315.primeFactorMultiplicity == Map(3 -> 2, 5 -> 1, 7 -> 1))
  }

  "totient2 methoud" should "calculate Euler's totient function phi(m)" in {
    assert((1 to 1000) forall { x => x.totient == x.totient2 })
  }

  "listPrimesinRange method" should "construct a list of all prime numbers in a given range" in {
    assert(listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  "goldbach method" should "find the two prime numbers that sum up to a given even integer" in {
    assert(7.goldbach == (2, 5))
    assert(28.goldbach == (5, 23))
  }
}
