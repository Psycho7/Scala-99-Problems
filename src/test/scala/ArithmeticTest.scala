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
}
