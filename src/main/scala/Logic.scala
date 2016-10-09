/**
  * Created by Psycho7 on 10/8/16.
  */
object Logic {
  object S99Logic {

    implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)

    def not(a: Boolean) = a match {
      case true => false
      case false => true
    }

    val tf = List(true, false)

    // P46
    def table2(expr: (Boolean, Boolean) => Boolean): Unit = {
      println("A     B     result")
      for {
        a <- tf
        b <- tf
      } {
        printf("%-5s %-5s %-5s\n", a, b, expr(a, b))
      }
    }

    def gray(n: Int): List[String] =
      if (n == 0) List("")
      else {
        val pre = gray(n - 1)
        pre.map{ "0" + _ } ::: pre.reverse.map{ "1" + _ }
      }
  }

  // P47
  class S99Logic(a: Boolean) {
    import S99Logic._

    def and(b: Boolean) = (a, b) match {
      case (true, true) => true
      case _ => false
    }

    def or(b: Boolean) = (a, b) match {
      case (false, false) => false
      case _ => true
    }

    def equ(b: Boolean) = (a, b) match {
      case (true, true) => true
      case (false, false) => true
      case _ => false
    }

    def nand(b: Boolean) = not(a and b)

    def nor(b: Boolean) = not(a or b)

    def xor(b: Boolean) = (a, b) match {
      case (true, false) => true
      case (false, true) => true
      case _ => false
    }

    def impl(b: Boolean) = not(a) or b
  }
}
