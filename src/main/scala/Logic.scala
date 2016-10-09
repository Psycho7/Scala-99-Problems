/**
  * Created by Psycho7 on 10/8/16.
  */
object Logic {
  object S99Logic {
    def not(a: Boolean) = a match {
      case true => false
      case false => true
    }

    def and(a: Boolean, b: Boolean) = (a, b) match {
      case (true, true) => true
      case _ => false
    }

    def or(a: Boolean, b: Boolean) = (a, b) match {
      case (false, false) => false
      case _ => true
    }

    def equ(a: Boolean, b: Boolean) = (a, b) match {
      case (true, true) => true
      case (false, false) => true
      case _ => false
    }

    def nand(a: Boolean, b: Boolean) = not(and(a, b))

    def nor(a: Boolean, b: Boolean) = not(or(a, b))

    def xor(a: Boolean, b: Boolean) = (a, b) match {
      case (true, false) => true
      case (false, true) => true
      case _ => false
    }

    def impl(a: Boolean, b: Boolean) = or(not(a), b)
  }
}
