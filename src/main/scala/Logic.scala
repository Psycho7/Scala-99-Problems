import scala.collection.mutable

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

    // P49
    def gray(n: Int): List[String] =
      if (n == 0) List("")
      else {
        val pre = gray(n - 1)
        pre.map{ "0" + _ } ::: pre.reverse.map{ "1" + _ }
      }

    // P50
    def huffman(ls: List[(String, Int)]): List[(String, String)] = {
      type DString = (String, String)

      abstract class Tree {
        val freq: Int
        def toCode: List[DString]
        def toCodePrefixed(prefix: String) = toCode map {
          case (sym, code) => (sym, prefix ++ code)
        }
      }

      final case class Fork(left: Tree, right: Tree) extends Tree {
        val freq = left.freq + right.freq
        def toCode: List[DString] = left.toCodePrefixed("0") ++ right.toCodePrefixed("1")
      }

      final case class Node(sym: String, freq: Int) extends Tree {
        def toCode: List[DString] = List((sym, ""))
      }

      object TreeOrdering extends Ordering[Tree] {
        def compare(a: Tree, b: Tree) = b.freq compare a.freq
      }

      import scala.collection.mutable
      def loop(queue: mutable.PriorityQueue[Tree]): Tree = {
        if (queue.length == 1) queue.dequeue
        else {
          val a, b = queue.dequeue
          //val b = queue.dequeue
          queue.enqueue(Fork(a, b))
          loop(queue)
        }
      }

      val queue = mutable.PriorityQueue[Tree](ls map { case (sym, freq) => Node(sym, freq) }: _*)(TreeOrdering)
      loop(queue).toCode.sortBy{ case (sym, _) => sym }
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
