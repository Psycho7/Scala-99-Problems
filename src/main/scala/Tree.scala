/**
  * Created by Psycho7 on 10/11/16.
  */
object Tree {

  sealed abstract class Tree[+T]

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  // P55
  def cBalanced[T](count: Int, value: T): List[Tree[T]] = count match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 =>
      val sub = cBalanced(n / 2, value)
      for {
        x <- sub
        y <- sub
      } yield Node(value, x, y)
    case n if n % 2 == 0 =>
      val half = n / 2
      val subX = cBalanced(half, value)
      val subY = cBalanced(half - 1, value)
      for {
        x <- subX
        y <- subY
        z <- List(true, false)
      } yield if (z) Node(value, x, y) else Node(value, y, x)
  }
}