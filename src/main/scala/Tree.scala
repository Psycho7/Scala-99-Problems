/**
  * Created by Psycho7 on 10/11/16.
  */
object Tree {

  sealed abstract class Tree[+T] {
    // P56
    def isSymmetric: Boolean
    def isMirrorOf[V](that: Tree[V]): Boolean

    // P57
    def addValue[U >: T <% Ordered[U]](v: U): Tree[U]
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    // P56
    def isSymmetric = left isMirrorOf right

    def isMirrorOf[V](that: Tree[V]): Boolean = that match {
      case Node(_, l, r) => (left isMirrorOf r) && (right isMirrorOf l)
      case End => false
    }

    // P57
    def addValue[U >: T <% Ordered[U]](v: U): Tree[U] =
      if (v < value) Node(value, left.addValue(v), right)
      else Node(value, left, right.addValue(v))
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    // P56
    def isSymmetric = true

    def isMirrorOf[V](that: Tree[V]) = that match {
      case End => true
      case _ => false
    }

    // P57
    def addValue[U <% Ordered[U]](v: U): Tree[U] = Node(v)
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

  // P57
  def fromList[T <% Ordered[T]](ls: List[T]): Tree[T] = {
    def loop(res: Tree[T], ls: List[T]): Tree[T] = ls match {
      case Nil => res
      case x :: xs => loop(res.addValue(x), xs)
    }

    loop(End, ls)
  }
}