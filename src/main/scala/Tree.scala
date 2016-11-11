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

    val height: Int

    val nodes: Int

    // P61
    def leafCount: Int

    // P61A
    def leafList: List[T]
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

    val height = 1 + (left.height max right.height)

    val nodes = 1 + left.nodes + right.nodes

    // P61
    def leafCount = this match {
      case Node(_, End, End) => 1
      case _ => left.leafCount + right.leafCount
    }

    // P61A
    def leafList: List[T] = this match {
      case Node(value, End, End) => List(value)
      case _ => left.leafList ::: right.leafList
    }
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

    val height = 0

    val nodes = 0

    // P61
    val leafCount = 0

    // P61A
    val leafList = Nil
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

  // P58
  def symmetricBalancedTrees[T](n: Int, v: T): List[Tree[T]] =
    cBalanced(n, v) filter { _.isSymmetric }

  // P59
  def hbalTrees[T](h: Int, v: T): List[Tree[T]] = h match {
    case 0 => List(End)
    case 1 => List(Node(v))
    case _ =>
      val full = hbalTrees(h - 1, v)
      val less = hbalTrees(h - 2, v)
      val f = for {
        x <- full
        y <- full
      } yield Node(v, x, y)
      val l = for {
        x <- full
        y <- less
        z <- List(true, false)
      } yield if (z) Node(v, x, y) else Node(v, y, x)
      f ::: l
  }

  // P60
  def minHbalNodes(height: Int): Int = height match {
    case n if n < 1 => 0
    case 1 => 1
    case n => minHbalNodes(n - 1) + minHbalNodes(n - 2) + 1
  }

  def maxHbalHeight(nodes: Int): Int =
    Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last

  def hbalTreeWithNodes[T](nodes: Int, v: T): List[Tree[T]] = {
    val minHeight = (Math.log(nodes + 1) / Math.log(2)).toInt
    (minHeight to maxHbalHeight(nodes)).flatMap(hbalTrees(_, v)).filter(_.nodes == nodes).toList
  }

}