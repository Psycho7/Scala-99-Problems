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

    // P62
    def internalList: List[T]

    // P62B
    def atLevel(n: Int): List[T]

    // P64
    def layoutBinaryTree = layoutBinaryTreeInternal(1, 1)._1

    def layoutBinaryTreeInternal(firstX: Int, depth: Int): (Tree[T], Int)
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
      case Node(_, End, End) => List(value)
      case _ => left.leafList ::: right.leafList
    }

    // P62
    def internalList: List[T] = this match {
      case Node(_, End, End) => Nil
      case _ => left.internalList ::: List(value) ::: right.internalList
    }

    // P62B
    def atLevel(n: Int): List[T] = if (n < 1) Nil
    else if (n == 1) List(value)
    else left.atLevel(n - 1) ::: right.atLevel(n - 1)

    // P63
    override def layoutBinaryTreeInternal(firstX: Int, depth: Int): (Tree[T], Int) = {
      val (leftTree, myX) = left.layoutBinaryTreeInternal(firstX, depth + 1)
      val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
      (new PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
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

    // P62
    val internalList = Nil

    // P62B
    def atLevel(n: Int) = Nil

    // P63
    override def layoutBinaryTreeInternal(firstX: Int, depth: Int) = (End, firstX)
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  class PositionedNode[+T](override val value: T,
                                override val left: Tree[T],
                                override val right: Tree[T],
                                x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" +
      value.toString + " " + left.toString + " " + right.toString + ")"
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

  // P63
  def completeBinaryTree[T](n: Int, value: T): Tree[T] = {
    def construct(addr: Int): Tree[T] = addr match {
      case x if x > n => End
      case _ => Node(value, construct(addr * 2), construct(addr * 2 + 1))
    }

    construct(1)
  }

}