object Multiwaytree {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    override def toString: String = value.toString + children.map(_.toString).mkString("") + "^"
    def nodeCount: Int = children match {
      case List() => 1
      case _ => children.map(_.nodeCount).sum + 1
    }
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())

    implicit def str2MTree(str: String): MTree[Char] = {
      def boundary(pos: Int, sum: Int): Int =
        if (sum == 0) pos
        else boundary(pos + 1, if (str(pos) == '^') sum - 1 else sum + 1)
      def childStrings(pos: Int): List[String] =
        if (pos + 1 >= str.length) Nil
        else {
          val bound = boundary(pos + 1, 1)
          str.substring(pos, bound - 1) :: childStrings(bound)
        }
      MTree(str(0), childStrings(1).map(str2MTree _))
    }
  }
}