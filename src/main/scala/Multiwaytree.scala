object Multiwaytree {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())

    override def toString: String = value.toString + children.map(_.toString).mkString("") + "^"

    def nodeCount: Int = children match {
      case List() => 1
      case _ => children.map(_.nodeCount).sum + 1
    }

    def internalPathLength: Int = children match {
      case List() => 0
      case ls => ls.map(c => c.internalPathLength + c.nodeCount).sum
    }

    def postorder: List[T] = children match {
      case List() => List(value)
      case ls => ls.flatMap(_.postorder) ::: List(value)
    }

    def lispyTree: String = children match {
      case List() => value.toString
      case ls => s"(${(value.toString :: ls.map(_.lispyTree)).mkString(" ")})"
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

    def fromLispy(lispy: String): MTree[String] = {
      def sumOffset(char: Char) = char match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }

      def nextSep(pos: Int, sum: Int): Int =
        if (sum == 0 && (lispy(pos) == ' ' || lispy(pos) == ')')) pos
        else nextSep(pos + 1, sum + sumOffset(lispy(pos)))

      def splitChildStrings(pos: Int): List[String] =
        if (pos >= lispy.length) Nil
        else {
          val sep = nextSep(pos, 0)
          lispy.substring(pos, sep) :: splitChildStrings(sep + 1)
        }

      if (lispy.length == 0 || lispy(0) != '(') MTree(lispy)
      else {
        val sep = nextSep(1, 0)
        MTree(lispy.substring(1, sep), splitChildStrings(sep + 1).map(fromLispy(_)))
      }
    }
  }

}