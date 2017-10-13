object Multiwaytree {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    override def toString: String = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
    def nodeCount: Int = children match {
      case List() => 1
      case _ => children.map(_.nodeCount).sum + 1
    }
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())
    // def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
  }
}