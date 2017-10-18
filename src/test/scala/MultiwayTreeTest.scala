import org.scalatest._

class MultiwayTreeTest extends FlatSpec {
  import Multiwaytree._

  // P70C
  "nodeCount" should "count the nodes of a multiway tree" in {
    assert(MTree('a').nodeCount == 1)
    assert(MTree('a', List(MTree('f'))).nodeCount == 2)
  }

  // P70
  "str2MTree" should "convert string to MTree" in {
    val tree = MTree('a',
      List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
    val str = "afg^^c^bd^e^^^"
    assert(tree.toString == str)
    assert(MTree.str2MTree(str).toString == str)
  }
}
