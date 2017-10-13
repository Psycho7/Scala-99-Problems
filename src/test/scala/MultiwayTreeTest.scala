import org.scalatest._

class MultiwayTreeTest extends FlatSpec {
  import Multiwaytree._

  // P70C
  "nodeCount" should "count the nodes of a multiway tree" in {
    assert(MTree('a').nodeCount == 1)
    assert(MTree('a', List(MTree('f'))).nodeCount == 2)
  }
}
