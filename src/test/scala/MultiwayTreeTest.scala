import org.scalatest._

class MultiwayTreeTest extends FlatSpec {
  import Multiwaytree.MTree
  import Multiwaytree.MTree._

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

  // P71
  "internalPathLength" should "determine the internal path length of a tree" in {
    assert("a^".internalPathLength == 0)
    assert("afg^^^".internalPathLength == 3)
    assert("afg^^c^bd^e^^^".internalPathLength == 9)
  }

  // P72
  "postorder" should "construct the postorder sequence of the tree nodes" in {
    assert("a^".postorder == List('a'))
    assert("ab^c^d^^".postorder == 'b' :: 'c' :: 'd' :: 'a' :: Nil)
    assert("afg^^c^bd^e^^^".postorder == List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
  }

  // P73
  "lispyTree" should "construct the listy representation of a multiway tree" in {
    assert("a^".lispyTree == "a")
    assert("ab^^".lispyTree == "(a b)")
    assert("abc^^^".lispyTree == "(a (b c))")
    assert("bd^e^^".lispyTree == "(b d e)")
    assert("afg^^c^bd^e^^^".lispyTree == "(a (f g) c (b d e))")
  }

  "fromLispy" should "construct MTree from lispy string" in {
    assert(fromLispy("a").lispyTree == "a")
    assert(fromLispy("(a b)").lispyTree == "(a b)")
    assert(fromLispy("(a (b c))").lispyTree == "(a (b c))")
    assert(fromLispy("(b d e)").lispyTree == "(b d e)")
    assert(fromLispy("(a (f g) c (b d e))").lispyTree == "(a (f g) c (b d e))")
  }
}
