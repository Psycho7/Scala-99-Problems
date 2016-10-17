/**
  * Created by Psycho7 on 10/11/16.
  */
import org.scalatest._

class TreeTest extends FlatSpec {
  import Tree._

  "cBalanced method" should "construct completely balanced binary trees." in {
    val v = 'X'
    val single = Node(v, End, End)
    val one = cBalanced(1, v)
    assert(one == List(single))
    val two = cBalanced(2, v)
    assert(two.contains(Node(v, single, End)) &&
      two.contains(Node(v, End, single)) &&
      two.length == 2)
    val four = cBalanced(4, v)
    assert(four.length == 4)
  }

  "isMirror method" should "check whether two trees are the mirror image of each other" in {
    val l1 = Node(1, Node(1), End)
    val r1 = Node(1, End, Node(1))
    assert(l1 isMirrorOf r1)
    val l2 = Node(1, l1, l1)
    val r2 = Node(1, r1, r1)
    assert(l2 isMirrorOf r2)
    assert(!(l1 isMirrorOf l1))
    assert(!(l1 isMirrorOf r2))
  }

  "isSymmetric method" should "check whether a tree is symmetric" in {
    val l1 = Node(1, Node(1), End)
    val r1 = Node(1, End, Node(1))
    val l2 = Node(1, l1, l1)
    val r2 = Node(1, r1, r1)
    assert(Node(1, l1, r1).isSymmetric)
    assert(Node(1, r1, l1).isSymmetric)
    assert(Node(1, l2, r2).isSymmetric)
    assert(Node(1).isSymmetric)
    assert(!l2.isSymmetric)
  }

  "addValue method" should "add a node to a BST" in {
    val res = End.addValue(2)
    assert(res == Node(2))
    val a = res.addValue(0)
    assert(a == Node(2, Node(0), End))
    val b = a.addValue(3)
    assert(b == Node(2, Node(0), Node(3)))
    val c = b.addValue(1)
    assert(c == Node(2, Node(0, End, Node(1)), Node(3)))
  }

  "fromList method" should "create a BST from a given List" in {
    assert(fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
    assert(!fromList(List(3, 2, 5, 7, 4)).isSymmetric)
  }

  "hbalTrees method" should "construct BSTs of a given height" in {
    val v = 1
    val a = hbalTrees(1, v)
    assert(a == List(Node(v)))
    val b = hbalTrees(2, v)
    assert(b.length == 3)
    assert(b contains Node(v, Node(v), End))
    assert(b contains Node(v, End, Node(v)))
    assert(b contains Node(v, Node(v), Node(v)))
    val c = hbalTrees(3, v)
    assert(c.length == 15)
  }

  "minHbalNodes method" should "find out the minimum number of nodes it can contain" in {
    assert(minHbalNodes(3) == 4)
  }

  "maxHbalHeight method" should "find out the maximum height H a height-balanced binary tree with N nodes can have" in {
    assert(maxHbalHeight(4) == 3)
  }
}