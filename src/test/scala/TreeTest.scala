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
}