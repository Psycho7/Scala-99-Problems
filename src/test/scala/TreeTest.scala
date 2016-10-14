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
}