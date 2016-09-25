/**
  * Created by Psycho7 on 9/26/16.
  */
import org.scalatest._

class ListsTest extends FlatSpec {
  import Lists._

  "A list" should "return its last element" in {
    val list = List(1, 1, 2, 3, 5, 8)
    assert(P01.last(list) == 8)
  }
}
