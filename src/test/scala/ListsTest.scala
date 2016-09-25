/**
  * Created by Psycho7 on 9/26/16.
  */
import org.scalatest._

class ListsTest extends FlatSpec {
  import Lists._

  "last method" should "return its last element" in {
    val list = List(1, 1, 2, 3, 5, 8)
    assert(P01.last(list) == 8)
    val list2 = List(5)
    assert(P01.last(list2) == 5)
  }

  it should "throw NoSuchElementException if a list has less than 1 element" in {
    assertThrows[NoSuchElementException] {
      P01.last(List())
    }
  }

  "penultimate method" should "return its last but one element" in {
    val list = List(1, 1, 2, 3, 5, 8)
    assert(P02.penultimate(list) == 5)
    val list2 = List(1, 2)
    assert(P02.penultimate(list2) == 1)
  }

  it should "throw NoSuchElementException if a list has less than 2 elements" in {
    assertThrows[NoSuchElementException] {
      P02.penultimate(List())
    }
    assertThrows[NoSuchElementException] {
      P02.penultimate(List(1))
    }
  }
}
