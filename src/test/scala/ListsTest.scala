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

  "nth method" should "return the nth element of a list" in {
    assert(P03.nth(0, List(1, 1, 2, 3, 5, 8)) == 1)
    assert(P03.nth(2, List(1, 1, 2, 3, 5, 8)) == 2)
    assert(P03.nth(5, List(1, 1, 2, 3, 5, 8)) == 8)
  }

  it should "throw NoSuchElementException if a list does not long enough" in {
    assertThrows[NoSuchElementException] {
      P03.nth(5, List(1, 2, 3, 4))
    }
  }

  "length method" should "return the length of a list" in {
    assert(P04.length(List(1, 2, 3 ,4)) == 4)
    assert(P04.length(List()) == 0)
  }

  "reverse method" should "return a reversed list" in {
    val ori = List(1, 1, 2, 3, 5, 8)
    assert(P05.reverse(ori) == List(8, 5, 3, 2, 1, 1))
  }

  "isPalindrome method" should "check if a list is palindrome" in {
    assert(P06.isPalindrome(List(1)))
    assert(P06.isPalindrome(List(1, 2, 1)))
    assert(!P06.isPalindrome(List(1, 2, 3)))
  }

  "flatten method" should "flatten a nested list structure" in {
    val ori = List(List(1, 1), 2, List(3, List(5, 8)))
    assert(P07.flatten(ori) == List(1, 1, 2, 3, 5, 8))
  }

  "compress method" should "eliminate consecutive duplicates of elements" in {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(P08.compress(list) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  "pack method" should "pack consecutive duplicates of list elements into sublists" in {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val ans = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    assert(P09.pack(ls) == ans)
  }

  it should "create an empty sublist if a list is empty" in {
    assert(P09.pack(List()) == List(List()))
  }

  "encode method" should "count run-length encoding of a list" in {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(P10.encode(ls) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "encodeModified method" should "modify the result of encode method" in {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(P11.encodeModified(ls) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  "decode method" should "decode the result of encode method" in {
    val c = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    val p = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(P12.decode(c) == p)
  }

  "encodeDirect method" should "count run-length encoding of a list" in {
    val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(P13.encodeDirect(ls) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "duplicate method" should "duplicate elements" in {
    val ls = List('a, 'b, 'c, 'c, 'd)
    assert(P14.duplicate(ls) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "duplicateN method" should "duplicate elements in a given number times" in {
    val ls = List('a, 'b, 'c, 'c, 'd)
    assert(P15.duplicateN(3, ls) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "drop method" should "drop every Nth elements" in {
    val x = P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(x == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "split method" should "split a list into two parts" in {
    val x = P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(x == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    val y = P17.split(5, List(1, 2, 3, 4, 5))
    assert(y == (List(1, 2, 3, 4, 5), Nil))
  }

  "slice method" should "extract a slice from a list" in {
    val x = P18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(x == List('d, 'e, 'f, 'g))
  }

  "rotate method" should "rotate a list N places to the left" in {
    import P19.rotate
    val x = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(x == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    val y = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(y == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
}
