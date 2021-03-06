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

  "removeAt method" should "remove the Nth element" in {
    import P20.removeAt
    val x = removeAt(1, List('a, 'b, 'c, 'd))
    assert(x == (List('a, 'c, 'd), 'b))
  }

  it should "throw NoSuchElementException if there's no Nth element" in {
    import P20.removeAt
    assertThrows[NoSuchElementException] {
      removeAt(-1, List(1, 2, 3))
    }
    assertThrows[NoSuchElementException] {
      removeAt(3, List(1, 2, 3))
    }
  }

  "insertAt method" should "insert a element in a given position" in {
    import P21.insertAt
    val x = insertAt('new, 1, List('a, 'b, 'c, 'd))
    assert(x == List('a, 'new, 'b, 'c, 'd))
    val y = insertAt('new, 0, List('a, 'b, 'c, 'd))
    assert(y == List('new, 'a, 'b, 'c, 'd))
  }

  "range method" should "create a list containing all integers within a given range" in {
    import P22.range
    val x = range(4, 9)
    assert(x == List(4, 5, 6, 7, 8, 9))
  }

  // TODO: God damn it... What should I do to test a random function (P23, P24, P25)?

  "combination method" should "generate combinations" in {
    import P26.combination
    val x = combination(1, List(1, 2, 3))
    assert(x == List(List(1), List(2), List(3)))
    val y = combination(2, List(1, 2, 3))
    assert(y == List(List(1, 2), List(1, 3), List(2, 3)))
    val z = combination(3, List(1, 2, 3))
    assert(z == List(List(1, 2, 3)))
    val zero = combination(0, List(1, 2, 3))
    assert(zero == List(Nil))
  }

  "group method" should "group the elements of a set into disjoint subsets." in {
    import P27.group
    val result = group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    assert(result.contains(List(List("Aldo", "Beat"), List("Carla", "David"), List("Evi", "Flip", "Gary", "Hugo", "Ida"))))
    assert(result.contains(List(List("Aldo", "Carla"), List("Beat", "David"), List("Evi", "Flip", "Gary", "Hugo", "Ida"))))
  }

  "lsort method" should "sort by length" in {
    import P28.lsort
    val result = lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    assert(result == List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
  }

  "lsortFreq method" should "sort by frequency" in {
    import P28.lsortFreq
    val result = lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    assert(result == List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
  }
}
