import java.io.OutputStream

/**
  * Created by Psycho7 on 9/26/16.
  */
object Lists {
  object P01 {
    def last[T](list: List[T]): T = list match {
      case x :: Nil => x
      case _ :: xs => last(xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P02 {
    def penultimate[T](list: List[T]): T = list match {
      case x :: _ :: Nil => x
      case _ :: xs => penultimate(xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P03 {
    def nth[T](n: Int, list: List[T]): T = list match {
      case x :: _ if n == 0 => x
      case _ :: xs => nth(n - 1, xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P04 {
    def length[T](list: List[T]): Int = {
      def loop(n: Int, list: List[T]): Int = list match {
        case Nil => n
        case _ :: xs => loop(n + 1, xs)
      }

      loop(0, list)
    }
  }

  object P05 {
    def reverse[T](list: List[T]): List[T] = {
      def loop(acc: List[T], res: List[T]): List[T] = res match {
        case Nil => acc
        case x :: xs => loop(x :: acc, xs)
      }

      loop(Nil, list)
    }
  }

  object P06 {
    def isPalindrome[T](list: List[T]): Boolean = list == P05.reverse(list)
  }

  object P07 {
    def flatten(list: List[Any]): List[Any] = (list map {
      case xs : List[_] => flatten(xs)
      case x => List(x)
    }).foldLeft(Nil: List[Any])(_ ++ _)
  }

  object P08 {
    def compress[T](list: List[T]): List[T] =
      list.foldRight(List[T]()){ (item, ls) =>
        if (ls.isEmpty || item != ls.head) item :: ls
        else ls
      }
  }

  object P09 {
    def pack[T](list: List[T]): List[List[T]] = {
      if (list.isEmpty) List(List())
      else {
        val (dups, res) = (
          list takeWhile {
            _ == list.head
          },
          list dropWhile {
            _ == list.head
          })
        if (res == Nil) List(dups)
        else dups :: pack(res)
      }
    }
  }

  object P10 {
    def encode[T](list: List[T]): List[(Int, T)] = P09.pack(list) map {
      x => (x.length, x.head)
    }
  }

  object P11 {
    def encodeModified[T](list: List[T]): List[Any] = P10.encode(list) map {
      case (1, x) => x
      case x => x
    }
  }

  object P12 {
    def decode[T](list: List[(Int, T)]): List[T] = list flatMap {
      case (n, x) => List.fill(n)(x)
    }
  }

  object P13 {
    def encodeDirect[T](list: List[T]): List[(Int, T)] = {
      if (list.isEmpty) Nil
      else {
        val (dups, res) = list span { _ == list.head }
        (dups.length, dups.head) :: encodeDirect(res)
      }
    }
  }
}
