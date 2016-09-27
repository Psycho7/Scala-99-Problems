import java.io.OutputStream

/**
  * Created by Psycho7 on 9/26/16.
  */
object Lists {
  object P01 {
    def last[A](ls: List[A]): A = ls match {
      case x :: Nil => x
      case _ :: xs => last(xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P02 {
    def penultimate[A](ls: List[A]): A = ls match {
      case x :: _ :: Nil => x
      case _ :: xs => penultimate(xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P03 {
    def nth[A](n: Int, ls: List[A]): A = ls match {
      case x :: _ if n == 0 => x
      case _ :: xs => nth(n - 1, xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P04 {
    def length[A](ls: List[A]): Int = {
      def loop(n: Int, ls: List[A]): Int = ls match {
        case Nil => n
        case _ :: xs => loop(n + 1, xs)
      }

      loop(0, ls)
    }
  }

  object P05 {
    def reverse[A](ls: List[A]): List[A] = {
      def loop(acc: List[A], res: List[A]): List[A] = res match {
        case Nil => acc
        case x :: xs => loop(x :: acc, xs)
      }

      loop(Nil, ls)
    }
  }

  object P06 {
    def isPalindrome[A](ls: List[A]): Boolean = ls == P05.reverse(ls)
  }

  object P07 {
    def flatten(ls: List[Any]): List[Any] = (ls map {
      case xs : List[_] => flatten(xs)
      case x => List(x)
    }).foldLeft(Nil: List[Any])(_ ++ _)
  }

  object P08 {
    def compress[A](ls: List[A]): List[A] =
      ls.foldRight(List[A]()){ (item, ls) =>
        if (ls.isEmpty || item != ls.head) item :: ls
        else ls
      }
  }

  object P09 {
    def pack[A](ls: List[A]): List[List[A]] = {
      if (ls.isEmpty) List(List())
      else {
        val (dups, res) = (
          ls takeWhile {
            _ == ls.head
          },
          ls dropWhile {
            _ == ls.head
          })
        if (res == Nil) List(dups)
        else dups :: pack(res)
      }
    }
  }

  object P10 {
    def encode[A](ls: List[A]): List[(Int, A)] = P09.pack(ls) map {
      x => (x.length, x.head)
    }
  }

  object P11 {
    def encodeModified[A](ls: List[A]): List[Any] = P10.encode(ls) map {
      case (1, x) => x
      case x => x
    }
  }

  object P12 {
    def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap {
      case (n, x) => List.fill(n)(x)
    }
  }

  object P13 {
    def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
      if (ls.isEmpty) Nil
      else {
        val (dups, res) = ls span { _ == ls.head }
        (dups.length, dups.head) :: encodeDirect(res)
      }
    }
  }
}
