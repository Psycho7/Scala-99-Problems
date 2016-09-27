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

  object P14 {
    def duplicate[A](ls: List[A]): List[A] = ls flatMap { x => List(x, x) }
  }

  object P15 {
    def duplicateN[A](n: Int, ls: List[A]): List[A] = ls flatMap { List.fill(n)(_) }
  }

  object P16 {
    def drop[A](n: Int, ls: List[A]): List[A] = {
      def loop(idx: Int, res: List[A], acc: List[A]): List[A] = res match {
        case Nil => acc.reverse
        case x :: xs if (idx + 1) % n != 0 => loop(idx + 1, xs, x :: acc)
        case _ :: xs => loop(idx + 1, xs, acc)
      }

      loop(0, ls, Nil)
    }
  }

  object P17 {
    def split[A](n: Int, ls: List[A]): (List[A], List[A]) = {
      def loop(curN: Int, curL: List[A], take: List[A]): (List[A], List[A]) = (curN, curL) match {
        case (0, _) => (take.reverse, curL)
        case (_, Nil) => (take.reverse, Nil)
        case (_, x :: xs) => loop(curN - 1, xs, x :: take)
      }

      loop(n, ls, Nil)
    }
  }

  object P18 {
    def slice[A](from: Int, until: Int, ls: List[A]): List[A] = {
      def loop(curN: Int, curL: List[A], result: List[A]): List[A] = curL match {
        case _ :: xs if curN < from => loop(curN + 1, xs, result)
        case x :: xs if curN < until => loop(curN + 1, xs, x :: result)
        case _ => result.reverse
      }

      loop(0, ls, Nil)
    }
  }

  object P19 {
    def rotate[A](n: Int, ls: List[A]): List[A] = {
      val shift = if (ls.isEmpty) 0 else n % ls.length
      val left = if (shift > 0) shift else ls.length + shift
      val (l, r) = (ls take left, ls drop left)
      r ::: l
    }
  }

  object P20 {
    def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
      def loop(curN: Int, curL: List[A], result: List[A]): (List[A], A) = curL match {
        case x :: xs if curN == n => (result.reverse ::: xs, x)
        case x :: xs => loop(curN + 1, xs, x :: result)
        //case _ => result.reverse
      }

      if (n < 0 || n >= ls.length) throw new NoSuchElementException
      else loop(0, ls, Nil)
    }
  }

  object P21 {
    def insertAt[A](item: A, n: Int, ls: List[A]): List[A] = {
      val (pre, next) = (ls take n, ls drop n)
      pre ::: (item :: next)
    }
  }

  object P22 {
    def range(from: Int, to: Int): List[Int] = {
      def loop(curN: Int, result: List[Int]): List[Int] = {
        if (curN < from) result
        else loop(curN - 1, curN :: result)
      }

      loop(to, Nil)
    }
  }

  object P23 {
    import P20.removeAt

    def randomSelect[A](n: Int, ls: List[A]): List[A] =
      if (n <= 0) Nil
      else {
        val (res, item) = removeAt((new util.Random).nextInt(ls.length), ls)
        item :: randomSelect(n - 1, res)
      }
  }

  object P24 {
    import P22.range
    import P23.randomSelect

    def lotto(n: Int, m: Int): List[Int] = randomSelect(n, range(1, m))
  }

  object P25 {
    import P23.randomSelect

    def randomPermute[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)
  }
}
