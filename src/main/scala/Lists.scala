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
      def loop[T](n: Int, list: List[T]): Int = list match {
        case Nil => n
        case _ :: xs => loop(n + 1, xs)
      }

      loop(0, list)
    }
  }
}
