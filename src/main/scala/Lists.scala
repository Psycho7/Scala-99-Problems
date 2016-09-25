import java.io.OutputStream

/**
  * Created by Psycho7 on 9/26/16.
  */
object Lists {
  object P01 {
    def last(list: List[Int]): Int = list match {
      case x :: Nil => x
      case _ :: xs => last(xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P02 {
    def penultimate(list: List[Int]): Int = list match {
      case x :: _ :: Nil => x
      case _ :: xs => penultimate(xs)
      case _ => throw new NoSuchElementException
    }
  }

  object P03 {
    def nth(n: Int, list: List[Int]): Int = list match {
      case x :: _ if n == 0 => x
      case _ :: xs => nth(n - 1, xs)
      case _ => throw new NoSuchElementException
    }
  }
}
