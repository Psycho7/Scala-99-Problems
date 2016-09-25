/**
  * Created by Psycho7 on 9/26/16.
  */
object Lists {
  object P01 {
    def last(list: List[Int]): Int = list match {
      case Nil => throw new NoSuchElementException
      case x :: Nil => x
      case _ :: xs => last(xs)
    }
  }
}
