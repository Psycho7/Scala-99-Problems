/**
  * Created by Psycho7 on 10/8/16.
  */
import org.scalatest._

class LogicTest extends FlatSpec {
  import Logic.S99Logic._

  "gray method" should "create Gray Code" in {
    assert(gray(2) == List("00", "01", "11", "10"))
    assert(gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }
}