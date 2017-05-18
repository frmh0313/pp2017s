package pp201701.hw4test
import pp201701.hw4.Main._
import pp201701.hw4.Data.DataBundle._
import pp201701.hw4.Main.Problem1._
import pp201701.hw4.Main.Problem2._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1
  {
    val intEq: Int => Int => Boolean = { x => y => (x == y) }
    val d0 = Problem1.IterDictImpl.empty[Int, Int](intEq)
    val d1 = d0.add(1, 2)

    print_result(
      d1.find(1) match {
        case Some(n) => n == 2
        case _ => false
      }
    )
  }

  // Problem 2
  {
    val x = List("A", "B", "C")
    val bx = new Problem2.BiIterableList(x)

    print_result(
      bx.biIter.getValue match {
        case Some(x) => x == "A"
        case _ => false
      }
    )
  }
}
