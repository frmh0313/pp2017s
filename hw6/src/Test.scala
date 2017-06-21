package pp201701.hw6test
import pp201701.hw6.Main._
import pp201701.hw6.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  val a = Complex.makeRectangular(0, 1)
  print_result(Real.equals(a.magnitude, 1.0))

  val b = Complex.makeRectangular(3, 3)
  val apb = implicitly[AddOp[Complex]].op(a, b)
  print_result(Real.equals(apb.magnitude, 5.0))
}
