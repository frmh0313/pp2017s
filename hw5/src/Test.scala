package pp201701.hw5test
import pp201701.hw5.Main._
import pp201701.hw5.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  val a = new ComplexNumberImpl(true, 0, 1)
  val b = new ComplexNumberImpl(true, 3, 4)
  val c = new ComplexNumberImpl(false, 1, scala.math.Pi)

  print_result(doubleEqFunc(a.magnitude, 1))
  print_result(doubleEqFunc(a.angle, 1.5708))
  print_result(doubleEqFunc(b.magnitude, 5))
  print_result(doubleEqFunc(c.real, -1))

  print_result(a.add(b).eqFunc(new ComplexNumberImpl(true, 3, 5)))
  print_result(a.mult(b).eqFunc(new ComplexNumberImpl(true, -4, 3)))

  val one = new ComplexNumberImpl(true, 1, 0)

  val t1 = (new ComplexNumberImpl(true, 10, 0)).eval(List(one, one, one))
  print_result(t1.eqFunc(new ComplexNumberImpl(true, 111, 0)))
  //(1 + x + x^2)[x := 10] gives 111
}
