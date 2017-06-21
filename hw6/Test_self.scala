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



  val a1 = Complex.makeRectangular(0, 1)
  val b1 = Complex.makeRectangular(3, 4)
  val c = Complex.makePolar(1, scala.math.Pi)
  assert(Real.equals(a1.magnitude, 1))
  assert(Real.equals(a1.angle, 1.5708))
  assert(Real.equals(b1.magnitude, 5))
  assert(Real.equals(b1.angle, 0.9273))
  assert(Real.equals(c.real, -1))
  assert(Real.equals(c.imaginary, 0))

  val add = (implicitly[AddOp[Complex]].op) _
  val mult = (implicitly[MultOp[Complex]].op) _
  assert(Complex.equals(add(a1, b1), (Complex.makeRectangular(3, 5))))
  assert(Complex.equals(mult(a1, b1), (Complex.makeRectangular(-4, 3))))
  assert(Complex.equals(add(a1, c), (Complex.makeRectangular(-1, 1))))
  assert(Complex.equals(mult(a1, c), (Complex.makeRectangular(0, -1))))

  val one = Complex.makeRectangular(1, 0)
  val zero = Complex.makeRectangular(0, 0)

  val t1 = Polynomial.eval(List(one, one, one), Complex.makeRectangular(10, 0))
  assert(Complex.equals(t1, Complex.makeRectangular(111, 0)))
  //(1 + x + x^2)[x := 10] gives 111

  val t2 = Polynomial.eval(List(one, zero, one), Complex.makeRectangular(0, 1))
  assert(Complex.equals(t2, Complex.makeRectangular(0, 0)))

  val poly = List(1.0, 1.0, 1.0, 1.0)
  println(implicitly[MultOp[Polynomial[Real]]].op(poly, poly))




  //Some property based test
  //let f(x) := (x - root_1)(x - root_2) ... (x - root_n)
  //then f(root_i) = 0
  import scala.util.Random
  val length = 40
  object RealTest {
    val roots =
      List.fill(length)(Random.nextDouble)
    val add = implicitly[AddOp[Real]]
    val mult = implicitly[MultOp[Real]]
    val polys =
      for(root <- roots) yield {
        List(add.inverse(root), mult.identity)
      }
    val polyMult = implicitly[MultOp[Polynomial[Real]]]
    val multed = polys.foldLeft(polyMult.identity)(polyMult.op)
    for(root <- roots) {
      val evaled = Polynomial.eval(multed, root)
      assert(Real.equals(implicitly[AddOp[Real]].identity, evaled))
    }
  }

  object ComplexTest {
    val roots =
      List.fill(length)(
        if(Random.nextBoolean) Complex.makeRectangular(Random.nextDouble, Random.nextDouble)
        else Complex.makePolar(Random.nextDouble, Random.nextDouble * 2 *scala.math.Pi))
    val add = implicitly[AddOp[Complex]]
    val mult = implicitly[MultOp[Complex]]
    val polys =
      for(root <- roots) yield {
        List(add.inverse(root), mult.identity)
      }
    val polyMult = implicitly[MultOp[Polynomial[Complex]]]
    val multed = polys.foldLeft(polyMult.identity)(polyMult.op)
    for(root <- roots) {
      val evaled = Polynomial.eval(multed, root)
      assert(Complex.equals(implicitly[AddOp[Complex]].identity, evaled))
    }
  }

  object PolynomialEvalExample {
    val realPoly: List[Real] = List(1, 1, 1, 1)
    val realPolyPoly: List[Polynomial[Real]] = List.fill(4)(realPoly)
    //Evaluating realPoly * realPoly^0 + realPoly * realPoly^1 + realPoly * realPoly^2 + realPoly * realPoly^3
    println(Polynomial.eval(realPolyPoly, realPoly))
  }

  RealTest
  ComplexTest
  PolynomialEvalExample
}
