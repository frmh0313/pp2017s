package pp201701.hw5
import pp201701.hw5.Data.DataBundle._

object Main extends App {

  /*
   Implement traits for complex number addition/multiplication.
   */
  trait ComplexNumberAdd extends ComplexNumberSpec {
    def add(that: ComplexNumberSpec): ComplexNumberSpec =
      this.makeRectangular(this.real + that.real, this.imaginary + that.imaginary)
  }

  trait ComplexNumberMult extends ComplexNumberSpec {
    def mult(that: ComplexNumberSpec): ComplexNumberSpec = {
      val a: Double = this.real
      val b: Double = this.imaginary
      val c: Double = that.real
      val d: Double = that.imaginary

      this.makeRectangular(a * c - b * d, a * d + b * c)
    }
  }

  /*
   Spec of eval
   Input: A polynomial, represented in a list of coefficients.
          For example, (1, 0, i, -1) = 1 + (0 * x) + (i * x^2) + (-1 * x^3)
          Nil means 0
   Output: Calculated result of given polynomial with "this" complex number.
          For example, if (1 + 0*i) calls "eval(1, 0, i, -1)", then output is:
          1 + (0 * (1 + 0*i)) + (i * (1 + 0*i)^2) + (-1 * (1 + 0*i)^3)
          = 1 + 0 + i - 1
          = 0 + i
   */
  trait ComplexNumberEval
    extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult {

    def eval(coeffs: List[ComplexNumberEval]): ComplexNumberSpec = {

      def exponent(n: Int): ComplexNumberSpec =
        if (n == 0) this.makeRectangular(1, 0)
        else this.mult(exponent(n - 1))

      def binarySum(a: ComplexNumberSpec, b: ComplexNumberSpec): ComplexNumberSpec =
        this.makeRectangular(a.real + b.real, a.imaginary + b.imaginary)

      def go(coeffs: List[ComplexNumberEval], count: Int): ComplexNumberSpec =
        coeffs match {
          case Nil => this.makeRectangular(0, 0)
          case hd :: tl => binarySum(hd.mult(exponent(count)), go(tl, count + 1))
        }

      go(coeffs, 0)
    }
  }

  /*
   Implement each missing fields so that it compiles.
   For constructor,
     if "isRectangular" is true,
       use "arg1" as "real" and "arg2" as "imaginary" in rectangular representation.
     else,
       use "arg1" as "magnitude" and "arg2" as "angle" in polar representation.

   * Note: The "angle" value should always be normalized.

   */
  class ComplexNumberImpl(isRectangular: Boolean, arg1: Double, arg2: Double)
    extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult
      with ComplexNumberEval {

    val real: Double =
      if (isRectangular) arg1
      else ComplexNumberSpec.polarToRectangular(arg1, arg2)._1

    val imaginary: Double =
      if (isRectangular) arg2
      else ComplexNumberSpec.polarToRectangular(arg1, ComplexNumberSpec.normalizeAngle(arg2))._2

    val magnitude: Double =
      if (isRectangular) ComplexNumberSpec.rectangularToPolar(arg1, arg2)._1
      else arg1

    val angle: Double =
      if (isRectangular) ComplexNumberSpec.rectangularToPolar(arg1, arg2)._2
      else ComplexNumberSpec.normalizeAngle(arg2)

    override def makeRectangular(real: Double, imaginary: Double): ComplexNumberSpec =
      new ComplexNumberImpl(true, real, imaginary)

    override def makePolar(magnitude: Double, angle: Double): ComplexNumberSpec =
      new ComplexNumberImpl(false, magnitude, angle)
  }

}
