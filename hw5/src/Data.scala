package pp201701.hw5.Data
import scala.math._
import scala.annotation.tailrec

object DataBundle {
  def doubleEqFunc(x: Double, y: Double) = (x-y).abs <= 1E-3

  /*
   "ComplexNumberSpec" describes complex numbers, such as 5 + 2i.
   Complex number can be represented in multiple forms:
     1. rectangular form, of "real" and "imaginary" numbers
     2. polar form, of "magnitude" and "angle"
   In this assignment, "angle" field is radian,
     and its value *must* be inside (-Pi, Pi] in your implementation.
   For more information, refer to: https://en.wikipedia.org/wiki/Complex_number
   */
  trait ComplexNumberSpec {
    val real: Double
    val imaginary: Double
    val magnitude: Double
    val angle: Double

    override def toString = {
      def formatDouble(x: Double): String =
        if(x < 0) "%.4f".format(x)
        else "+%.4f".format(x)
      s"Rectangular Form: (${formatDouble(real)}, ${formatDouble(imaginary)}), " +
      s"Polar Form: (${formatDouble(magnitude)}, ${formatDouble(angle)})"
    }

    def eqFunc(that: ComplexNumberSpec) = {
      doubleEqFunc(real, that.real) &&
      doubleEqFunc(imaginary, that.imaginary) &&
      doubleEqFunc(magnitude, that.magnitude) &&
      (doubleEqFunc(magnitude, 0) || doubleEqFunc(angle, that.angle))
    }

    // Use this for "constructing" new complex number objects.
    // ( This design is weird indeed, but please bear with it for now. )
    def makeRectangular(real: Double, imaginary: Double): ComplexNumberSpec

    def makePolar(magnitude: Double, angle: Double): ComplexNumberSpec
  }

  object ComplexNumberSpec {
    def rectangularToPolar(real: Double, imaginary: Double) = {

      val (x, y) = (real, imaginary)
      val magnitude = sqrt(pow(x, 2) + pow(y, 2))
      val angle: Double = {
        //from wikipedia
        if(x > 0) atan(y/x)
        else if(x < 0 && y >= 0) atan(y/x) + Pi
        else if(x < 0 && y < 0) atan(y/x) - Pi
        else if(x == 0 && y > 0) Pi/2
        else if(x == 0 && y < 0) -Pi/2
        else 0 //(x == 0 && y == 0), Indeterminate value. Let's just pick 0.
      }
      (magnitude, angle)
    }

    def polarToRectangular(magnitude: Double, angle: Double) = {
      (magnitude * cos(angle), magnitude * sin(angle))
    }

    @tailrec
    def normalizeAngle(x: Double): Double =
      if(x > Pi) normalizeAngle(x - 2*Pi)
      else if(x <= -Pi) normalizeAngle(x + 2*Pi)
      else x
  }
}
