package pp201701.hw7
import pp201701.hw7.Data.DataBundle._
import pp201701.hw7.Data.DynObj._

/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as var, while, for, return, and so on. You may get deduction
      in your points.

 * ** Do not use equivalent built-in features in Scala. The core logic should be
      implemented by you.

 */

object Main {

  object PiDigit {
    // Pi = 3.1415926535..
    val digits = List(1, 4, 1, 5, 9, 2, 6, 5, 3, 5,  8, 9, 7, 9, 3, 2, 3, 8, 4, 6)
  }

  /* Generate a report that contains n items.
   * If is_prime is true, generate a report for primes that starts from 3.
   * Otherwise, generate a report for Pi digits.
   * If n > 20, generate an error report.
   * Use the "Primes" class in "Data.scala". Otherwise you may get deduction.
   */

  class PiKeyValue(val key: Int) {
    val value = PiDigit.digits(key-1)
  }

  implicit def piKeyValProxy: KeyVal[PiKeyValue] = new KeyVal[PiKeyValue] {
    def get_key(a: PiKeyValue): String = " * " + a.key.toString() + "-th digit"
    def get_val(a: PiKeyValue): String = a.value.toString()
  }

  class PiR(val count: Int) {
    def keyValPair = new PiKeyValue(count)
  }

  implicit def piRProxy: Report[PiR] = new Report[PiR] {
    type A = PiKeyValue

    def title(a: PiR) = if (a.count > 20) "<Error: Too big input>\n" else "<Pi " + a.count + ">\n"

    def keyval: KeyVal[A] = piKeyValProxy

    def it: Iterable[PiR, A] = new Iterable[PiR, A] {
      def iter(a: PiR): Dyn2[Iter, A] = {
        def tilNthDigit(n: Int): List[A] = {
          def go(res: List[A], count: Int): List[A] = {
            if (count == n) res else go(res ++ (new PiKeyValue(count + 1) :: Nil), count + 1)
          }
          go(new PiKeyValue(1) :: Nil, 1)
        }
        Dyn2(tilNthDigit(a.count))
      }
    }
  }

  class PrimesKeyValue(val key: Int) {
    val prime: Int = {
      def nthPrime(n: Int): Int = {
        def go(p: Primes, k: Int): Primes =
          if (k <= 1) p else go(p.getNext, k - 1)

        go(new Primes, n).prime
      }

      nthPrime(key)
    }
  }

  implicit def primesKeyValProxy: KeyVal[PrimesKeyValue] = new KeyVal[PrimesKeyValue] {
    def get_key(a: PrimesKeyValue) = " * " + a.key.toString() + "-th prime"
    def get_val(a: PrimesKeyValue) = a.prime.toString()
  }

  class PrimeR(val count: Int) {
    def keyValPair = new PrimesKeyValue(count)
  }

  implicit def primeRProxy: Report[PrimeR] =
    new Report[PrimeR] {
      type A = PrimesKeyValue

      def title(a: PrimeR) = if (a.count > 20) "<Error: Too big input>\n" else "<Primes " + a.count + ">\n"

      def keyval: KeyVal[A] = primesKeyValProxy

      def it: Iterable[PrimeR, A] = new Iterable[PrimeR, A] {
        def iter(a: PrimeR): Dyn2[Iter, A] = {
          def tilNthPrime(n: Int): List[A] = {
            def go(res: List[A], count: Int): List[A] = {
              if (count == n) res else go(res ++ (new PrimesKeyValue(count + 1) :: Nil), count + 1)
            }

            go(new PrimesKeyValue(1) :: Nil, 1)
          }
          Dyn2(tilNthPrime(a.count))
        }
      }
    }

  def get_report(is_prime: Boolean, n: Int): Dyn[Report] = {
    if (is_prime) Dyn(new PrimeR(n))(primeRProxy)
    else Dyn(new PiR(n))(piRProxy)
  }

  /* Check "Test.scala" for the exact format. */
  def pretty_printer(r: Dyn[Report]): String = {
    val reportProxy = r.i
    val kvProxy = reportProxy.keyval

    def printElements[I](xs: I, res: String)(implicit proxy: Iter[I, reportProxy.A]): String =
      proxy.getValue(xs) match {
        case None => res
        case Some(n) =>
          val str = kvProxy.get_key(n) + " : " + kvProxy.get_val(n) + "\n"
          printElements(proxy.getNext(xs), res + str)
      }

    def printElements2[R](xs: R, res: String)(implicit proxy: Iterable[R, reportProxy.A]): String = {
      val cs = proxy.iter(xs)
      printElements(cs.d, "")(cs.i)
    }

    val title = r.i.title(r.d)
    title match {
      case "<Error: Too big input>\n" => title
      case "<Pi 0>\n" | "<Primes 0>\n" => title
      case _ =>
        val numbers = printElements2(r.d, "")(reportProxy.it)
        title + numbers
      }
    }
  }
