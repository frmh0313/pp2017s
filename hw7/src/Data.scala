package pp201701.hw7.Data
import scala.language.higherKinds
import scala.language.implicitConversions

object DynObj {
  abstract class Dyn[S[_]] {
    type Data
    val d: Data
    val i: S[Data]
  }

  object Dyn {
    implicit def apply[D,S[_]](dd: D)(implicit ii: S[D]):
        Dyn[S] = new Dyn[S] {
      type Data = D
      val d = dd
      val i = ii
    }
    implicit def methods[S[_]](d: Dyn[S]): S[d.Data] = d.i
  }

  abstract class Dyn2[S[_,_],A] {
    type Data
    val d: Data
    val i: S[Data,A]
  }
  object Dyn2 {
    implicit def apply[S[_, _],D,A](dd: D)(implicit ii: S[D,A]):
        Dyn2[S,A] = new Dyn2[S,A] {
      type Data = D
      val d = dd
      val i = ii
    }
    implicit def methods[S[_, _],A](d: Dyn2[S,A]): S[d.Data,A] = d.i
  }
}

object DataBundle {
  import DynObj._

  abstract class Iter[I,A] {
    def getValue(i: I): Option[A]
    def getNext(i: I): I
  }
  
  abstract class Iterable[R, A] {
    def iter(a:R): Dyn2[Iter, A]
  }

  implicit def listIter[A] : Iter[List[A], A] =
    new Iter[List[A],A] {
      def getValue(a: List[A]) = a.headOption
      def getNext(a: List[A]) = a.tail
    }

  abstract class KeyVal[A] {
    def get_key(a:A) : String
    def get_val(a:A) : String
  }

  abstract class Report[R] {
    type A
    def title(r:R) : String
    def it : Iterable[R, A]
    def keyval : KeyVal[A]
  }

  // Primes
  class Primes private (val prime:Int, protected val primes:List[Int])
  { def this() = this(3, List (3))
    def getNext: Primes = {
      val p = computeNextPrime(prime + 2)
      new Primes(p, primes ++ (p :: Nil ))
    }
    private def computeNextPrime(n: Int) : Int =
      if (primes.forall((p:Int) => n%p != 0)) n
      else computeNextPrime(n+2)
  }
}
