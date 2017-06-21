Dyn --> Primes

Dyn[Report] {
  type Data
  val d : Data
  val i : Report[Data]
}

object Dyn {
  implicit def apply[Primes, Report[_]](dd: Primes)(implicit ii: Report[Primes]): Dyn[Report] =
    new Dyn[Report] {
      type Data = Primes
      val d = dd
      val i = ii
    }

    implicit def methods[Report[_]](d: Dyn[Report]): Report[Primes] = d.i
}


def primeReport : Report[Primes] = new Report[Primes] {
type A = Int
  def title(r: Primes): String
  def keyVal: KeyVal[A]
  def it: Iterable[Primes, Int] = {
    def iter(a: Primes): Dyn2[Iter, Int] // data, method가 다 있어야 함. List로 만들기??
  }
}

Dyn2[Iter, Int] {
  type Data
  val d: Data
  val i: Iter[Data, Int]
}

object Dyn2 {
  implicit def apply[Iter, List[Int], Int](dd: List[Int])(implicit ii: Iter[List[Int], A]): Dyn2[Iter, AInt =
    new Dyn2[Iter, Int] {
      type Data = List[Int]
      val d = dd
      val i = ii
    }
    implicit def methods[Iter, Int](d: Dyn2[Iter, Int]): S[d.Data, Int] = d.i
}

전체 구조가 문제일 수도.
Report -> query 하나당, 즉 get_report(is_prime: Boolean, n: Int): Dyn[Report] 한 번 호출시 하나씩.
Iterable[Primes, Int]



Report[R]
R type {
  type A
  def title(a: R): String
  def keyVal : KeyVal[A] --> A type: key, value를 가진 타입
  def it: Iterable[R, A] {
    def iter(a: R): Dyn2[Iter, A]
  }
}


Report[R] --> R type data를 가지고 Report를 만듦.
KeyVal[A] --> A type data를 가지고 keyVal을 만듦.

R, A type data클래스와 typeclass를 따로 만들어야.
R: PrimeR
A: PrimesKeyValue: Primes with counter.

PrimeR, proxy: Report[PrimeR]
PrimesKeyValue, proxy: KeyVal[PrimesKeyValue]

class PrimeR(val title: String, val primes: PrimesKeyValue) {

}
}

Iterable[PrimeR, PrimesKeyValue] ..?
