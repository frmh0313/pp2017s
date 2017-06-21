def listSum(a: List[Int], b: List[Int]): List[Int] =
  a match {
    case Nil => b
    case ha :: ta =>
      b match {
        case Nil => a
        case hb :: tb => (ha + hb) :: listSum(ta, tb)
      }
  }


def elementMultList(e: Int, p: List[Int]): List[Int] =
  p match {
    case Nil => Nil
    case h::tl => e*h :: elementMultList(e, tl)
  }

def op(a: List[Int], b: List[Int]): List[Int] = {
  a match {
    case Nil => Nil
    case ha::ta => listSum(elementMultList(ha, b), op(ta, b))
  }
}

elementMultList(2, List(1, 2))
elementMultList(3, List(1, 2, 3))

op(List(1, 2, 3), List(1))
op(List(1, 2, 3), List(1,2))
op(List(1, 2), Nil)
op(Nil, List(1, 2, 3))
listSum(List(1, 2), List(1, 2))
