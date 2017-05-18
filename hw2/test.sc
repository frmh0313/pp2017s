sealed abstract class IList
case class INil() extends IList
case class ICons(hd: Int, tl: IList) extends IList


def map(xs: IList)(f: Int => Int): IList = {
  xs match {
    case INil() => xs
    case ICons(hd, tl) => ICons(f(hd), map(tl)(f))
  }
}

val sample = ICons(1, ICons(2, ICons(3, ICons(4, ICons(5, INil())))))
map(sample)(x=>x+1)


def reverse(xs: IList): IList = {

  def traverse(xs: IList, res: IList): IList = {
    xs match {
      case INil() => res
      case ICons(hd, tl) => traverse(tl, ICons(hd, res))
    }
  }
  traverse(xs, INil())
}


reverse(sample)

sealed abstract class MyList[Int]
case class MyNil[Int]() extends MyList[Int]
case class MyCons[Int](v: Int, tl: MyList[Int]) extends MyList[Int]


def nextPrime(xs: MyList[Int]): Int = {
  def findLast(xs: MyList[Int]) : Int =
    xs match {
      case MyCons(hd, MyNil()) => hd
      case MyCons(hd, tl) => findLast(tl)
    }

  def test(xs: MyList[Int], num: Int): Int =
    xs match {
      case MyNil() => num
      case MyCons(hd, tl) =>
        if (num % hd == 0) test(xs, num+1)
        else test(tl, num)
    }

  test(xs, findLast(xs)+1)
}

nextPrime(MyCons(2, MyNil()))
nextPrime(MyCons(2, MyCons(3, MyNil())))
nextPrime(MyCons(2, MyCons(3, MyCons(5, MyNil()))))
nextPrime(MyCons(2, MyCons(3, MyCons(5, MyCons(7, MyNil())))))