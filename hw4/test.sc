
abstract class Iter[A] {
  def getValue: Option[A]
  def getNext: Iter[A]
}

// Problem 1
abstract class IterDict[K, V] extends Iter[(K, V)] {
  def add(k: K, v: V) : IterDict[K, V]
  def find(k: K) : Option[V]
}

// Problem 2
abstract class BiIter[A] extends Iter[A] {
  def getNext: BiIter[A]
  def getPrev: BiIter[A]
}

abstract class BiIterable[A] {
  def biIter: BiIter[A]
}

class ListBiIter[A](val prev: List[A])(val next: List[A])(val pointer: Int) extends BiIter[A] {
  def getValue = {
    if (pointer < 0) None
    else if (pointer == 0) next.headOption
    else None
  }
  def getPrev : ListBiIter[A] =
    prev match {
      case Nil =>
        next match {
          case Nil => new ListBiIter(Nil)(Nil)(0)
          case _ => new ListBiIter(prev)(next)(pointer -1)
        }
      case hd:: tl =>
        if (pointer > 0) new ListBiIter(prev)(next)(pointer-1)
        else new ListBiIter(tl)(hd::next)(pointer)
      //          next match {
      //            case Nil => new ListBiIter(tl)(hd::next)(pointer)
      //            case l =>
      //          }
    }

  def getNext: ListBiIter[A] =
    next match {
      case Nil =>
        prev match {
          case Nil => new ListBiIter(Nil)(Nil)(0)
          case _ => new ListBiIter(prev)(next)(pointer+1)
        }
      case hd::tl =>
        if (pointer < 0) new ListBiIter(prev)(next)(pointer+1)
        else new ListBiIter(hd::prev)(tl)(pointer)
    }

//  def getPrev : ListBiIter[A]=
//    prev match {
//      case Nil => new ListBiIter(prev)(next)(pointer-1)
//      case hd :: tl => new ListBiIter(tl)(hd::next)(pointer)
//    }
//
//  def getNext : ListBiIter[A] =
//    next match {
//      case Nil => new ListBiIter(prev)(next)(pointer+1)
//      case hd :: tl => new ListBiIter(hd::prev)(tl)(pointer)
//    }
}
class BiIterableList[A](val data: List[A]) extends BiIterable[A] {
  def biIter: BiIter[A] = new ListBiIter(Nil)(data)(0)
}

sealed abstract class BiIterableTree[A] extends BiIterable[A] {
  // You can write something here
  def biIter : ListBiIter[A]
}

case class Empty[A]() extends BiIterableTree[A] {
  def biIter = new ListBiIter(Nil)(Nil)(0)
}

case class Node[A](value: A, left: BiIterableTree[A], right: BiIterableTree[A])
  extends BiIterableTree[A] {
  def biIter = new ListBiIter(Nil)(value::(left.biIter.next++right.biIter.next))(0)
}

def print_result(b:Boolean) : Unit =
  if (b) println("O") else println("X")


//def printBiIter[A](bi: ListBiIter[A]) : Option[A] =
//bi.next match {
//  case Nil => None
//  case _ => {
//    println(bi.getValue)
//    printBiIter(bi.getNext)
//  }
//}

def printBiIter[A](b: ListBiIter[A]) : Option[A] =
b.next match {
  case Nil => { println(None)
    None }
//  case hd :: Nil => Some(hd)
  case hd :: tl => {
    println(Some(hd))
    printBiIter(new ListBiIter(Nil)(tl)(0))
  }
}

println("BiIterableTree")
val w = Node[String]("A", Node("B", Empty(), Empty()), Node("C", Empty(), Empty()))

printBiIter(w.biIter)
printBiIter(w.biIter.getNext)
printBiIter(w.biIter.getPrev)
printBiIter(w.biIter.getPrev.getNext)
printBiIter(w.biIter.getNext.getPrev)

print("1: ")
//printBiIter(w.biIter.getValue)
print_result(w.biIter.getValue == Some("A"))
print("2: ")
printBiIter(w.biIter.getPrev)
print_result(w.biIter.getPrev.getValue == None)
print("3: ")
printBiIter(w.biIter.getPrev.getNext)
print_result(w.biIter.getPrev.getNext.getValue == Some("A"))
println(w.biIter.getPrev.getNext.getValue)
print("4: ")
print_result(w.biIter.getNext.getValue == Some("B"))
print("5: ")
print_result(w.biIter.getNext.getNext.getValue == Some("C"))
print("6: ")
print_result(w.biIter.getNext.getNext.getNext.getValue == None)
print("7: ")
print_result(w.biIter.getNext.getNext.getNext.getPrev.getValue == Some("C"))