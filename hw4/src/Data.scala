package pp201701.hw4.Data

object DataBundle {
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
}
