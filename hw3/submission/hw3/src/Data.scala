package pp201701.hw3.Data

object DataBundle {
  sealed abstract class MyList[A]
  case class MyNil[A]() extends MyList[A]
  case class MyCons[A](hd: A, tl: MyList[A]) extends MyList[A]

  type Stack[A] = MyList[A]
  type Queue[A] = (Stack[A], Stack[A])

  sealed abstract class BTree[A]
  case class Leaf[A]() extends BTree[A]
  case class Node[A](value: A, left: BTree[A], right: BTree[A]) extends BTree[A]

  type BSTree[K, V] = BTree[(K, V)]
}