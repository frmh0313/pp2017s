package pp201701.hw3test
import pp201701.hw3.Main._
import pp201701.hw3.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1 - stack
  {
    val stk0 : Stack[Int] = emptyStk[Int]
    val stk1 : Stack[Int] = push[Int](stk0)(3)
    val opt_r : Option[(Int,Stack[Int])] = pop(stk1)
    print_result(
      opt_r match {
        case Some((n,_)) => n == 3
        case _ => false
      }
    )
  }

  // Problem 1 - queue
  {
    val q0 : Queue[Int] = emptyQ[Int]
    val q1 : Queue[Int] = enQ(q0)(3)
    val opt_r : Option[(Int, Queue[Int])] = deQ(q1)
    print_result(
      opt_r match {
        case Some((n,_)) => n == 3
        case _ => false
      }
    )
  }

  // Problem 2
  {
    val cmp = (scala.math.Ordering.Int.compare _).curried
    val emptyIntIntBST : BSTree[Int, Int] = emptyBST[Int, Int]
    val insertedBST = insert(emptyIntIntBST)((1, 3))(cmp)
    val opt_r = lookup(insertedBST)(1)(cmp)
    print_result(
      opt_r match {
        case Some(n) => n == 3
        case _ => false
      }
    )
  }

  // Problem 3
  {
    val mc = new MyClass[Int, Int, Int, Int, Int, Int]()

    val obj1: mc.Ty1 = new {
      def apply: { val func: mc.Func1 ; val c: Int } => { val b: Int ; val d: Int } =
        (x) => new { val b = 0 ; val d = 1 }
      val a: Int = 1
      val b: Int = 2
    }

    val obj2: mc.Ty2 = new {
      def apply: { val func: mc.Func2 ; val e: Int } => { val b: Int ; val f: Int } =
        x => new { val b = 1 ; val f = 0}
      val a: Int = 0
      val c: Int = 0
    }

    val obj3: mc.CommonTy = new {
      def apply: {val func: mc.Func3; val c: Int; val e: Int} => {val b: Int} =
      x => new {val b = 3}
      val a: Int = 4
    }
    val r1 = mc(obj1, 0,0,0,0,0,0)
    val r2 = mc(obj2, 0,0,0,0,0,0)
    val r3 = mc(obj3, 0,0,0,0,0,0)

    // We cannot provide test code since it gives the type information of the result,
    // that is highly related to the solution.
    println("Test problem 3 by yourself")
  }
}
