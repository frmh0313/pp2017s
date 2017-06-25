package pp201701.projtest
import pp201701.proj.Main._
import pp201701.proj.Value._
import pp201701.proj.Data.DataBundle._
import pp201701.proj.Data.DataBundle.ExprToString._
import pp201701.proj.Lexer._
import pp201701.proj.Parser._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  def run_eval(eval: Expr => Val)(code:String) : Val = {
    val tokens = ProjLexer(code)
    val e:Expr = Parser(tokens)
    println(e)
    eval(e)
  }

  def run_myeval = run_eval(myeval) _
  def run_myeval_memo = run_eval(myeval_memo) _

  def run_test(implicit conv:ConvertToScala[Val]) = {
    try {
      println("=================")
      println("1. Basic Test")

      { // 1
        val code = "(hd (cons 1 2))"
        //        println("1: "+code)
        val res = conv.toInt(run_myeval(code)) match {
          case Some(1) => true
          case _ => false
        }
        //        println("1: "+conv.toInt(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: Some(1)")
        print_result(res)
        //        println("\n")
      }

      { // 2
        val code = "(let ((val p (cons 1 (cons true nil)))) (cons 0 p))"
        //        println("2: "+code)
        val res = conv.toPair(run_myeval(code)) match {
          case Some(_) => true // this only checks whether the result is a pair.
          case _ => false
        }
        //        println("2: "+ conv.toPair(run_myeval(code)) +" "+ { if (res == true) "O" else "X"} + " answer: Some(_)")
        print_result(res)
        //        println("\n")
      }

      { // 3
        val code = "(if true 10 20)"
        //        println("3: "+code)
        val res = conv.toInt(run_myeval(code)) match {
          case Some(10) => true
          case _ => false
        }
        //        println("3: "+ conv.toInt(run_myeval(code)) + " " + { if (res == true) "O" else "X"} + " answer: Some(10)")
        print_result(res)
        //        println("\n")
      }

      { // 4
        val code = "((fun (x y) (+ x y)) 1 2)" // EFun(List(x, y), EPlus(x, y)), EConst(CInt(1)), EConst(CInt(1)))
      //        println("4: "+code)
      val res = conv.toInt(run_myeval(code)) match {
        case Some(3) => true
        case _ => false
      }
        //        println("4: "+conv.toInt(run_myeval(code))+ " " +{if (res==true) "O" else "X" } + " answer: Some(3)")
        print_result(res)
        //        println("\n")
      }

      { // 5
        val code = "(let ((val f (fun () (+ 1 2)))) (f))"
        //        println("5: "+code)
        val res = conv.toInt(run_myeval(code)) match {
          case Some(3) => true
          case _ => false
        }
        print_result(res)
        //        println("5: "+conv.toInt(run_myeval(code)) + " " + {if (res==true) "O" else "X"} + " answer: Some(3)")
        //        println("\n")
      }

      { // 6
        val code = "(let ((val a 10) (val b (+ a 1))) (* b 3))"
        //        println("6: "+code)
        val res = conv.toInt(run_myeval(code)) match {
          case Some(33) => true
          case _ => false
        }
        print_result(res)
        //        println("6: "+conv.toInt(run_myeval(code)) + " " + {if (res == true) "O" else "X"}+ " answer: Some(33)")
        //        println("\n")
      }

      { // 7
        val code = "(let ((def f (fun (x) (if (= x 0) 0 (+ x (f (- x 1))))))) (f 5))"
        //        println("7: "+code)
        val res = conv.toInt(run_myeval(code)) match {
          case Some(15) => true
          case _ => false
        }
        print_result(res)
        //        println("7: "+conv.toInt(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: Some(15)")
        //        println("\n")
      }

      { // 8
        val code = "(let ((def f (fun (n) (g n 1))) (def g (fun (a b) (> a b)))) (f 3))"
        //        println("8: "+code)
        val res = conv.toBool(run_myeval(code)) match {
          case Some(true) => true
          case _ => false
        }
        print_result(res)
        //        println("8: "+conv.toBool(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: Some(true)")
        //        println("\n")
      }

      { // 9
        val code = "((fun (f) (fun (x) (f x))) (fun (x) (+ x 1)))"
        //        println("9: "+code)
        val res = conv.isFun(run_myeval(code)) match {
          case true => true
          case _ => false
        }
        print_result(res)
        //        println("9: " + conv.isFun(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: true")
        println("\n\n")
      }
    } catch {
      case e : LexerException =>
        println("Lexer failed: " + e.msg)
      case e : ParserException =>
        println("Parser failed: " + e.msg)
      case e : EvalException =>
        println("myeval failed: " + e.msg)
    }

    try {
      println("=================")
      println("2. Tailrec Test (should be finished)")
      val code = "(let ((def f (fun (x n) (if (= x 0) n (f (- x 1) (+ n x)))) )) (f 9999 0))"
      /*
      def f(x, n) {
        if (x == 0) n
        else f(x-1, n+x)
        }

        f(9999, 0)

        -->
        f(9999, 0)
        --> f(9998, 9999)
        --> f(9997, 9999 + 9998)
        --> f(9997, 9999 + 9998 + 9997)
        ...
        f(0, 9999 + 9998 + ... + 1)
       */
      val res = conv.toInt(run_myeval(code)) match {
        case Some(49995000) => true
        case _ => false
      }
      print_result(res)
    } catch {
      case e : LexerException =>
        println("Lexer failed: " + e.msg)
      case e : ParserException =>
        println("Parser failed: " + e.msg)
      case e : EvalException =>
        println("myeval failed: " + e.msg)
    }

    /*
    def f(n) {
    if (n == 0) 1
    else if (n == 1) 0
    else if (f(n-1) > f(n-2)) 0
    else 1
    }

    f(100)

     */
    try {
      println("=================")
      println("3. Memoization Test (should take less than 5 sec)")

      { // 1
        val code = "(hd (cons 1 2))"
        //        println("1: "+code)
        val res = conv.toInt(run_myeval_memo(code)) match {
          case Some(1) => true
          case _ => false
        }
        //        println("1: "+conv.toInt(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: Some(1)")
        print_result(res)
        //        println("\n")
      }

      { // 2
        val code = "(let ((val p (cons 1 (cons true nil)))) (cons 0 p))"
        //        println("2: "+code)
        val res = conv.toPair(run_myeval_memo(code)) match {
          case Some(_) => true // this only checks whether the result is a pair.
          case _ => false
        }
        //        println("2: "+ conv.toPair(run_myeval(code)) +" "+ { if (res == true) "O" else "X"} + " answer: Some(_)")
        print_result(res)
        //        println("\n")
      }

      { // 3
        val code = "(if true 10 20)"
        //        println("3: "+code)
        val res = conv.toInt(run_myeval_memo(code)) match {
          case Some(10) => true
          case _ => false
        }
        //        println("3: "+ conv.toInt(run_myeval(code)) + " " + { if (res == true) "O" else "X"} + " answer: Some(10)")
        print_result(res)
        //        println("\n")
      }

      { // 4
        val code = "((fun (x y) (+ x y)) 1 2)" // EFun(List(x, y), EPlus(x, y)), EConst(CInt(1)), EConst(CInt(1)))
      //        println("4: "+code)
      val res = conv.toInt(run_myeval_memo(code)) match {
        case Some(3) => true
        case _ => false
      }
        //        println("4: "+conv.toInt(run_myeval(code))+ " " +{if (res==true) "O" else "X" } + " answer: Some(3)")
        print_result(res)
        //        println("\n")
      }

      { // 5
        val code = "(let ((val f (fun () (+ 1 2)))) (f))"
        //        println("5: "+code)
        val res = conv.toInt(run_myeval_memo(code)) match {
          case Some(3) => true
          case _ => false
        }
        print_result(res)
        //        println("5: "+conv.toInt(run_myeval(code)) + " " + {if (res==true) "O" else "X"} + " answer: Some(3)")
        //        println("\n")
      }

      { // 6
        val code = "(let ((val a 10) (val b (+ a 1))) (* b 3))"
        //        println("6: "+code)
        val res = conv.toInt(run_myeval_memo(code)) match {
          case Some(33) => true
          case _ => false
        }
        print_result(res)
        //        println("6: "+conv.toInt(run_myeval(code)) + " " + {if (res == true) "O" else "X"}+ " answer: Some(33)")
        //        println("\n")
      }

      { // 7
        val code = "(let ((def f (fun (x) (if (= x 0) 0 (+ x (f (- x 1))))))) (f 5))"
        //        println("7: "+code)
        val res = conv.toInt(run_myeval_memo(code)) match {
          case Some(15) => true
          case _ => false
        }
        print_result(res)
        //        println("7: "+conv.toInt(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: Some(15)")
        //        println("\n")
      }

      { // 8
        val code = "(let ((def f (fun (n) (g n 1))) (def g (fun (a b) (> a b)))) (f 3))"
        //        println("8: "+code)
        val res = conv.toBool(run_myeval_memo(code)) match {
          case Some(true) => true
          case _ => false
        }
        print_result(res)
        //        println("8: "+conv.toBool(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: Some(true)")
        //        println("\n")
      }

      { // 9
        val code = "((fun (f) (fun (x) (f x))) (fun (x) (+ x 1)))"
        //        println("9: "+code)
        val res = conv.isFun(run_myeval_memo(code)) match {
          case true => true
          case _ => false
        }
        print_result(res)
        //        println("9: " + conv.isFun(run_myeval(code)) + " " + {if (res == true) "O" else "X"} + " answer: true")
        println("\n\n")
      }

      {
        val code = "(let ((def f (fun (n) (if (= n 0) 1 (if (= n 1) 0 (if (> (f (- n 1)) (f (- n 2))) 0 1)))))) (f 100))"
        // smaller test
        //      val code = "(let ((def f (fun (n) (if (= n 0) 1 (if (= n 1) 0 (if (> (f (- n 1)) (f (- n 2))) 0 1)))))) (f 5))"
        val res = conv.toInt(run_myeval_memo(code)) match {
          case Some(1) => true
          case _ => false
        }
        print_result(res)
      }
    }catch {
      case e : LexerException =>
        println("Lexer failed: " + e.msg)
      case e : ParserException =>
        println("Parser failed: " + e.msg)
      case e : EvalException =>
        println("myeval failed: " + e.msg)
    }
  }

  run_test
}
