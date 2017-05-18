package pp201701.hw1
import scala.annotation.tailrec

object Main {
  /*
   Implement given functions, which is currently blank. (???)
   */

  /*
   Exercise 1: PPascal's Triangle.
   As this course is PP, we are going to build PPascal's Triangle.
   This is similar to the Pascal's Triangle (google it!) except that
   it adds the upper left value once more.
   Write the program to calculate the number in PPascal's Triangle,
   for the given row and column.
   You may assume 0 <= r <= 8 && 0 <= c <= r
                           1
                        1     1
                     1     3     1
                  1     5     7     1
               1     7     17    15    1
            1     9     31    49    31    1
         1     11    49    111   129   63    1
      1     13    71    209   351   321   127   1
   1     15    97    351   769   1023  769   255   1
   */
  def ppascal(r: Int, c: Int): Int = {
    if (r==0||c==0||c==r) 1
    else 2*ppascal(r-1, c-1) + ppascal(r-1, c)
  }

  /*
   Exercise 2: Fibonacci
   A) Given n, calculate n'th value of Fibonacci number. (https://en.wikipedia.org/wiki/Fibonacci_number)
   Having exponential time complexity is OK for this problem.
   You may assume 1 <= n <= 20.
   */
  def fibA(n: Int): Int = {
    if(n==1||n==2) 1
    else fibA(n-1) + fibA(n-2)
  }

  /*
   B) Same with a), but you should implement it faster.
   Having exponential time complexity is NOT OK for this problem.
   Your algorithm should take linear time complexity.
   You may assume 1 <= n <= 10^3.
   */
  def fibB(n: Int): BigInt = {
    /*
     This skeleton code is just for hint. You may implement this problem in your own way.
     n := index for Fibonacci number, n >= 2
     output := (n-1)'th Fibonacci number and n'th Fibonacci number
     */
    /*
     You can implement remaining parts without knowledge of pair type.
     If you want to know more about pair/tuple, search "scala tuple" on google.
     In short, you can build tuple with "(_, _, _, .. _,)" syntax,
     and get n'th element with "._n".
     */
    def _fibB(n: Int): (BigInt, BigInt) = {
      if(n==2) (1, 1)
      else {
        val (pastPast, past) = _fibB(n - 1)
        (past, pastPast+past)
      }
    }
    if(n==1) 1
    else _fibB(n)._2
  }

  /*
   C) Same with b), but you should implement it with tail recursion.
   By using tail recursion strategy, your algorithm will not take linear stack depth.
   Instead it will only take constant stack depth.
   You may assume 1 <= n <= 10^5.
   */
  def fibC(n: Int): BigInt = {
    /*
     This skeleton code is just for hint. You may implement this problem in your own way.
     idx := current index
     current := Fibonacci number for idx.
     past := Fibonacci number for idx-1.
     output := n'th Fibonacci number
     */
    @tailrec
    def _fibC(idx: Int, current: BigInt, past: BigInt): BigInt = {
      if (idx<=1) current
      else _fibC(idx-1, current+past, current)
    }
    _fibC(n, 1, 0)
  }
}
