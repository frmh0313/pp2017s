package pp201701.hw7test
import pp201701.hw7.Main._
import pp201701.hw7.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  val report1 = get_report(true, 2)
  print_result(pretty_printer(report1) == "<Primes 2>\n * 1-th prime : 3\n * 2-th prime : 5\n")
  println(pretty_printer(report1))

  val report2 = get_report(false, 3)
  print_result(pretty_printer(report2) == "<Pi 3>\n * 1-th digit : 1\n * 2-th digit : 4\n * 3-th digit : 1\n")
  println(pretty_printer(report2))

  val report3 = get_report(true, 21)
  print_result(pretty_printer(report3) == "<Error: Too big input>\n")
  println(pretty_printer(report3))

  val report4 = get_report(true, 20)
  //  print_result(pretty_printer(report4) == "<Primes 20>\n * 1 : 3\n * 2 : 5\n")
  println(pretty_printer(report4))

  val report5 = get_report(false, 20)
  //  print_result(pretty_printer(report5) == "<Pi 20>\n * 1 : 1\n * 2 : 4\n * 3 : 1\n")
  println(pretty_printer(report5))


  // additional
  val report6 = get_report(true, 0)
  print_result(pretty_printer(report6) == "<Primes 0>\n")
  println(pretty_printer(report6))

  val report7 = get_report(false, 0)
  print_result(pretty_printer(report7) == "<Pi 0>\n")
  println(pretty_printer(report7))

  val report8 = get_report(false, 22)
  print_result(pretty_printer(report8) == "<Error: Too big input>\n")
  println(pretty_printer(report8))
}
