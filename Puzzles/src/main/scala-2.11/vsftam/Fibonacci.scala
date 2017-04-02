package vsftam

import scala.annotation.tailrec

/**
  * Created by vincenttam on 4/2/17.
  */
object Fibonacci {

  def fib(n: Int): Int =  n match {
    case 0 => 1
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }

  def fib2(n: Int): Int = {

    @tailrec
    def fib2helper(n: Int, prev: Int, acc: Int): Int = n match {
      case 0 => acc
      case _ => fib2helper(n - 1, acc, prev + acc)
    }

    fib2helper(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    val n = fib(5)
    val m = fib2(5)

    System.out.println(s"fib(5) returns $n")
    System.out.println(s"fib2(5) returns $m")
  }
}
