package reductions

import scala.math.min
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceHelper(chars: Array[Char], counter: Int) : Boolean =  {
      if(chars.isEmpty) counter == 0
      else if( counter < 0 ) false
      else {
        val delta = if(chars.head == '(') 1 else if (chars.head == ')') -1 else 0
        balanceHelper(chars.tail, counter + delta )
      }
    }
    balanceHelper(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /**
      * @param arg1 running total
      * @param arg2 running minimum
      */
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if(idx == until)
        (arg1, arg2)
      else {
        val c = chars(idx)
        val new_arg1 = if (c == '(')  arg1+1  else if( c == ')') arg1-1 else arg1
        traverse(idx+1, until, new_arg1, min(new_arg1, arg2))
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      //println("Calling reduce " + from + "->" + until)
      if(until - from <= threshold) {
        traverse(from, until, 0, 0)
        //println("traverse returns " + x._1 + ":" + x._2 + ":" + x._3)
      }
      else {
        val mid = from + (until - from) / 2
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        val running_total = l._1 + r._1
        val running_minimum = l._1 + r._2
        //println("reduce returns " + running_total + ":" + running_minimum + ":" + running_balance_flag.toString)
        (running_total, running_minimum)
      }
    }

    val res = reduce(0, chars.length)
    res._1 == 0 && res._2 >= 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
