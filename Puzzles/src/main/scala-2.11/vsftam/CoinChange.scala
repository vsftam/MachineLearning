package vsftam

/**
  * Created by Vincent on 7/16/16.
  */
object CoinChange {

  val denominations = List(25, 10, 5, 1)

  private def coinChange(amount: Int, denominations: List[Int]) : Int = {
    if(amount == 0 && denominations.length >= 0) 1
    else if(amount < 0 || denominations.isEmpty) 0
    else
      coinChange(amount - denominations.head, denominations) + coinChange(amount, denominations.tail)

  }

  def getCoinChange(amount: Int) = coinChange(amount, denominations)

  def main(args: Array[String]) {
    println("Call coinChange for $100: " + getCoinChange(100) )
    println("Call coinChange for $10: " + getCoinChange(10) )
    println("Call coinChange for $5: " + getCoinChange(5) )
  }
}
