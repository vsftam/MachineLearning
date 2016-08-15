package vsftam

import org.scalatest.FunSuite
import vsftam.CoinChange._

/**
  * Created by Vincent on 8/14/16.
  */
class CoinChangeSuite extends FunSuite{

  test("One denomination") {
    val denominations = List(25)
    assert(1 === coinChange(100, denominations))
  }

  test("no change") {
    val denominations = List(35)
    assert(0 === coinChange(50, denominations))
  }

  test("multiple denominations") {
    val denominations = List(25, 10, 5, 1)
    assert(2 === coinChange(5, denominations))
    assert(4 === coinChange(10, denominations))
    assert(242 === coinChange(100, denominations))
  }
}
