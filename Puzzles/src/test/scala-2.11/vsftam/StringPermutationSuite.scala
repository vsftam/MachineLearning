package vsftam

import org.scalatest.FunSuite
import vsftam.StringPermutation._

/**
  * Created by Vincent on 8/14/16.
  */
class StringPermutationSuite extends FunSuite {

  test("empty string") {
    assert(List() === getStringPermutations(List("")))
  }

  test("one string") {
    assert(List("N", "O", "W") === getStringPermutations(List("NOW")))
  }

  test("two strings") {
    assert(List("GW", "GE", "GS", "GT", "OW", "OE", "OS", "OT") === getStringPermutations(List("GO", "WEST")))
  }
}
