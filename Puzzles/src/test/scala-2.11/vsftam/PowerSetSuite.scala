package vsftam

import org.scalatest._

import vsftam.PowerSet._

/**
  * Created by Vincent on 8/13/16.
  */
class PowerSetSuite extends FunSuite {

  test("Empty Power Set") {
    val a = powerSet( Set() )
    assert(a === Set(Set()) )
  }

  test("Power Set of 1 element") {
    val a = powerSet( Set('A') )
    assert(a === Set(Set(), Set('A')))
  }

  test("Power Set of 3 elements") {
    val a = powerSet( Set('A','B','C') )
    assert(a === Set(Set(), Set('C', 'A'), Set('B'), Set('C', 'B', 'A'), Set('B', 'A'), Set('A'), Set('C'), Set('C','B')))
  }
}
