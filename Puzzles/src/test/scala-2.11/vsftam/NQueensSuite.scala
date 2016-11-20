package vsftam

import org.scalatest.FunSuite
import vsftam.NQueens._

class NQueensSuite extends FunSuite {

  test("solve super queen") {
    assert(4 === (solveNQueens(10)(inCheckSuperQueen)).size)
  }
}