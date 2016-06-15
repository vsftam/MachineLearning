package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val bVal = b()
    bVal * bVal - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val d = delta()
    var s = Set[Double]()
    if( d < 0 ) s
    else {
      val aVal = a()
      val ans = -1 * b() / (2 * aVal)
      if( d == 0) {
        s = s + ans
        s
      }
      else {
        val p = Math.sqrt(d) / (2 * aVal)
        val ans1 = ans + p
        val ans2 = ans - p
        s = s + ans1
        s = s + ans2
        s
      }
    }
  }
}
