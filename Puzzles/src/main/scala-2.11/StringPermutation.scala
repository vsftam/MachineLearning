/**
  * Created by Vincent on 7/16/16.
  */
object StringPermutation {

  def getStringPermutations(s: List[String]): List[String] = s match {
    case Nil => Nil
    case a :: Nil =>
      val l = for ( c <- a ) yield c.toString
      l.toList

    case a :: as =>
      val p  = getStringPermutations(as)
      val l = for ( c <- a; d <- p ) yield c +: d
      l.toList

  }

  def main(args: Array[String]) = {
    println(getStringPermutations(List("GO", "WEST")))
  }
}
