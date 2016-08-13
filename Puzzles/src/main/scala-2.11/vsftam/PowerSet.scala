package vsftam

/**
  * Created by Vincent on 7/16/16.
  */
object PowerSet {

  def powerSet[A](s: Set[A]) : Set[Set[A]] =
    if( s.isEmpty ) {
      Set( Set() )
    }
    else {
      s.flatMap( e => {
        val ps = powerSet(s - e)
        ps ++ ps.map(_ + e)
      } )
    }

  def main(arg: Array[String]) = {
    println( powerSet(Set("A", "B", "C")))
  }

}
