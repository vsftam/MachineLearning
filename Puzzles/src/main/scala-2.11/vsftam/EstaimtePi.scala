package vsftam

import scala.util.Random

/**
  * Created by vincenttam on 4/2/17.
  */
object EstimatePi {

  def estimatePi(tries: Int) : Double = {
      var in = 0
      for(i <- 1 to tries;
        x = Random.nextDouble();
        y = Random.nextDouble();
        if x * x + y * y <= 1
      ) {
        in = in + 1
      }
      in.toDouble * 4 / tries
  }


  def main(args: Array[String]): Unit = {

    val pi = estimatePi(500000)
    System.out.println(s"estimatePi(500000) returns $pi")
  }
}
