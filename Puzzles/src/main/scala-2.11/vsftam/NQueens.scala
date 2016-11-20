package vsftam

object NQueens {
 
    def solveNQueens(n: Int)(checkStrategy: ((Int,Int), (Int,Int)) => Boolean = inCheck): List[List[(Int, Int)]] = {
        def placeQueens(k: Int): List[List[(Int, Int)]] = {
            if(k == 0)
                List(List())
            else {
                for {
                    queens <- placeQueens(k - 1)
                    column <- 1 to n
                    queen = (k, column) 
                    if( isSafe(queen, queens))
                } yield queen :: queens 
            }
        }

        def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = {
            queens forall (q => !checkStrategy(queen, q))
        }
        placeQueens(n)
    }

    def inCheck(q1: (Int, Int), q2: (Int, Int)): Boolean = 
        q1._1 == q2._1 || q1._2 == q2._2 || (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

    def inCheckSuperQueen(q1: (Int, Int), q2: (Int, Int)): Boolean = 
        inCheck(q1, q2) ||
        ((q1._1 - q2._1).abs == 2 && (q1._2 - q2._2).abs == 1) ||
        ((q1._1 - q2._1).abs == 1 && (q1._2 - q2._2).abs == 2)

    def main(args: Array[String]) {
        println(solveNQueens(8) _)
        println(solveNQueens(10)(inCheckSuperQueen))
    }
}