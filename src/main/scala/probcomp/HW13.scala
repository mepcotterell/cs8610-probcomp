package probcomp

import scala.collection.mutable.Set
import scala.math.abs
import scala.util.control.Breaks._

object HW13 extends App {

  for (n <- 1 to 6) {

    val V = Set.empty[(Int, Int)]
    val E = Set.empty[((Int, Int), (Int, Int))]

    for (i <- 1 to n; j <- 1 to n) V += ((i, j))

    for (i <- 1 to n; j <- 1 to n) {
      for (ii <- 1 to n; jj <- 1 to n) {
        if ((abs(i - ii) + abs(j - jj)) == 1) E += (((i, j),(ii, jj)))
      } // for  
    } // for

    var count = 0;

    for (subset <- V.subsets) {
      breakable { 
        for (i <- subset; j <- subset) if (E.contains((i, j))) break
        count += 1
      } // breakable
    } // for

    println("g(%d) = %d".format(n, count))

  } // for

} // HW13
