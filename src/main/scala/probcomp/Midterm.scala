package probcomp

import probcomp.random.Implicits._
import probcomp.random.Random
import probcomp.stat.Statistic

/**
 * Code for the midterm assignment
 */
object Midterm extends App {

  import scala.util.control.Breaks._

  println("EXERCISE 5.20 (b)")

  // 100(100)1000
  for (n <- 100 to 1000 by 100) {
    
    val stat = new Statistic()
    val r    = new Random()
    val p    = 0.5

    var g: DisjointSetsGraph = null

    // need to take 100 samples
    for (i <- 0 until 100) {

      var counter = 0
      
      g = new DisjointSetsGraph(n)

      breakable {
	for (edge <- (0 until n shuffled).combinations(2)) {
	  if (r.gen < p) {
	    g.addEdge(edge(0), edge(1))
	    counter += 1
	  } // if
          if (g.numIsolated == 0) break
	} // for
      } // breakable
      
      // add our sample to the statistic
      stat.sample(counter)

    } // for

    println
    println("n = %d".format(n))
    stat.printSummary

  } // for 

  println("EXERCISE 5.20 (a)")

  // 100(100)1000
  for (n <- 100 to 1000 by 100) {
    
    val stat = new Statistic()
    val r    = new Random()
    val p    = 0.5

    var g: DisjointSetsGraph = null

    // need to take 100 samples
    for (i <- 0 until 100) {

      var counter = 0
      
      g = new DisjointSetsGraph(n)

      breakable {
	for (edge <- (0 until n shuffled).combinations(2)) {
	  if (r.gen < p) {
	    g.addEdge(edge(0), edge(1))
	    counter += 1
	  } // if
          if (g.connected) break
	} // for
      } // breakable
      
      // add our sample to the statistic
      stat.sample(counter)

    } // for

    println
    println("n = %d".format(n))
    stat.printSummary

  } // for 

} // Midterm


