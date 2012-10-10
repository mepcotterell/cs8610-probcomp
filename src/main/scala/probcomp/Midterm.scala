package probcomp

import scala.util.control.Breaks._

import probcomp.random.Implicits._
import probcomp.random.Random
import probcomp.stat.Statistic

/**
 * Code for the midterm assignment.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 */
object Midterm extends App {

  // time a function in ns
  def time (f: => Unit): Double = {
    val begin = System.nanoTime
    f
    System.nanoTime - begin
  } // time

  println
  println("EXERCISE 5.20 (b)")

  // 100(100)1000
  for (n <- 100 to 1000 by 100) {
    
    val stat = new Statistic()
    val r    = new Random()
    val p    = 0.5

    var g: DisjointSetsGraph = null
    
    // all possible edges for a graph of size n

    println("n = %d".format(n))
    print(" - generating all possible edges for %d vertices... ".format(n))
    val allEdges = (0 until n).combinations(2).toIterable
    println("finished")

    print(" - generating random iterators... ")
    val iters = Array.fill(100)(r.shuffle(allEdges))
    println("finished")

    print(" - sampling... ");
    // need to take 100 samples
    val t = time { 

      for (i <- 1 to 100) {

        var counter = 0
      
        g = new DisjointSetsGraph(n)

        breakable {
	  for (edge <- iters(i-1)) {
	    if (r.gen < p) {
	      g.addEdge(edge(0), edge(1))
	      counter += 1
	    } // if
            if (g.numIsolated == 0) break
          } // for
        } // breakable
      
        // add our sample to the statistic
        stat.sample(counter)

	if (i % 10 == 0) print("%d ".format(i))

      } // for

    } // time
    println("finished")
    println(" - sampling stats %.3f ns, %.3f us, %.6f ms, %.9f s".format(t, t / 1000, t / 1000000, t / 1000000000))

    println
    stat.printSummary
    println

  } // for 

  println
  println("EXERCISE 5.20 (a)")

  // 100(100)1000
  for (n <- 100 to 1000 by 100) {
    
    val stat = new Statistic()
    val r    = new Random()
    val p    = 0.5

    var g: DisjointSetsGraph = null
    
    // all possible edges for a graph of size n

    println("n = %d".format(n))
    print(" - generating all possible edges for %d vertices... ".format(n))
    val allEdges = (0 until n).combinations(2).toIterable
    println("finished")

    print(" - generating random iterators... ")
    val iters = Array.fill(100)(r.shuffle(allEdges))
    println("finished")

    print(" - sampling... ");
    // need to take 100 samples
    val t = time {

      for (i <- 1 to 100) {

        var counter = 0
      
        g = new DisjointSetsGraph(n)

        breakable {
	  for (edge <- iters(i-1)) {
	    if (r.gen < p) {
	      g.addEdge(edge(0), edge(1))
	      counter += 1
	    } // if
            if (g.connected) break
	  } // for
        } // breakable
      
        // add our sample to the statistic
        stat.sample(counter)

        if (i % 10 == 0) print("%d ".format(i))

      } // for

    } // time
    println("finished")
    println(" - sampling stats %.3f ns, %.3f us, %.6f ms, %.9f s".format(t, t / 1000, t / 1000000, t / 1000000000))

    println
    stat.printSummary
    println

  } // for 

} // Midterm


