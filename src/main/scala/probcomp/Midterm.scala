package probcomp

import scala.collection.parallel.mutable.ParArray
import scala.util.control.Breaks._

import probcomp.random.Implicits._
import probcomp.random.Random
import probcomp.stat.Statistic

object MidtermTest extends App {

  // time a function in ns
  def time (f: => Unit): Double = {
    val begin = System.nanoTime
    f
    System.nanoTime - begin
  } // time


  val n = 10
  val stat = new Statistic()
  val r    = new Random()
  val p    = 0.5

  // all possible edges for a graph of size n

  println("n = %d".format(n))

  print(" - running garbage collection... ")
  System.gc
  println("finished")

  print(" - generating all possible edges for %d vertices... ".format(n))
  val allEdges = (0 until n).combinations(2).toIterable
  println("finished")

  print(" - generating random iterators... ")
  val iters = ParArray.fill(100)(r.shuffle(allEdges).toArray)
  println("finished")

  print(" - sampling... ");
  // need to take 100 samples
  val t = time { 

    for (i <- 1 to 1 par) {

      var counter = 0
      val g = new DisjointSetsGraph(n, true)
      val rand = new Random()

      breakable {
	for (j <- 0 until iters(i-1).length) {
	  val edge = iters(i-1)(j)
	  if (rand.gen < p) {
	    g.addEdge(edge(0), edge(1))
	    counter += 1
	  } // if
          if (g.connected) break
        } // for
      } // breakable
      
      // add our sample to the statistic
      synchronized { stat.sample(counter) }

      print(".")

    } // for

  } // time

  println("finished")
  println(" - sampling stats %.3f ns, %.3f us, %.6f ms, %.9f s".format(t, t / 1000, t / 1000000, t / 1000000000))

  println
  stat.printSummary
  println

} // MidtermTest

/**
 * Code for the midterm assignment.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 */
object Midterm extends App {

  val min  = 1000
  val max  = 1000
  val step = 100

  // time a function in ns
  def time (f: => Unit): Double = {
    val begin = System.nanoTime
    f
    System.nanoTime - begin
  } // time

  println
  println("EXERCISE 5.20 (b)")

  // 100(100)1000
  for (n <- min to max by step) {
    
    val stat = new Statistic()
    val r    = new Random()
    val p    = 0.5

    // all possible edges for a graph of size n

    println("n = %d".format(n))

    print(" - running garbage collection... ")
    System.gc
    println("finished")

    print(" - generating all possible edges for %d vertices... ".format(n))
    val allEdges = (0 until n).combinations(2).toIterable
    println("finished")

    print(" - generating random iterators... ")
    val iters = ParArray.fill(100)(r.shuffle(allEdges).toArray)
    println("finished")

    print(" - sampling... ");
    // need to take 100 samples
    val t = time { 

      for (i <- 1 to 100 par) {

        var counter = 0
        val g = new DisjointSetsGraph(n)
	val rand = new Random()

        breakable {
	  for (j <- 0 until iters(i-1).length) {
	    val edge = iters(i-1)(j)
	    if (rand.gen < p) {
	      g.addEdge(edge(0), edge(1))
	      counter += 1
	    } // if
            if (g.numIsolated == 0) break
          } // for
        } // breakable
      
        // add our sample to the statistic
        synchronized { stat.sample(counter) }

	print(".")

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
  for (n <- min to max by step) {
    
    val stat = new Statistic()
    val r    = new Random()
    val p    = 0.5

    // all possible edges for a graph of size n

    println("n = %d".format(n))

    print(" - running garbage collection... ")
    System.gc
    println("finished")

    print(" - generating all possible edges for %d vertices... ".format(n))
    val allEdges = (0 until n).combinations(2).toIterable
    println("finished")

    print(" - generating random iterators... ")
    val iters = ParArray.fill(100)(r.shuffle(allEdges).toArray)
    println("finished")

    print(" - sampling... ");
    // need to take 100 samples
    val t = time {

      for (i <- 1 to 100 par) {

        var counter = 0
        val g = new DisjointSetsGraph(n)
	val rand = new Random()

        breakable {
	  for (j <- 0 until iters(i-1).length) {
	    val edge = iters(i-1)(j)
	    if (rand.gen < p) {
	      g.addEdge(edge(0), edge(1))
	      counter += 1
	    } // if
            if (g.connected) break
	  } // for
        } // breakable
      
        // add our sample to the statistic
        synchronized { stat.sample(counter) }

	print(".")

      } // for

    } // time
    println("finished")
    println(" - sampling stats %.3f ns, %.3f us, %.6f ms, %.9f s".format(t, t / 1000, t / 1000000, t / 1000000000))

    println
    stat.printSummary
    println

  } // for 

} // Midterm


