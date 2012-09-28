package probcomp

/**
 * A disjoint sets data structure.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 * @param n the number of initial disjoint sets
 */
class DisjointSets (n: Int) {

  // make sure we're using mutable sets
  import scala.collection.mutable.Set

  // create a parallel set of mutable sets
  val sets = Set.empty[Set[Int]]

  // create the initial disjoint sets
  for (i <- 0 until n) {
    sets add Set(i)
  } // for
 
  /**
   * Returns the indices in the set array of the sets that contain x.
   * 
   * @param an element to find
   * @return an array of indices corresponding to the sets containing x
   */
  def find (x: Int): Set[Int] = {
    var set: Set[Int] = null
    sets.view.filter(set => set.exists(_ == x)).foreach(s => set = s)
    set
  } // find

  /**
   * Unites the dynamic sets that contains x and y into a new set that is the
   * union of these two sets.
   *
   * @param x an element of the first set
   * @param y an element of the second set
   */
  def union (x: Int, y: Int): Unit = {
    val xSet = find(x)
    val ySet = find(y)
    if (xSet != ySet){
      if (xSet.size > ySet.size) {
	xSet ++= ySet
	sets remove ySet
      } else {
	ySet ++= xSet
	sets remove xSet
      } // if
    } // if
  } // union

} // DisjointSets

/**
 * A disjoint sets datastructure made to look more like a graph.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 * @param n the number of initial disjoint sets
 */
class DisjointSetGraph (n: Int) extends DisjointSets (n) {

  // make sure we're using mutable sets
  import scala.collection.mutable.Set

  /**
   * Adds an edge to the graph.
   *
   * @param x one vertex in the edge
   * @param y the other vertex in the edge
   */
  def addEdge (x: Int, y: Int): Unit = union(x, y)

  def numComponents: Int = sets.toSet.size

} // DisjointSetGraph

object Hypergeometric {

  /**
   * Regularized Incomplete Beta Function
   */
  //def I (x: Int, a: Double, b: Double) = B(x, a ,b) / B(a, b)

} // Hypergeometric

/**
 * Statistic class based on code from the ScalaTion project
 */
class Statistic () {

  import scala.math.sqrt

  // samples
  protected var n = 0

  // sum
  protected var sum = 0.
  
  // sum of squares
  protected var sumSq = 0.

  // min
  protected var minX = Double.MaxValue

  // max
  protected var maxX = 0.

  /**
   * Samples a value
   *
   * @param x the value sampled
   */
  def sample (x: Double) {
    n     += 1
    sum   += x
    sumSq += x * x
    if (x < minX) minX = x
    if (x > maxX) maxX = x
  } // sample

  def size     = n
  def min      = if (n == 0) 0. else minX
  def max      = if (n == 0) 0. else maxX
  def mean     = if (n == 0) 0. else sum / n.toDouble
  def variance = if (n == 0) 0. else sumSq / (n.toDouble - 1) - mean * mean
  def stddev   = sqrt(variance)
  def ms       = sumSq / n.toDouble 
  def rms      = sqrt(ms)

  def printSummary: Unit = {
    println("| %7s | %9s | %9s | %9s | %9s |".format("samples", "min", "max", "mean", "stdDev"))
    println("| %7d | %9.3f | %9.3f | %9.3f | %9.3f |".format(n, min, max, mean, stddev))
  } // printSummary

} // Statistic

/**
 * Code for the midterm assignment
 */
object Midterm extends App {

  import scala.util.control.Breaks._
  import scala.util.Random

  // 100(100)1000
  for (n <- 100 to 1000 by 100) {
    
    val stat = new Statistic()
    val r    = new Random()
    val p    = 0.5

    var g: DisjointSetGraph = null

    // need to take 100 samples
    for (i <- 0 until 100) {

      var counter = 0
      
      g = new DisjointSetGraph(n)

      breakable {
	for (edge <- (0 until n).combinations(2).toList) {
	  if (r.nextDouble < p) {
	    g.addEdge(edge(0), edge(1))
	    counter += 1
	  } // if
          if (g.numComponents == 1) break
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


