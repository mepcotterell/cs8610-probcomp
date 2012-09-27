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

  // create an array of mutable sets
  val sets = Array.ofDim[Set[Int]](n)
  
  // create the sets
  for (i <- 0 until sets.length) sets(i) = Set(i)

  /**
   * Returns the indices in the set array of the sets that contain x.
   *
   * @param an element to find
   * @return an array of indices corresponding to the sets containing x
   */
  def find (x: Int): Array[Int] = sets.view.zipWithIndex.filter(e => e._1.exists(_ == x)).map(e => e._2).toArray

  /**
   * Unites the dynamic sets that contains x and y into a new set that is the
   * union of these two sets.
   *
   * @param x an element of the first set
   * @param y an element of the second set
   */
  def union (x: Int, y: Int): Unit = for (xRoot <- find(x); yRoot <- find(y)) {
    sets(xRoot) ++= sets(yRoot)
    sets(yRoot)   = sets(xRoot)
  } // union

} // DisjointSets

/**
 * A disjoint sets datastructure made to look more like a graph.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 * @param n the number of initial disjoint sets
 */
class DisjointSetGraph (n: Int) extends DisjointSets (n) {

  /**
   * Adds an edge to the graph.
   *
   * @param x one vertex in the edge
   * @param y the other vertex in the edge
   */
  def addEdge (x: Int, y: Int): Unit = {
    union(x, y)
    println("add (%d, %d) => %s".format(x, y, sets.deep))
  } // addEdge

} // DisjointSetGraph

/**
 * Code for the midterm assignment
 */
object Midterm extends App {

  val dsg = new DisjointSetGraph(10)

  println("initial => %s".format(dsg.sets.deep))

  dsg.addEdge(1, 3)
  dsg.addEdge(4, 6)
  dsg.addEdge(0, 2)
  dsg.addEdge(7, 8)
  dsg.addEdge(0, 1)
  dsg.addEdge(4, 5)
  dsg.addEdge(1, 2)

} // Midterm


