package probcomp

/**
 * A disjoint sets datastructure made to look more like a graph.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 * @param n the number of initial disjoint sets
 */
class DisjointSetsGraph (n: Int) extends DisjointSets (n) {

  // make sure we're using mutable sets
  import scala.collection.mutable.Set

  /**
   * Adds an edge to the graph.
   *
   * @param x one vertex in the edge
   * @param y the other vertex in the edge
   */
  def addEdge (x: Int, y: Int): Unit = union(x, y)

  /**
   * Returns the number of connected components. Isolated vertices count as a
   * connected component here.
   */
  def numComponents: Int = sets.toSet.size

} // DisjointSetsGraph
