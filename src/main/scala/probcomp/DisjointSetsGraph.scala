package probcomp

/**
 * A disjoint sets datastructure made to look more like a graph.
 *
 * @author Michael E. Cotterell <mepcotterell@gmail.com>
 * @param n the number of initial disjoint sets
 */
class DisjointSetsGraph (n: Int, debug: Boolean = false) extends DisjointSets (n) {

  // make sure we're using mutable sets
  import scala.collection.mutable.Set

  /**
   * Adds an edge to the graph.
   *
   * @param x one vertex in the edge
   * @param y the other vertex in the edge
   */
  def addEdge (x: Int, y: Int): Unit = {
    if (debug) println("[dbg] adding edge (%d, %d)".format(x, y))
    if (debug) println("[dbg] sets before adding (%d, %d): %s".format(x, y, sets))
    union(x, y)
    if (debug) println("[dbg] sets after (%d, %d): %s".format(x, y, sets))
    if (debug) println("[dbg] number of components = %d".format(numComponents))
  } // addEdge

  /**
   * Returns the number of connected components. Isolated vertices count as a
   * connected component here.
   */
  def numComponents: Int = sets.size

  /**
   * Returns whether or not the graph is connected
   */
  def connected: Boolean = sets.view.count(set => set.size == n) > 0

  /**
   * Returns the number of isolated vertices
   */
  def numIsolated: Int = sets.view.count(set => set.size == 1)

} // DisjointSetsGraph
