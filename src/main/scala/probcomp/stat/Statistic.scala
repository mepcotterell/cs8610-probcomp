package probcomp.stat

/**
 * Statistic class based on code from the ScalaTion project
 */
class Statistic () {

  // import the sqrt function
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
