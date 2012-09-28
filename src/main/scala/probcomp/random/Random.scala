package probcomp.random

/**
 * Represents a uniform (0.0, 1.0) random variable
 */
class Random () {

  // random number generator
  private val r = new scala.util.Random()

  /**
   * Returns a value from the codomain of this random variable with uniform
   * probability.
   */
  def gen: Double = r.nextDouble

} // Random


