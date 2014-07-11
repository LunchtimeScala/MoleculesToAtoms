package mark

import scalaz._
import Scalaz._

class ElementWeights(val weights: Map[Element, Int]) extends Equals {
  
  /**
   * Creates a new ElementWeights where all
   * elements of this and the other weights
   * are present. Elements in both are summed.
   * 
   * Uses Scalaz's semigroup.
   * 
   * See http://stackoverflow.com/a/7076581
   * @param e2
   * @return
   */
  def +(e2: ElementWeights) = {
    new ElementWeights(weights |+| e2.weights)
  }
  
  /**
   * Creates a new map of element weights where all
   * existing element weights are multiplied by i
   * @param i
   * @return
   */
  def *(i: Int) = {
    new ElementWeights(weights.map {case (k,v) => (k,v*i)})
  }
  
  override def toString() = weights.toString
  
  def canEqual(other: Any) = {
    other.isInstanceOf[mark.ElementWeights]
  }
  
  override def equals(other: Any) = {
    other match {
      case that: mark.ElementWeights => that.canEqual(ElementWeights.this) && weights == that.weights
      case _ => false
    }
  }
  
  override def hashCode() = {
    val prime = 41
    prime + weights.hashCode
  }
  
}

/**
 * (ElementWeights, +) is a monoid.
 * 
 * @author mbuckley
 *
 */
object ElementWeights {

  val zero = new ElementWeights(Map.empty[Element, Int])
}