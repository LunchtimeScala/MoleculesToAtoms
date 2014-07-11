package scala.mark

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.GivenWhenThen
import mark.ElementWeights
import mark.Element

class ElementWeightsTest extends WordSpec with ShouldMatchers with GivenWhenThen  {

  "ElementWeight" should {
    "add two empty ElementWeights" in {
      
      val z1 = ElementWeights.zero
      val z2 = ElementWeights.zero
      
      z1 + z2 should be (ElementWeights.zero)
    }
    "multiply an element weights" in {
      
      val z1 = new ElementWeights(Map(Element("H")->2))
      
      val result = new ElementWeights(Map(Element("H")->4))
      
      z1 *2 should be (result)
    }
    "add two ElementWeights" in {
      
      val z1 = new ElementWeights(Map(Element("H")->2))
      val z2 = new ElementWeights(Map(Element("O")->1))
      
      val result = new ElementWeights(Map(Element("H")->2, Element("O")->1))
      
      z1 + z2 should be (result)
    }
    "add two overlapping ElementWeights" in {
      
      val z1 = new ElementWeights(Map(Element("H")->2, Element("O")->1))
      val z2 = new ElementWeights(Map(Element("O")->1))
      
      val result = new ElementWeights(Map(Element("H")->2, Element("O")->2))
      
      z1 + z2 should be (result)
    }
  }
  
}