package scala.mark

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.GivenWhenThen

import mark._

class ComputeElementWeightsTest extends WordSpec with ShouldMatchers with GivenWhenThen  {

  "The tokeniser" should {
    "not fail on empty string" in {

    ComputeElementWeights.tokeniseMoleculeString("") should be (Seq.empty[Token])
    }
    "tokenise H2O" in {
      
      val result = Seq(new Element("H"), new Multiplier(2),new Element("O"))
      ComputeElementWeights.tokeniseMoleculeString("H2O") should be (result)
    }
    "tokenise H(O2)S" in {
      
      val result = Seq(
          new Element("H"),
          new OpenBracket("("),
          new Element("O"),
          new Multiplier(2),
          new CloseBracket(")"),
          new Element("S"))
      ComputeElementWeights.tokeniseMoleculeString("H(O2)S") should be (result)
    }
    "skip bad tokens in H2##O" in {
      
      val result = Seq(new Element("H"), new Multiplier(2),new Element("O"))
      ComputeElementWeights.tokeniseMoleculeString("H2##O") should be (result)
    }
  }
  
  "The element weights computer" should {
    
    "count the atoms in H2O" in {
      
      val actual = ComputeElementWeights.computeElementWeightsInitialise("H2O")
      // return {O: 1, H: 2}
      val expected = new ElementWeights(Map(
          Element("O") -> 1,
          Element("H") -> 2))
      
      actual should equal (expected)
    }
    
    "count the atoms in Mg(OH)2" in {
      
      val actual = ComputeElementWeights.computeElementWeightsInitialise("Mg(OH)2")
      // return {Mg: 1, O: 2, H: 2}
      val expected = new ElementWeights(Map(
          Element("Mg") -> 1,
          Element("O") -> 2,
          Element("H") -> 2))
      
      actual should equal (expected)
    }
    
    "count the atoms in K4(ON(SO3)2)2" in {
      
      val actual = ComputeElementWeights.computeElementWeightsInitialise("K4(ON(SO3)2)2")
      // return {K: 4, O: 14, N: 2, S: 4}
      val expected = new ElementWeights(Map(
          Element("K") -> 4,
          Element("O") -> 14,
          Element("N") -> 2,
          Element("S") -> 4))
      
      actual should equal (expected)
    }
    
  }
  
}