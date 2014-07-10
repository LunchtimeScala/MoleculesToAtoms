package scala.simon

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.GivenWhenThen

class TestFormulas extends WordSpec with ShouldMatchers with GivenWhenThen  {

  "My Molecule Deconstructor" should {
    "confirm that a molecule string is correct" in {
      given("a new Deconstructor object")
      val deconst: Deconstructor = new Deconstructor()
            
      when ("I test with a new water molecule string")
      val water = "H2O"

      then ("the number of atoms should be {H: 2, O: 1}")   
      deconst.parseMolecule(water) should be (Map("H" -> 2, "O" -> 1))

      and ("when I test with a new Magnesium Hydroxide molecule string")
      val magnesiumHydroxide = "Mg(OH)2"

      then ("the number of atoms should be {Mg: 1, O: 2, H: 2}")   
      deconst.parseMolecule(magnesiumHydroxide) should be (Map("Mg" -> 1, "O" -> 2, "H" -> 2))
      
      and ("when I test with a new Fremy Salt molecule string")
      val fremySalt = "K4[ON(SO3)2]2"

      then ("the number of atoms should be {K: 4, O: 14, N: 2, S: 4}")   
      deconst.parseMolecule(fremySalt) should be (Map("K" -> 4, "O" -> 14, "N" -> 2, "S" -> 4))
      
      and ("when I test with an invalid molecule string")
      val invalidMolecule = "H2*O"

      then ("the we should throw an exception")   
      evaluating{deconst.parseMolecule(invalidMolecule)} should produce [java.lang.IllegalArgumentException]
    }
  }
  
}