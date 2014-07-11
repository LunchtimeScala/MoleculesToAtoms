package mark

import scala.collection.mutable.Stack

object ComputeElementWeights {
  
  /**
   * Creates a typed token list from a string
   * Skips anything which isn't a legal molecule token.
   * 
   * Todo: error checking for e.g. unbalanced bracketing
   * Different bracket types
   * @param s
   * @return
   */
  def tokeniseMoleculeString(s: String): Seq[Token] = {
    s.flatMap{Token.char2Token(_)}
  }
  
  /**
   * Start parsing and weighing a new molecule
   * @param s
   * @return
   */
  def computeElementWeightsInitialise(s: String): ElementWeights = {
    val toks = tokeniseMoleculeString(s)
    val tokPos = 0
    
    val (result, finalPos) = computeElementWeight(toks, tokPos)
    
    // if (finalPos != toks.length) do something
    result
  }
  
  /**
   * @param toks The tokens in the molecule.
   * @param tokPos The current token position. We weigh the molecule starting from here
   * @return
   */
  def computeElementWeight(toks: Seq[Token], tokPos: Int): (ElementWeights, Int) = {

    // this stack contains the weights of each element and submolecule
    // of the molecule we're currently weighing
    val stk = Stack.empty[ElementWeights]
    
    var currentPos = tokPos
    
    while (currentPos < toks.length) {
      
      toks(currentPos) match {
        
        // push a new element onto the stack
        case tok: Element => {
          stk.push(new ElementWeights(Map((tok->1))))
          currentPos += 1
        }
        
        // multiply the previous element or submolecule by a multiplier
        case Multiplier(n) => {
          stk.push(stk.pop * n)
          currentPos += 1
        }
        
        // recursive call: start a new submolecule, then shift token position to end of submolecule
        case _: OpenBracket => {
          val (subMolecule, subMolEndPos) = computeElementWeight(toks, currentPos+1)
          stk.push(subMolecule)
          currentPos = subMolEndPos+1
        }
        
        // finished a submolecule, so sum up the stack and return the submolecule
        case _: CloseBracket => {
          val subMolecule = stk.reduce(_+_)
          return (subMolecule, currentPos)
        }
      }
    }
    
    // we're at the end of the string, so add up the elements on the stack and we're done.
    (stk.reduce(_+_), currentPos)
    
  }

}