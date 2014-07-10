package scala.simon

/** A tool for deconstructing molecule strings like H2O. 
  *
  * @constructor create a new deconstructor with right and left parentheses.
  * @param leftParentChars A list of left parentheses, default = List('{', '[', '(')
  * @param rightParentChars  A list of right parentheses, default = List('}', ']', ')')
  */
class Deconstructor(val leftParentChars: List[Char] = List('{', '[', '('), 
    val rightParentChars: List[Char] = List('}', ']', ')')) {
  
  private var factor: Int = 1
  private var previousDigit: Option[Int] = None
  private var previousLetter: Option[Char] = None 
  private var factorBreadcrumbs: List[Int] = List[Int]()
  
  /**
   * This method returns a map of the atoms in a molecule string and their counts.
   * 
   * It takes a molecule string as input, for example "H2O". It parses this string 
   * from right to left and passes each character to the handleCharacter function. The result 
   * of the foldRight is a List containing each atom string for the amount of times it is found 
   * in the molecule, for example: List("H", "H", "O"). Finally we do a groupBy and map to turn
   * this list into a map of atoms to counts and return this result, e.g.: Map(H -> 2, O -> 1)
   * 
   * @param input The Molecule string
   */
  def parseMolecule(input: String): Map[String,Int] = {
    val elements: Array[String] = input.toCharArray().foldRight[Array[String]](Array[String]())((c, array) => handleCharacter(c, array))
    elements.groupBy(e => e).map(g => (g._1, g._2.length))
  } 
  
  
  /*
   * Call the relevant method depending on character type.
   */
  private def handleCharacter(c: Char, atomsArray: Array[String]): Array[String] = {
    if(c.isDigit) digit(c, atomsArray)
    else if(rightParentChars.contains(c)) rightParentheses(atomsArray)
    else if(leftParentChars.contains(c)) leftParentheses(atomsArray)
    else if(c.isLower) lowerCaseLetter(c, atomsArray)
    else if(c.isUpper) upperCaseLetter(c, atomsArray)
    else throw new IllegalArgumentException("The character '" + c + "' is not acceptable.")
  }
  
  
  /*
   * If the character is a digit then we multiply it against the current factor and store 
   * the new value. We also store the digit in case the next character is a right parentheses.
   */
  private def digit(c: Char, array: Array[String]): Array[String] = {
    factor = factor * c.asDigit
    previousDigit = Some(c.asDigit)
    array
  }
  
  
  /*
   * If the character is a right parentheses then we add the previous digit to the 
   * factorBreadcrumbs variable. This allows us to reduce the factor appropriately when
   * we encounter a left parentheses.
   */
  private def rightParentheses(array: Array[String]): Array[String] = {
    factorBreadcrumbs = factorBreadcrumbs :+ previousDigit.get
    previousDigit = None
    array
  }
  
  
  /*
   * If the character is a left parentheses then we need to reduce the factor variable by the 
   * appropriate amount. We do this by popping the last digit from the factorBreadcrumbs
   * List and dividing the factor variable by this value.
   */
  private def leftParentheses(array: Array[String]): Array[String] = {
    factor = factor/factorBreadcrumbs.takeRight(1)(0)
    factorBreadcrumbs = factorBreadcrumbs.take(factorBreadcrumbs.length - 1)
    array
  }
  
  
  /*
   * If the character is a lower case letter then we just store it as it will be need to be 
   * appended to the end of the next character to make a full atom. e.g. Fe
   */
  private def lowerCaseLetter(c: Char, array: Array[String]): Array[String] = {
    previousLetter = Some(c)
    array
  }
  
  
  /*
   * If the character is an upper case letter then we create a string for the atom 
   * name by appending the previous lower case letter if there was any to the current
   * upper case letter. We then create an array containing this atom name times the
   * factor value. If previousDigit is not none then the last character was a digit 
   * and because there was no right parentheses then the previous digit only applies 
   * to this atom and the factor should be reset to the value before the previous 
   * digit. 
   */
  private def upperCaseLetter(c: Char, array: Array[String]): Array[String] = {
    val atom: String = c.toString + previousLetter.getOrElse("")
    previousLetter = None
    var returnArray = array ++ Array.fill[String](factor)(atom)
    factor = factor/previousDigit.getOrElse(1)
    previousDigit = None
    returnArray
  }
  
}