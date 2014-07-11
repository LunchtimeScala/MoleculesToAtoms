package mark

/**
 * The result of tokenising a Molecule string
 * is a list of Tokens
 * @author mbuckley
 *
 */
sealed trait Token

case class Element(sym: String) extends Token

case class Multiplier(i: Int) extends Token

case class OpenBracket(sym: String) extends Token

case class CloseBracket(sym: String) extends Token


object Token  {
  def char2Token(c:Char) = {
    if (c.isDigit) Some(new Multiplier(c.toString.toInt))
    else if (c.isLetter) Some(new Element(c.toString))
    else if (c == '(') Some(new OpenBracket(c.toString))
    else if (c == ')') Some(new CloseBracket(c.toString))
    else None
  }
}