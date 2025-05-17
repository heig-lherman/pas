package Chat

import Chat.Token.*
import Data.ProductService.AvailableProducts

class TokenizerService(dictionary: Map[String, String]):
  import TokenizerService.*

  /** Separate the user's input into tokens
    * @param input
    *   the user's input
    * @return
    *   an iterator over the tokens of the input. The last token is always EOL.
    */
  def tokenize(input: String): Iterator[(String, Token)] = {
    val classify: String => Token = {
      case "bonjour"        => BONJOUR
      case "je"             => JE
      case "mon"            => MON
      case "le"             => LE
      case "de"             => DE
      case "combien"        => COMBIEN
      case "quel"           => QUEL
      case "etre"           => ETRE
      case "vouloir"        => VOULOIR
      case "couter"         => COUTER
      case "prix"           => PRIX
      case "commander"      => COMMANDER
      case "connaitre"      => CONNAITRE
      case "et"             => ET
      case "ou"             => OU
      case "solde"          => SOLDE
      case p if p.isProduct => PRODUIT
      case b if b.isBrand   => MARQUE
      case n if n.isInt     => NUM
      case _                => UNKNOWN
    }

    normalize(input).map(w => w -> classify(w)).withEol
  }

  /** Converts a given user input into a set of lexemes for further tokenization
    * @param input
    *   the user's input
    * @return
    *   a set of lexemes
    */
  private def normalize(input: String): Iterator[String] = input
    .filterNot(Punctuation.contains)
    .split("['\\s]+")
    .iterator
    .filter(_.nonEmpty)
    .map(_.toLowerCase)
    .map(word => dictionary.getOrElse(word, word))

end TokenizerService

private object TokenizerService:
  /** The punctuation marks that are removed from the user's input.
    */
  private val Punctuation = ".,;:!?*"

  // Some utilities for pattern matching above and generating outputs

  extension (iter: Iterator[(String, Token)])
    def withEol: Iterator[(String, Token)] =
      iter ++ Iterator.single(EOL.toString -> EOL)

  extension (str: String)
    private def isInt: Boolean = util.Try(str.toInt).isSuccess
    private def isProduct: Boolean = AvailableProducts.keySet.contains(str)
    private def isBrand: Boolean = AvailableProducts
      .flatMap(_._2.map(_._1))
      .exists(_ == str)
